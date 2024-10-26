import pygame
import sys
import requests
import urllib.parse


API_URL = 'http://localhost:8080'
SCREEN_WIDTH = 512
SCREEN_HEIGHT = 512
click_x, click_y = None, None
mouse_x, mouse_y = 0, 0
is_moving_piece = False
moving = None
target = None



# Initialize Pygame
pygame.init()


# Set up the display
screen = pygame.display.set_mode((SCREEN_HEIGHT, SCREEN_WIDTH))  # Width: 800, Height: 600
pygame.display.set_caption("Basic Pygame Window")


# Set up colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)

LIGHT = (210, 180, 140)
DARK = (139, 69, 19)

class Query:
    @staticmethod
    def encode_fen(fen):
        return urllib.parse.quote(fen, safe='')

    @staticmethod
    def possible_moves(fen, square):
        encoded_fen = Query.encode_fen(fen)
        url = f'{API_URL}/moves/{encoded_fen}/{square}'
        request = requests.get(url)
        request.raise_for_status()
        return request.json()

    @staticmethod
    def do_move(fen, moving, target):
        encoded_fen = Query.encode_fen(fen)
        url = f'{API_URL}/domove/{encoded_fen}/{moving}/{target}'
        request = requests.get(url)
        request.raise_for_status()
        return request.json()


class Piece:
    def __init__(self, char, row, col):
        self.image = pygame.image.load(f'./GUI/Pieces/{char}.png')
        self.image = pygame.transform.scale(self.image, (SCREEN_WIDTH // 8, SCREEN_HEIGHT // 8))
        self.char = char
        self.row = row
        self.col = col

    def draw(self):
        global is_moving_piece
        global moving

        click_row = click_y // (SCREEN_HEIGHT//8) if click_y != None else None
        click_col = click_x // (SCREEN_WIDTH//8) if click_x != None else None

        if self.row != click_row or self.col != click_col:
            y_screen = SCREEN_HEIGHT//8 * self.row
            x_screen = SCREEN_WIDTH//8 * self.col 
            screen.blit(self.image, (x_screen, y_screen))
        else:
            is_moving_piece = True
            col = mouse_x // (SCREEN_WIDTH//8)
            row = mouse_y // (SCREEN_HEIGHT//8)
            pos = (SCREEN_WIDTH//8 * col, SCREEN_HEIGHT//8 * row)
            screen.blit(self.image, pos)
            moving = ChessBoard.to_algebraic_position(self.row, self.col)

class ChessBoard:
    def __init__(self):
        #self.fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
        self.fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

    @staticmethod
    def to_algebraic_position(row, col):
        match row:
            case 0: arow = '8'
            case 1: arow = '7'
            case 2: arow = '6'
            case 3: arow = '5'
            case 4: arow = '4'
            case 5: arow = '3'
            case 6: arow = '2'
            case 7: arow = '1'

        match col:
            case 0: acol = 'a'
            case 1: acol = 'b'
            case 2: acol = 'c'
            case 3: acol = 'd'
            case 4: acol = 'e'
            case 5: acol = 'f'
            case 6: acol = 'g'
            case 7: acol = 'h'

        return acol + arow


    def draw(self):
        self.drawBoard()
        self.drawPieces()

    def drawBoard(self):
        for y in range(8):
            for x in range(8):
                sector_x = x * (SCREEN_WIDTH//8)
                sector_y = y * (SCREEN_HEIGHT//8)
                if (x + y) % 2:
                    color = DARK
                else:
                    color = LIGHT

                pygame.draw.rect(
                    screen, color,
                    (sector_x, sector_y,SCREEN_WIDTH//8, SCREEN_HEIGHT//8)
                )

    def drawPieces(self):
        split_fen = self.fen.split(' ')[0].split('/')

        pieces = []

        for row in range(len(split_fen)):
            acc = 0

            for col in range(len(split_fen[row])):
                char = split_fen[row][col]

                if char.isdigit():
                    acc += int(char) - 1
                else:
                    pieces.append(Piece(char, row, col + acc))


        for piece in pieces:
            piece.draw()
        
if __name__ == '__main__':
    running = True
    board = ChessBoard()

    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                click_x, click_y = pygame.mouse.get_pos()

            if event.type == pygame.MOUSEBUTTONUP and event.button == 1:
                click_x, click_y = None, None

                if is_moving_piece:
                    row = mouse_y // (SCREEN_HEIGHT//8)
                    col = mouse_x // (SCREEN_WIDTH//8)
                    target = ChessBoard.to_algebraic_position(row, col)

                    print(moving)
                    print(target)

                    new_fen = Query.do_move(board.fen, moving, target)
                    print(new_fen)
                    if new_fen != None:
                        board.fen = new_fen[0]

                    is_moving_piece = False
                    

        mouse_x, mouse_y = pygame.mouse.get_pos()

        board.draw()

        # Update the display
        pygame.display.flip()

    pygame.quit()
    sys.exit()
