import pygame
import sys
import requests
import urllib.parse
import json


API_URL = 'http://localhost:3000'
SCREEN_WIDTH = 800
SCREEN_HEIGHT = 800
click_x, click_y = None, None
mouse_x, mouse_y = 0, 0
is_moving_piece = False
moving = None
target = None


# Initialize Pygame
pygame.init()


# Set up the display
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))  # Width: 800, Height: 600
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
    def possible_moves(fen):
        encoded_fen = Query.encode_fen(fen)
        url = f'{API_URL}/possible-moves?fen={encoded_fen}'
        request = requests.get(url)
        request.raise_for_status()
        return request.json()

    @staticmethod
    def make_move(fen, moving, target):
        encoded_fen = Query.encode_fen(fen)
        url = f'{API_URL}/make-move?fen={encoded_fen}&start={moving}&target={target}'
        request = requests.get(url)
        request.raise_for_status()
        return request.json()
    
    def best_move(fen):
        encoded_fen = Query.encode_fen(fen)
        url = f'{API_URL}/bestmove/{encoded_fen}'
        request = requests.get(url)
        request.raise_for_status()
        return request.json()

class Piece:
    def __init__(self, char, row, col):
        image_name = f'{char.lower()}w' if char.isupper() else f'{char}b' 
        self.image = pygame.image.load(f'./GUI/Pieces/{image_name}.png')
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
        self.fen = 'r4b1r/8/8/6Pp/4n3/8/PPPPPP1P/4K3 b KQkq - 0 1'
        self.possible_moves = None

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

    @staticmethod
    def to_row_and_col_position(algebraic_position):

        arow = algebraic_position[1]
        acol = algebraic_position[0]

        match arow:
            case '8': row = 0
            case '7': row = 1
            case '6': row = 2
            case '5': row = 3
            case '4': row = 4
            case '3': row = 5
            case '2': row = 6
            case '1': row = 7

        match acol:
            case 'a': col = 0
            case 'b': col = 1
            case 'c': col = 2
            case 'd': col = 3
            case 'e': col = 4
            case 'f': col = 5
            case 'g': col = 6
            case 'h': col = 7

        return row, col

    def draw(self):
        self.draw_board()
        self.draw_possible_moves()
        self.draw_pieces()

    def draw_board(self):
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

    def draw_pieces(self):
        global is_moving_piece
        global moving

        split_fen = self.fen.split(' ')[0].split('/')
        pieces = []
        move_elem = None

        if is_moving_piece:
            moving_row, moving_col = ChessBoard.to_row_and_col_position(moving)

        for row in range(len(split_fen)):
            acc = 0

            for col in range(len(split_fen[row])):
                char = split_fen[row][col]

                if char.isdigit():
                    acc += int(char) - 1
                else:
                    pieces.append(Piece(char, row, col + acc))

                if is_moving_piece and moving_row == row and moving_col == col + acc:
                    move_elem = len(pieces) - 1

        # swap the moving elem and the last elem to draw correctly
        if move_elem is not None:
            tmp = pieces[move_elem]
            pieces[move_elem] = pieces[-1]
            pieces[-1] = tmp

        for piece in pieces:
            piece.draw()

    def draw_possible_moves(self):
        if is_moving_piece:
            if self.possible_moves == None:
                self.possible_moves = Query.possible_moves(self.fen)
                print(self.possible_moves)

            for move in self.possible_moves:
                if move['startingSquare'] == moving:
                    row, col = ChessBoard.to_row_and_col_position(move['targetSquare'])

                    screen_x = col * SCREEN_WIDTH // 8 + SCREEN_WIDTH//32
                    screen_y = row * SCREEN_HEIGHT // 8 + SCREEN_HEIGHT//32

                    pygame.draw.rect(
                        screen, (34, 139, 34),
                        (screen_x, screen_y,SCREEN_WIDTH//16, SCREEN_HEIGHT//16)
                    )
                
        
if __name__ == '__main__':
    running = True
    board = ChessBoard()
    out = Query.possible_moves(board.fen)
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
                    
                    if moving != target:
                        new_fen = Query.make_move(board.fen, moving, target)
                        print(new_fen)
                        board.fen = new_fen
                        board.possible_moves = None
                        # if new_fen != None:
                        #     board.fen = Query.best_move(new_fen[0])
                        #     board.possible_moves = None

                    is_moving_piece = False
                    

        mouse_x, mouse_y = pygame.mouse.get_pos()

        board.draw()

        # Update the display
        pygame.display.flip()

    pygame.quit()
    sys.exit()
