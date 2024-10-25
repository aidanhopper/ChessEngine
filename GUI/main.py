import pygame
import sys


SCREEN_WIDTH = 512
SCREEN_HEIGHT = 512


startingPosition = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'


# Initialize Pygame
pygame.init()


# Set up the display
screen = pygame.display.set_mode((SCREEN_HEIGHT, SCREEN_WIDTH))  # Width: 800, Height: 600
pygame.display.set_caption("Basic Pygame Window")


# Set up colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)


class Piece:
    def __init__(self, char, row, col):
        self.image = pygame.image.load(f'./GUI/Pieces/{char}.png')
        self.char = char
        self.row = row
        self.col = col

    def draw(self):
        pass


class ChessBoard:
    def __init__(self):
        self.fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'

    def draw(self):
        self.drawBoard()
        self.drawPieces()

    def drawBoard(self):
        for y in range(SCREEN_HEIGHT):
            for x in range(SCREEN_WIDTH):
                sector_x = x // (SCREEN_WIDTH//8)
                sector_y = y // (SCREEN_HEIGHT//8)
                if (sector_x % 2 + sector_y % 2) % 2 == 0:
                    screen.set_at((x, y), BLACK)
                else:
                    screen.set_at((x, y), WHITE)

    def drawPieces(self):
        split_fen = self.fen.split(' ')[0].split('/')

        pieces = []

        for row in range(len(split_fen)):
            acc = 0

            for col in range(len(split_fen[row])):
                char = split_fen[row][col]
                
                if char.isdigit():
                    acc + int(char)

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

        # Fill the screen with white
        screen.fill(BLACK)

        board.draw()

        # Update the display
        pygame.display.flip()

    pygame.quit()
    sys.exit()
