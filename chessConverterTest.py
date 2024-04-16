import chess 
import pandas as pd

board = chess.Board()
san_mainline = "d4 Nc6 e4 e5 f4 f6 dxe5 fxe5"

for move in san_mainline.split():
    coordmove = str(board.parse_san(move))
    board.push_san(move)
    coord = [coordmove[:2], coordmove[2:]]
    print(coord)