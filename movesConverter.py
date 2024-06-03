# We import necessary libraries 
import pandas as pd  # We use pandas to manipulate the .csv
import chess  # We use pychess to operate the conversion from PGN to coordinates format for chess moves

# Create a pandas dataframe from the games archive on games.csv 
games = pd.read_csv('./games.csv')

# Drop every non-interesting column from the dataframe
games = games.drop(columns=['id', 'rated', 'created_at', 'last_move_at', 'turns', 'increment_code', 'white_id', 'white_rating', 'black_id', 'black_rating', 'opening_eco', 'opening_ply'])

# Function for moves conversion
def conversion(moves):
    board = chess.Board()  # First we create a plain board 
    res = ""  # Create an empty string for the result

    for move in moves.split():  # For loop on every move in the PGN string 
        coordmove = str(board.parse_san(move))  # Extract move coordinates
        coord = coordmove[:2] + "," + coordmove[2:]  # Split the coordinates to separate starting and final square
        board.push_san(move)  # Update board
        res += coord + "\n"  # Append each move to the result
    
    return res

# Add a column processed_moves to games
games['processed_moves'] = None

# Apply the conversion function to each row in the 'moves' column and store the result in the 'processed_moves' column
games['processed_moves'] = games['moves'].apply(conversion)

# Modify index to start from 1
games.index = range(1, len(games) + 1)

# Create a .csv named 'gamesProcessed.csv' with the processed data, add a column named 'index' to keep indices to the left
games.to_csv('gamesProcessed.csv', index_label='index')

# Print a message to confirm the conversion was successful
print("Conversion completed successfully!")