{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.Char ( intToDigit, toLower, isUpper)


{- DATA and TYPES -}

-- the game's state is its board state, additional flags, and the current 
-- move's player
data State = State Board [Flag] Player
    deriving (Eq)

type Board = [[Tile]]

-- a move moves a piece from one tile to another tile
data Move = Move Tile Tile
    deriving (Eq)

-- Points to 8 other tiles and may have a piece on it
data Tile = Tile Int Int Piece | OutOfBoard
    deriving (Eq)

-- 8 cardinal directions
data Direction = NO | NE | EA | SE | SO | SW | WE | NW

-- players. 
-- Note that we can add other colors and the logic shouldn't change...
data Player = White | Black
    deriving (Eq, Show)

-- constructor takes in the player the piece belongs to
data Piece = Pawn Player | Rook Player | Knight Player 
    | Bishop Player | Queen Player | King Player 
    | Empty
    deriving (Eq, Show)

-- additional flags. Not sure how we represent these yet.
data Flag = Castled | EnPassant | FiftyMoves
    deriving (Eq, Show)

{- CONSTANTS -}

turn_order = [White, Black]
starting_board = strsToBoard [
    "rnbqkbnr",
    "pppppppp",
    "________",
    "________",
    "________",
    "________",
    "PPPPPPPP",
    "RNBQKBNR"]
starting_state = State starting_board [] White


-- {- MAIN FUNCTIONS -}

-- -- plays a given move
-- play :: Move -> State -> State

-- -- reads move from string
-- readMove :: String -> Maybe Move

-- -- checks whether a move is valid
-- isValidMove :: Move -> State -> Bool

-- -- checks whether a move results in player's king being checked
-- isCheck :: Move -> State -> Bool

-- -- checks whether a move is a checkmate
-- isMate :: Move -> State -> Bool


{- HELPER FUNCTIONS -}

-- gets tiles potentially reachable by a knight
tilesKnight :: Board -> Tile -> [Tile]
tilesKnight board (Tile i j _) = [
    tileAt board (i+2) (j+1), tileAt board (i+1) (j+2), 
    tileAt board (i-1) (j+2), tileAt board (i-2) (j+1), 
    tileAt board (i-2) (j-1), tileAt board (i-1) (j-2),
    tileAt board (i+1) (j-2), tileAt board (i+2) (j-1)]

-- gets tile at coord
tileAt :: Board -> Int -> Int -> Tile
tileAt board i j = if 0 <= i && i <= 7 && 0 <= j && j <= 7
    then board!!i!!j else OutOfBoard 

-- gets potentially reachable tiles that lie along 
-- some direction from a starting tile
tilesAlong :: Board -> Tile -> Direction -> [Tile]
tilesAlong board (Tile i j _) dir =
    case next_tile of
        Tile _ _ Empty -> next_tile : tilesAlong board next_tile dir
        _ -> [next_tile]
    where next_tile = case dir of
            NO -> tileAt board (i+1) j
            NE -> tileAt board (i+1) (j+1)
            EA -> tileAt board i (j+1)
            SE -> tileAt board (i-1) (j+1)
            SO -> tileAt board (i-1) j
            SW -> tileAt board (i-1) (j-1)
            WE -> tileAt board i (j-1)
            NW -> tileAt board (i+1) (j-1)


{- UTILITIES -}
-- getting pieces from chars. Used in board initialization
chrToPiece :: Char -> Piece
chrToPiece char = case lower_char of
    'p' -> Pawn player
    'r' -> Rook player
    'n' -> Knight player
    'b' -> Bishop player
    'q' -> Queen player
    'k' -> King player
    _ -> Empty
    where 
        lower_char = toLower char
        player = if isUpper char then White else Black

-- converts a list of strings to a board
strsToBoard :: [String] -> Board
strsToBoard str = [
    [Tile i j (chrToPiece piece) | (j,piece) <- zip [0..7] (str!!i)] | i <- [0..7]]

-- used show for tile
instance Show Tile where
    show :: Tile -> String
    show OutOfBoard = "x"
    show (Tile i j piece) = ['(', 
        printTile (Tile i j piece), ',', 
        intToDigit i, ',',  
        intToDigit j, ')']

-- prints out the board
printBoard :: Board -> IO ()
printBoard board = putStrLn $ concat [intToDigit i : (printTile <$> board!!i) ++ "\n" | i <- [7,6..0]] ++ "/01234567"

-- prints out a tile
printTile :: Tile -> Char
printTile OutOfBoard = 'X'
printTile (Tile _ _ piece) = case piece of
    Pawn Black -> '♟'
    Pawn White -> '♙'
    Rook Black -> '♜'
    Rook White -> '♖'
    Knight Black -> '♞'
    Knight White -> '♘'
    Bishop Black -> '♝'
    Bishop White -> '♗'
    Queen Black -> '♛'
    Queen White -> '♕'
    King Black -> '♚'
    King White -> '♔'
    Empty -> '☐'

    
