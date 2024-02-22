{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant lambda" #-}

import Data.Char ( intToDigit, toLower, isUpper, digitToInt, ord, chr )
import Data.List (intersperse)


{- DATA and TYPES -}

-- the game's state is its board state, additional flags, and the current 
-- move's player
data State = State Board [Flag] Player
    deriving (Eq)

type Board = [[Tile]]

-- a move moves a piece from one tile to another tile
data Move = Move Tile Tile
    deriving (Eq, Show)

-- Points to 8 other tiles and may have a piece on it
data Tile = Tile Int Int Piece | OutOfBoard
    deriving (Eq)

-- 8 cardinal directions
data Direction = NO | NE | EA | SE | SO | SW | WE | NW
    deriving (Eq, Show)

-- players. 
data Player = White | Black
    deriving (Eq, Show)

-- constructor takes in the player the piece belongs to
data Piece = Piece PieceName Player | Empty
    deriving (Eq, Show)

data PieceName = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Eq, Show)

-- additional flags. Not sure how we represent these yet.
data Flag = Castled | EnPassant | FiftyMoves
    deriving (Eq, Show)

{- CONSTANTS -}

turn_order = [White, Black]
starting_board = strsToBoard (reverse [
    "rnbqkbnr",
    "pppppppp",
    "________",
    "________",
    "________",
    "________",
    "PPPPPPPP",
    "RNBQKBNR"])
starting_state = State starting_board [] White

test_board = strsToBoard (reverse [
    "rnb_kbnr",
    "ppppqppp",
    "________",
    "________",
    "________",
    "________",
    "PPPP_PPP",
    "RNBQKBNR"])


-- {- MAIN FUNCTIONS -}

-- -- plays a given move
-- play :: Move -> State -> State


-- reads move from string
-- string should be in the form "0143" for "move piece in tile 0 1 to tile 4 3"
readMove :: Board -> String -> Maybe Move
readMove board [c1,c2,c3,c4] =
    if all (\c -> '0' <= c && c <= '7') [c1,c2,c3,c4] then
        Just (Move
                (tileAt board (digitToInt c1) (digitToInt c2))
                (tileAt board (digitToInt c3) (digitToInt c4)))
    else Nothing
readMove _ _ = Nothing

-- -- checks whether a move is valid
-- -- valid if
-- --   1. move does not result in your own king being checked
-- --   2. move does not land on an unreachable square
-- --   3. move does not land on one of your own pieces
-- isValidMove :: Move -> State -> Bool

-- checks whether a player is in check
isCheck :: Player -> Board -> Bool
isCheck player board =
        any (\(Tile _ _ p) -> p == Piece Knight otherPlayer) 
            (tilesKnight board kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Bishop otherPlayer) 
            (concat [tilesFromPiece board kingTile dir | dir <- [NE, SE, SW, NW]]) ||
        any (\(Tile _ _ p) -> p == Piece Rook otherPlayer)
            (concat [tilesFromPiece board kingTile dir | dir <- [NO, EA, SO, WE]]) ||
        any (\(Tile _ _ p) -> p == Piece Queen otherPlayer)
            (concat [tilesFromPiece board kingTile dir | dir <- [NO, NE, EA, SE, SO, SW, WE, NW]]) ||
        any (\(Tile _ _ p) -> p == Piece Pawn otherPlayer) 
            (tilesPawn board kingTile) ||
        any (\(Tile _ _ p) -> p == Piece King otherPlayer) 
            (tilesKing board kingTile)
    where
        kingTile = head (filter (\(Tile _ _ p) -> p == Piece King player) (concat board))
        (Tile i j _) = kingTile
        otherPlayer = oppPlayer player 


-- checks whether a player has been checkmated
-- isMate :: Player -> Board -> Bool


{- HELPER FUNCTIONS -}

-- gets the opposite player
oppPlayer :: Player -> Player
oppPlayer White = Black
oppPlayer Black = White

-- if that tile is reachable by a player next turn
isReachable :: Player -> Tile -> Bool
isReachable _ OutOfBoard = False
isReachable _ (Tile _ _ Empty) = True
isReachable player (Tile _ _ (Piece _ otherPlayer)) = otherPlayer /= player

-- gets tile at coord
tileAt :: Board -> Int -> Int -> Tile
tileAt board i j = if 0 <= i && i <= 7 && 0 <= j && j <= 7
    then board!!i!!j else OutOfBoard

-- gets tiles potentially reachable by a knight
tilesKnight :: Board -> Tile -> [Tile]
tilesKnight board (Tile i j (Piece _ player)) = filter (isReachable player) [
    tileAt board (i+2) (j+1), tileAt board (i+1) (j+2),
    tileAt board (i-1) (j+2), tileAt board (i-2) (j+1),
    tileAt board (i-2) (j-1), tileAt board (i-1) (j-2),
    tileAt board (i+1) (j-2), tileAt board (i+2) (j-1)]

-- gets tiles potentially reachable by a pawn
tilesPawn :: Board -> Tile -> [Tile]
tilesPawn board (Tile i j (Piece _ player)) = filter (isReachable player) [
        tileAt board (i+r) (j+1), tileAt board (i+r) (j-1)
    ] where r = if player == White then 1 else -1

-- gets tiles potentially reachable by a king
tilesKing :: Board -> Tile -> [Tile]
tilesKing board (Tile i j (Piece _ player)) = filter (isReachable player) [
    tileAt board (i+1) j, tileAt board (i+1) (j+1),
    tileAt board i (j+1), tileAt board (i-1) (j+1),
    tileAt board (i-1) j, tileAt board (i-1) (j-1),
    tileAt board i (j-1), tileAt board (i+1) (j-1)]

-- 
tilesFromPiece :: Board -> Tile -> Direction -> [Tile]
tilesFromPiece board tile dir =
    case tile of
        Tile _ _ (Piece _ player) -> filter (isReachable player) (tilesAlong board tile dir)
        _ -> []


-- gets tiles in "line of sight" along some direction from a starting tile
tilesAlong :: Board -> Tile -> Direction -> [Tile]
tilesAlong board (Tile i j _) dir =
    case next_tile of
        Tile _ _ Empty -> next_tile : tilesAlong board next_tile dir
        OutOfBoard -> []
        _ -> [next_tile]
    where 
        next_tile = case dir of
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
    'p' -> Piece Pawn player
    'r' -> Piece Rook player
    'n' -> Piece Knight player
    'b' -> Piece Bishop player
    'q' -> Piece Queen player
    'k' -> Piece King player
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
printBoard board = putStrLn $ concat [intToDigit i : ' ' : intersperse ' ' (printTile <$> board!!i) ++ "\n" | i <- [7,6..0]] ++ "/ 0 1 2 3 4 5 6 7"

-- prints out a tile
printTile :: Tile -> Char
printTile OutOfBoard = 'X'
printTile (Tile _ _ Empty) = '☐'
printTile (Tile _ _ (Piece name player)) = chr ((if player == Black then 6 else 0) + ord (
    case name of
        Pawn -> '♙'
        Rook -> '♖'
        Knight -> '♘'
        Bishop -> '♗'
        Queen -> '♕'
        King -> '♔'))

