{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use lambda-case" #-}

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
    "rRb_kbnr",
    "ppppPppp",
    "________",
    "________",
    "____q___",
    "__n_____",
    "PPPPPPPP",
    "_NBQKBNR"])


-- {- MAIN FUNCTIONS -}
main :: IO State
main = playGame starting_state

playGame :: State -> IO State
playGame (State board flags player) = do
    printBoard board
    putStrLn (show player ++ "'s turn")
    putStrLn "Please enter a move (for example '1234' for 'move from 1 2 to 3 4') or 'q' to quit."
    line <- getLine
    if line == "q" then
        return (State board flags player)
    else
        let maybeMove = readMove board line in
            case maybeMove of
                Just move -> 
                    if isValidMove move (State board flags player) then 
                        playGame (play move (State board flags player))
                    else do
                        putStrLn "illegal move!"
                        playGame (State board flags player)
                Nothing -> do
                    putStrLn "invalid move!"
                    playGame (State board flags player)


    

-- -- plays a given move
-- play :: Move -> State -> State
play :: Move -> State -> State
play (Move (Tile i1 j1 p1) (Tile i2 j2 _)) (State board flags player) = State 
    [[ if i == i1 && j == j1 then 
        Tile i j Empty
      else if i == i2 && j == j2 then
        Tile i j p1
      else Tile i j p | (Tile i j p) <- rows ] | rows <- board] flags (oppPlayer player)


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

-- checks whether a move is valid
-- valid if
--   1. move does not result in your own king being checked
--   2. move does not land on an unreachable square
isValidMove :: Move -> State -> Bool
isValidMove (Move (Tile _ _ Empty) _) _ = False
isValidMove (Move from to) (State board flags thisPlayer) = 
        player == thisPlayer &&
        not (isCheck thisPlayer next_board) && 
        to `elem` reachable_tiles
    where
        (State next_board _ _) = play (Move from to) (State board flags thisPlayer)
        (Tile _ _ (Piece name player)) = from
        reachable_tiles = case name of
            Pawn -> tilesPawn board from
            Rook -> tilesRook board from
            Knight -> tilesKnight board from
            Bishop -> tilesBishop board from
            Queen -> tilesQueen board from
            King -> tilesKing board from


-- checks whether a player is in check
isCheck :: Player -> Board -> Bool
isCheck player board =
        any (\(Tile _ _ p) -> p == Piece Knight otherPlayer) 
            (tilesKnight board kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Bishop otherPlayer) 
            (tilesBishop board kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Rook otherPlayer)
            (tilesRook board kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Queen otherPlayer)
            (tilesQueen board kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Pawn otherPlayer) 
            (tilesPawnTake board kingTile) ||
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

tilesPawn :: Board -> Tile -> [Tile]
tilesPawn board tile = tilesPawnMove board tile ++ tilesPawnTake board tile

-- gets tiles capturable by a pawn
tilesPawnTake :: Board -> Tile -> [Tile]
tilesPawnTake board (Tile i j (Piece _ player)) = filter 
    (\tile -> case tile of Tile _ _ (Piece _ otherPlayer) -> True; _ -> False) [
        tileAt board (i+r) (j+1), tileAt board (i+r) (j-1)
    ] where
        r = if player == White then 1 else -1
        otherPlayer = oppPlayer player

tilesPawnMove :: Board -> Tile -> [Tile]
tilesPawnMove board (Tile i j (Piece _ player)) =  
    case (i, player) of
        (1, White) -> 
            case (tileAt board 2 j, tileAt board 3 j) of
                (Tile 2 _ Empty, Tile 3 _ Empty) -> [Tile 2 j Empty, Tile 3 j Empty]
                (Tile 2 _ Empty, _) -> [Tile 2 j Empty]
                _ -> []
        (6, Black) ->
            case (tileAt board 5 j, tileAt board 4 j) of
                (Tile 5 _ Empty, Tile 4 _ Empty) -> [Tile 5 j Empty, Tile 4 j Empty]
                (Tile 5 _ Empty, _) -> [Tile 5 j Empty]
                _ -> []
        (_, White) -> if tileAt board (i+1) j == Tile (i+1) j Empty
            then [Tile (i+1) j Empty] else []
        (_, Black) -> if tileAt board (i-1) j == Tile (i-1) j Empty
            then [Tile (i-1) j Empty] else []
                
                

-- gets tiles potentially reachable by a king
tilesKing :: Board -> Tile -> [Tile]
tilesKing board (Tile i j (Piece _ player)) = filter (isReachable player) [
    tileAt board (i+1) j, tileAt board (i+1) (j+1),
    tileAt board i (j+1), tileAt board (i-1) (j+1),
    tileAt board (i-1) j, tileAt board (i-1) (j-1),
    tileAt board i (j-1), tileAt board (i+1) (j-1)]

tilesQueen :: Board -> Tile -> [Tile]
tilesQueen board tile = concat [tilesFromPiece board tile dir | dir <- [NO, NE, EA, SE, SO, SW, WE, NW]]

tilesBishop :: Board -> Tile -> [Tile]
tilesBishop board tile = concat [tilesFromPiece board tile dir | dir <- [NE, SE, SW, NW]]

tilesRook :: Board -> Tile -> [Tile]
tilesRook board tile = concat [tilesFromPiece board tile dir | dir <- [NO, SO, EA, WE]]

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

