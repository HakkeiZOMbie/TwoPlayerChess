{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use lambda-case" #-}

import Data.Char ( intToDigit, toLower, isUpper, digitToInt, ord, chr )
import Data.List ( intersperse )
import Control.Monad ( when )


{- DATA and TYPES -}

-- the game's state is its board state, additional flags, and the current 
-- move's player
data State = State Board Flags Player
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
data Flags = Flags { 
    white_castled :: Bool,
    black_castled :: Bool,
    en_passant :: Int,
    fiftyMoves :: Int 
    } deriving (Eq, Show)

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
starting_state = State starting_board (Flags True True (-1) 0) White

test_board = strsToBoard (reverse [
    "r___k__r",
    "ppppPppp",
    "________",
    "________",
    "____q___",
    "__n_____",
    "PPPPPPPP",
    "_NBQKBNR"])
test_state = State test_board (Flags True True (-1) 0) Black

checkmate_board = strsToBoard (reverse [
    "rnbqkbnr",
    "pppppppp",
    "________",
    "________",
    "_______q",
    "________",
    "PPPPP__P",
    "RNBQKB_R"])

checkmate_state = State checkmate_board (Flags True True (-1) 0) White

stalemate_board = strsToBoard (reverse [
    "k_______",
    "_______R",
    "________",
    "________",
    "________",
    "________",
    "_R______",
    "K_______"])

stalemate_state = State stalemate_board (Flags False False (-1) 0) White


-- {- MAIN FUNCTIONS -}
main :: IO State
main = playGame starting_state

playGame :: State -> IO State
playGame state = do
    printBoard board
    if isCheckmate state then do
            putStrLn ("Checkmate! " ++ show (oppPlayer player) ++ " wins")
            return state
    else 
        if isStalemate state then do
            putStrLn ("Stalemate...")
            return state
        else do
            when (isCheck player state) (putStrLn "You are in check!")
            putStrLn (show player ++ "'s turn")
            putStrLn "Please enter a move (for example '1234' for 'move from rank 1 file 2 to rank 3 file 4') or 'q' to quit."
            line <- getLine
            if line == "q" then do
                putStrLn "quitting..."
                return state
            else
                let maybeMove = readMove board line in
                    case maybeMove of
                        Just move -> 
                            if isValidMove move state then 
                                playGame (play move state)
                            else do
                                putStrLn "Illegal move!"
                                playGame state
                        Nothing -> do
                            putStrLn "That's not a move!"
                            playGame state
    where (State board flags player) = state


    

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
isValidMove (Move from to) state = 
        player == thisPlayer &&
        not (isCheck thisPlayer next_state) && 
        to `elem` reachable_tiles
    where
        next_state = play (Move from to) state
        (Tile _ _ (Piece _ player)) = from
        (State board flags thisPlayer) = state
        reachable_tiles = tilesPiece state from

-- tiles reachable by the piece (if any) on the tile
tilesPiece :: State -> Tile -> [Tile]
tilesPiece _ (Tile _ _ Empty) = []
tilesPiece state tile =
    case name of
            Pawn -> tilesPawn state tile
            Rook -> tilesRook state tile
            Knight -> tilesKnight state tile
            Bishop -> tilesBishop state tile
            Queen -> tilesQueen state tile
            King -> tilesKing state tile
    where 
        (Tile _ _ (Piece name _)) = tile
        (State board flags player) = state


-- checks whether a player is in check
isCheck :: Player -> State -> Bool
isCheck player state =
        any (\(Tile _ _ p) -> p == Piece Knight otherPlayer) 
            (tilesKnight state kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Bishop otherPlayer) 
            (tilesBishop state kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Rook otherPlayer)
            (tilesRook state kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Queen otherPlayer)
            (tilesQueen state kingTile) ||
        any (\(Tile _ _ p) -> p == Piece Pawn otherPlayer) 
            (tilesPawnTake state kingTile) ||
        any (\(Tile _ _ p) -> p == Piece King otherPlayer) 
            (tilesKingBase state kingTile)
    where
        kingTile = head (filter (\(Tile _ _ p) -> p == Piece King player) (concat board))
        (State board _ _) = state
        (Tile i j _) = kingTile
        otherPlayer = oppPlayer player 

-- checks whether a player has been checkmated
isCheckmate :: State -> Bool
isCheckmate state = isCheck player state && all (\move -> not (isValidMove move state)) moves
    where 
        (State board _ player) = state
        moves = concat [
            Move (Tile i j (Piece n p)) <$> tilesPiece state (Tile i j (Piece n p)) 
            | Tile i j (Piece n p) <- concat board,
            p == player]

-- checks whether the game is a stalemate
isStalemate :: State -> Bool
isStalemate state = not (isCheck player state) && all (\move -> not (isValidMove move state)) moves
    where 
        (State board _ player) = state
        moves = concat [
            Move (Tile i j (Piece n p)) <$> tilesPiece state (Tile i j (Piece n p)) 
            | Tile i j (Piece n p) <- concat board,
            p == player]


{- HELPER FUNCTIONS -}

-- gets the opposite player
oppPlayer :: Player -> Player
oppPlayer White = Black
oppPlayer Black = White

-- is an opposite color
isOppPiece :: Player -> Tile -> Bool
isOppPiece White (Tile _ _ (Piece _ Black)) = True
isOppPiece Black (Tile _ _ (Piece _ White)) = True
isOppPiece _ _ = False

-- if that tile is reachable by a player next turn
isReachable :: Player -> Tile -> Bool
isReachable _ OutOfBoard = False
isReachable _ (Tile _ _ Empty) = True
isReachable player tile = isOppPiece player tile

-- gets tile at coord
tileAt :: Board -> Int -> Int -> Tile
tileAt board i j = if 0 <= i && i <= 7 && 0 <= j && j <= 7
    then board!!i!!j else OutOfBoard

-- gets tiles potentially reachable by a knight
tilesKnight :: State -> Tile -> [Tile]
tilesKnight (State board _ _) (Tile i j (Piece _ player)) = filter (isReachable player) [
    tileAt board (i+2) (j+1), tileAt board (i+1) (j+2),
    tileAt board (i-1) (j+2), tileAt board (i-2) (j+1),
    tileAt board (i-2) (j-1), tileAt board (i-1) (j-2),
    tileAt board (i+1) (j-2), tileAt board (i+2) (j-1)]

tilesPawn :: State -> Tile -> [Tile]
tilesPawn state tile = tilesPawnMove state tile ++ tilesPawnTake state tile

-- gets tiles capturable by a pawn
tilesPawnTake :: State -> Tile -> [Tile]
tilesPawnTake (State board _ _) (Tile i j (Piece _ player)) = 
        filter (isOppPiece player) [tileAt board (i+r) (j+1), tileAt board (i+r) (j-1)] 
    where r = if player == White then 1 else -1

tilesPawnMove :: State -> Tile -> [Tile]
tilesPawnMove (State board _ _) (Tile i j (Piece _ player)) =  
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

-- gets tiles potentially reachable by a king (with castling)
tilesKing :: State -> Tile -> [Tile]
tilesKing state tile = tilesKingBase state tile ++ tilesCastle state
                
-- gets tiles potentially reachable by a king (no castling)
tilesKingBase :: State -> Tile -> [Tile]
tilesKingBase state (Tile i j (Piece _ player)) = filter (isReachable player) [
        tileAt board (i+1) j, tileAt board (i+1) (j+1),
        tileAt board i (j+1), tileAt board (i-1) (j+1),
        tileAt board (i-1) j, tileAt board (i-1) (j-1),
        tileAt board i (j-1), tileAt board (i+1) (j-1)]
    where (State board flags player) = state

tilesQueen :: State -> Tile -> [Tile]
tilesQueen (State board _ _) tile = concat [tilesAlong board tile dir | dir <- [NO, NE, EA, SE, SO, SW, WE, NW]]

tilesBishop :: State -> Tile -> [Tile]
tilesBishop (State board _ _) tile = concat [tilesAlong board tile dir | dir <- [NE, SE, SW, NW]]

tilesRook :: State -> Tile -> [Tile]
tilesRook (State board _ _) tile = concat [tilesAlong board tile dir | dir <- [NO, SO, EA, WE]]

-- gets tiles in "line of sight" along some direction from a starting tile
tilesAlong :: Board -> Tile -> Direction -> [Tile]
tilesAlong board tile dir =
    case tile of
        Tile _ _ (Piece _ player) -> filter (isReachable player) (tilesAlongHelper board tile dir)
        _ -> []

-- recursive helper function for tilesAlong
tilesAlongHelper :: Board -> Tile -> Direction -> [Tile]
tilesAlongHelper board (Tile i j _) dir =
    case next_tile of
        Tile _ _ Empty -> next_tile : tilesAlongHelper board next_tile dir
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

tilesCastle :: State -> [Tile]
tilesCastle state =
    if castle && not (isCheck player state) then
        (if take 5 ((\(Tile _ _ piece) -> piece) <$> board!!i) == [Piece Rook player, Empty, Empty, Empty, Piece King player] then
            [Tile i 2 (Piece King player)]
        else []) ++
        (if drop 4 ((\(Tile _ _ piece) -> piece) <$> board!!i) == [Piece King player, Empty, Empty, Piece Rook player] then
            [Tile i 6 (Piece King player)]
        else [])
    else [] where
        (State board flags player) = state
        castle = if player == White then white_castled flags else black_castled flags
        i = if player == White then 0 else 7


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

