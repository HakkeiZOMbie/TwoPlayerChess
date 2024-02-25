{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
data Move = Move MoveType Tile Tile
    deriving (Eq)

data MoveType = Normal | Castle | EnPassant
    deriving (Eq)

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
    whiteCastledKingSide :: Bool,
    whiteCastledQueenSide :: Bool,
    blackCastledKingSide :: Bool,
    blackCastledQueenSide :: Bool,
    en_passant :: Int,
    fifty_moves :: Int 
    } deriving (Eq, Show)

{- CONSTANTS -}

turnOrder = [White, Black]
startingBoard = strsToBoard (reverse [
    "rnbqkbnr",
    "pppppppp",
    "________",
    "________",
    "________",
    "________",
    "PPPPPPPP",
    "RNBQKBNR"])
startingState = State startingBoard (Flags False False False False (-1) 0) White

testBoard = strsToBoard (reverse [
    "r___k__r",
    "________",
    "________",
    "________",
    "________",
    "________",
    "________",
    "R___K__R"])
testState = State testBoard (Flags False False False False (-1) 0) Black

checkmateBoard = strsToBoard (reverse [
    "rnbqkbnr",
    "pppppppp",
    "________",
    "________",
    "_______q",
    "________",
    "PPPPP__P",
    "RNBQKB_R"])

checkmateState = State checkmateBoard (Flags False False False False (-1) 0) White

stalemateBoard = strsToBoard (reverse [
    "k_______",
    "_______R",
    "________",
    "________",
    "________",
    "________",
    "_R______",
    "K_______"])

stalemateState = State stalemateBoard (Flags False False False False (-1) 0) White


-- {- MAIN FUNCTIONS -}
main :: IO State
main = playGame startingState

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
            putStrLn "Alternatively query moves for a tile by entering ? then the tile coords. (e.g. ?32)"
            line <- getLine
            case line of
                "q" -> do
                    putStrLn "quitting..."
                    return state
                ['?',i,j] -> do
                    case readTile board [i,j] of
                        Just tile -> do
                            print (filter (\m -> isValidMove m state) (tileMoves state tile))
                        Nothing -> do
                            putStrLn "wrong format!"
                    playGame state
                _ -> 
                    case readMove board line of
                        Just move -> 
                            if isValidMove move state then 
                                playGame (play move state)
                            else do
                                putStrLn "Illegal move!"
                                playGame state
                        Nothing -> do
                            putStrLn "wrong format!"
                            playGame state
    where (State board flags player) = state


setTile :: Tile -> Board -> Board
setTile (Tile i1 j1 p1) board = 
    [[ if i == i1 && j == j1 then 
            Tile i j p1
        else Tile i j p | (Tile i j p) <- rows ] | rows <- board ]

-- plays a given move
play :: Move -> State -> State
play (Move Castle (Tile i _ (Piece King player)) (Tile _ j Empty)) state =
        State 
            ((setTile (Tile i j1 Empty) . setTile (Tile i j2 (Piece Rook player))) board) 
            flags nextPlayer
    where 
        State board flags nextPlayer = 
            play (Move Normal (Tile i 4 (Piece King player)) (Tile i j Empty)) state
        j1 = if j == 2 then 0 else 7
        j2 = if j == 2 then 3 else 5

play (Move Normal from to) (State board flags player) = State 
        ((setTile (Tile i1 j1 Empty) . setTile (Tile i2 j2 p1)) board)
        (case from of
            Tile _ _ (Piece King White) -> 
                flags { whiteCastledKingSide = True, whiteCastledQueenSide = True }
            Tile _ _ (Piece King Black) ->
                flags { blackCastledKingSide = True, blackCastledQueenSide = True }
            Tile 0 0 _ -> flags { whiteCastledQueenSide = True }
            Tile 0 7 _ -> flags { whiteCastledKingSide = True }
            Tile 7 0 _ -> flags { blackCastledQueenSide = True }
            Tile 7 7 _ -> flags { blackCastledKingSide = True }
            _ -> flags)
        (oppPlayer player)
    where
        (Tile i1 j1 p1) = from
        (Tile i2 j2 _) = to


-- reads move from string
-- string should be in the form "0143" for "move piece in tile 0 1 to tile 4 3"
readMove :: Board -> String -> Maybe Move
readMove board [c1,c2,c3,c4] =
    if all (\c -> '0' <= c && c <= '7') [c1,c2,c3,c4] then
        let 
            Tile i1 j1 p1 = tileAt board (digitToInt c1) (digitToInt c2)
            Tile i2 j2 p2 = tileAt board (digitToInt c3) (digitToInt c4)
        in case (p1, abs (j1-j2)) of 
            (Piece King _, 2) -> Just (Move Castle (Tile i1 j1 p1) (Tile i2 j2 p2))
            _ -> Just (Move Normal (Tile i1 j1 p1) (Tile i2 j2 p2))
    else Nothing
readMove _ _ = Nothing

-- reads tile from string
readTile :: Board -> String -> Maybe Tile
readTile board [c1,c2] = 
    if all (\c -> '0' <= c && c <= '7') [c1,c2] then
        Just (tileAt board (digitToInt c1) (digitToInt c2))
    else Nothing
readTile _ _ = Nothing


-- checks whether a move is valid
-- valid if
--   1. move does not result in your own king being checked
--   2. move does not land on an unreachable square
isValidMove :: Move -> State -> Bool
isValidMove (Move _ (Tile _ _ Empty) _) _ = False
isValidMove move state = 
        player == thisPlayer &&
        not (isCheck thisPlayer nextState) && 
        move `elem` moves
    where
        (Move _ from to) = move
        (Tile _ _ (Piece _ player)) = from
        (State board flags thisPlayer) = state
        nextState = play move state
        moves = tileMoves state from

-- tiles reachable by the piece (if any) on the tile
tileMoves :: State -> Tile -> [Move]
tileMoves _ (Tile _ _ Empty) = []
tileMoves state tile =
    case name of
            Pawn -> pawnMoves state tile
            Rook -> rookMoves state tile
            Knight -> knightMoves state tile
            Bishop -> bishopMoves state tile
            Queen -> queenMoves state tile
            King -> kingMoves state tile
    where 
        (Tile _ _ (Piece name _)) = tile
        (State board flags player) = state


-- checks whether a player is in check
isCheck :: Player -> State -> Bool
isCheck player state =
        any (\(Move _ _ (Tile _ _ p)) -> p == Piece Knight otherPlayer) 
            (knightMoves state kingTile) ||
        any (\(Move _ _ (Tile _ _ p)) -> p == Piece Bishop otherPlayer) 
            (bishopMoves state kingTile) ||
        any (\(Move _ _ (Tile _ _ p)) -> p == Piece Rook otherPlayer)
            (rookMoves state kingTile) ||
        any (\(Move _ _ (Tile _ _ p)) -> p == Piece Queen otherPlayer)
            (queenMoves state kingTile) ||
        any (\(Move _ _ (Tile _ _ p)) -> p == Piece Pawn otherPlayer) 
            (pawnMovesTake state kingTile) ||
        any (\(Move _ _ (Tile _ _ p)) -> p == Piece King otherPlayer) 
            (kingMovesBase state kingTile)
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
        moves = concat [tileMoves state (Tile i j (Piece n p)) 
            | Tile i j (Piece n p) <- concat board,
            p == player]

-- checks whether the game is a stalemate
isStalemate :: State -> Bool
isStalemate state = not (isCheck player state) && all (\move -> not (isValidMove move state)) moves
    where 
        (State board _ player) = state
        moves = concat [
            tileMoves state (Tile i j (Piece n p)) 
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
knightMoves :: State -> Tile -> [Move]
knightMoves (State board _ _) tile = Move Normal tile <$> filter (isReachable player) [
        tileAt board (i+2) (j+1), tileAt board (i+1) (j+2),
        tileAt board (i-1) (j+2), tileAt board (i-2) (j+1),
        tileAt board (i-2) (j-1), tileAt board (i-1) (j-2),
        tileAt board (i+1) (j-2), tileAt board (i+2) (j-1)]
    where 
        (Tile i j (Piece _ player)) = tile

pawnMoves :: State -> Tile -> [Move]
pawnMoves state tile = pawnMovesMove state tile ++ pawnMovesTake state tile

-- gets tiles capturable by a pawn
pawnMovesTake :: State -> Tile -> [Move]
pawnMovesTake (State board _ _) tile = Move Normal tile <$>
        filter (isOppPiece player) [tileAt board (i+r) (j+1), tileAt board (i+r) (j-1)] 
    where 
        (Tile i j (Piece _ player)) = tile
        r = if player == White then 1 else -1

pawnMovesMove :: State -> Tile -> [Move]
pawnMovesMove (State board _ _) tile = Move Normal tile <$>
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
    where
        (Tile i j (Piece _ player)) = tile

-- gets tiles potentially reachable by a king (with castling)
kingMoves :: State -> Tile -> [Move]
kingMoves state tile = kingMovesBase state tile ++ castleMoves state tile
                
-- gets tiles potentially reachable by a king (no castling)
kingMovesBase :: State -> Tile -> [Move]
kingMovesBase state tile = Move Normal tile <$> filter (isReachable player) [
        tileAt board (i+1) j, tileAt board (i+1) (j+1),
        tileAt board i (j+1), tileAt board (i-1) (j+1),
        tileAt board (i-1) j, tileAt board (i-1) (j-1),
        tileAt board i (j-1), tileAt board (i+1) (j-1)]
    where 
        (Tile i j (Piece _ player)) = tile
        (State board flags _) = state

queenMoves :: State -> Tile -> [Move]
queenMoves (State board _ _) tile = Move Normal tile <$> 
    concat [alongMoves board tile dir | dir <- [NO, NE, EA, SE, SO, SW, WE, NW]]

bishopMoves :: State -> Tile -> [Move]
bishopMoves (State board _ _) tile = Move Normal tile <$> 
    concat [alongMoves board tile dir | dir <- [NE, SE, SW, NW]]

rookMoves :: State -> Tile -> [Move]
rookMoves (State board _ _) tile = Move Normal tile <$>
    concat [alongMoves board tile dir | dir <- [NO, SO, EA, WE]]

-- gets tiles in "line of sight" along some direction from a starting tile
alongMoves :: Board -> Tile -> Direction -> [Tile]
alongMoves board tile dir =
    case tile of
        Tile _ _ (Piece _ player) -> filter (isReachable player) (alongMovesHelper board tile dir)
        _ -> []

-- recursive helper function for alongMoves
alongMovesHelper :: Board -> Tile -> Direction -> [Tile]
alongMovesHelper board (Tile i j _) dir =
    case nextTile of
        Tile _ _ Empty -> nextTile : alongMovesHelper board nextTile dir
        OutOfBoard -> []
        _ -> [nextTile]
    where 
        nextTile = case dir of
            NO -> tileAt board (i+1) j
            NE -> tileAt board (i+1) (j+1)
            EA -> tileAt board i (j+1)
            SE -> tileAt board (i-1) (j+1)
            SO -> tileAt board (i-1) j
            SW -> tileAt board (i-1) (j-1)
            WE -> tileAt board i (j-1)
            NW -> tileAt board (i+1) (j-1)

castleMoves :: State -> Tile -> [Move]
castleMoves state tile =
    let 
        (State board flags player) = state
        (castledQueenSide, castledKingSide) = case player of
            White -> (whiteCastledQueenSide flags, whiteCastledKingSide flags)
            Black -> (blackCastledQueenSide flags, blackCastledKingSide flags)
        i = case (tile, player, isCheck player state) of
            (Tile 7 4 (Piece King Black), Black, False) -> 7
            (Tile 0 4 (Piece King White), White, False) -> 0
            _ -> -1
    in 
        if i /= -1 then
            Move Castle tile <$> (
                (if take 5 ((\(Tile _ _ piece) -> piece) <$> board!!i) == [Piece Rook player, Empty, Empty, Empty, Piece King player] then
                    [Tile i 2 Empty]
                else []) ++
                (if drop 4 ((\(Tile _ _ piece) -> piece) <$> board!!i) == [Piece King player, Empty, Empty, Piece Rook player] then
                    [Tile i 6 Empty]
                else []))
        else []


{- UTILITIES -}
-- getting pieces from chars. Used in board initialization
chrToPiece :: Char -> Piece
chrToPiece char = case lowerChar of
    'p' -> Piece Pawn player
    'r' -> Piece Rook player
    'n' -> Piece Knight player
    'b' -> Piece Bishop player
    'q' -> Piece Queen player
    'k' -> Piece King player
    _ -> Empty
    where
        lowerChar = toLower char
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

instance Show Move where
    show :: Move -> String
    show (Move mtype from to) = show from ++ 
        case mtype of 
            Normal -> " --> "
            Castle -> " o-o "
            EnPassant -> " ep." 
        ++ show to

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

