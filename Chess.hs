{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use list comprehension" #-}
module Chess where

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
    whiteCanKingSide :: Bool,
    whiteCanQueenSide :: Bool,
    blackCanKingSide :: Bool,
    blackCanQueenSide :: Bool,
    enPassant :: Int,
    fiftyMoves :: Int
    } deriving (Eq, Show)





{- CONSTANTS -}

startingBoard = strsToBoard (reverse [
    "rnbqkbnr",
    "pppppppp",
    "________",
    "________",
    "________",
    "________",
    "PPPPPPPP",
    "RNBQKBNR"])

startingState = State startingBoard (Flags True True True True (-1) 0) White


testBoard = strsToBoard (reverse [
    "____k___",
    "_P______",
    "________",
    "__pP____",
    "________",
    "________",
    "________",
    "____K___"])


testState = State testBoard (Flags True True True True 2 0) White





{- KEY FUNCTIONS -}

-- is it being attacked by player?
-- if any tile in x + * (v or ^) are enemy pieces
isAttacking :: Player -> Tile -> Board -> Bool
isAttacking player tile board =
        any (\(Tile _ _ piece) -> piece == Piece Knight player) (knightTiles board tile) ||
        any (\(Tile _ _ piece) -> piece == Piece Rook player) (rookTiles board tile) ||
        any (\(Tile _ _ piece) -> piece == Piece Bishop player) (bishopTiles board tile) ||
        any (\(Tile _ _ piece) -> piece == Piece Queen player) (queenTiles board tile) ||
        any (\(Tile _ _ piece) -> piece == Piece King player) (kingTiles board tile) ||
        any (\(Tile _ _ piece) -> piece == Piece Pawn player)
            (pawnTakeTiles board (Tile i j (Piece Pawn (oppPlayer player))))
    where (Tile i j _) = tile

-- gets characteristic tiles for a piece - base tiles only.
-- for pawns, only attack tiles, and also based on its color
pieceTiles :: Board -> Tile -> [Tile]
pieceTiles board tile = case ptype of
        Pawn -> pawnTakeTiles board tile
        Rook -> rookTiles board tile
        Knight -> knightTiles board tile
        Bishop -> bishopTiles board tile
        Queen -> queenTiles board tile
        King -> kingTiles board tile
    where (Tile i j (Piece ptype owner)) = tile

-- gets moves for a tile
--  - gets possible moves from a tile
--  - also handles special cases; i.e. castling and en passant hence the need for state
--  - naturally, for every piece except pawns and kings this is their pieceTiles
pieceMoves :: State -> Tile -> [Move]
-- empty tile have no moves
pieceMoves _ (Tile _ _ Empty) = []
-- king moves
pieceMoves state (Tile i j (Piece King owner)) =
    if player /= owner then [] else
        (Move Normal tile <$>
            filter
                (\(Tile _ _ p) -> isOppColor p (Piece King owner) || p == Empty)
                (pieceTiles board tile))
        ++ (Move Castle tile <$> castleTiles state tile)
    where
        (State board flag player) = state
        tile = Tile i j (Piece King owner)
-- pawn moves
pieceMoves state (Tile i j (Piece Pawn owner)) =
    if player /= owner then [] else
        (Move Normal tile <$>
            (filter
                (\(Tile _ _ p) -> isOppColor p (Piece Pawn owner)) (pieceTiles board tile)
            ++ pawnMoveTiles board tile))
        ++ (Move EnPassant tile <$> enPassantTiles state tile)
    where
        (State board flag player) = state
        tile = Tile i j (Piece Pawn owner)
-- everything else
pieceMoves (State board _ player) tile =
    if player /= owner then [] else
        Move Normal tile <$>
        filter
                (\(Tile _ _ p) -> isOppColor p (Piece King owner) || p == Empty)
                (pieceTiles board tile)
    where (Tile _ _ (Piece _ owner)) = tile

-- is player in check?
isChecked :: Player -> Board -> Bool
isChecked player board =
        (oppPlayer player `isAttacking` kingTile) board
    where
        kingTile = head (
            dropWhile (\(Tile _ _ piece) -> piece /= Piece King player) (concat board))

-- checks whether state is a checkmate
isCheckmate :: State -> Bool
isCheckmate state =
    player `isChecked` board && all (\move -> not (isValid move state)) moves
    where
        (State board _ player) = state
        moves = concat [pieceMoves state (Tile i j (Piece ptype owner))
            | Tile i j (Piece ptype owner) <- concat board,
            owner == player]

-- checks whether state is a stalemate
isStalemate :: State -> Bool
isStalemate state =
    not (player `isChecked` board) && all (\move -> not (isValid move state)) moves
    where
        (State board _ player) = state
        moves = concat [pieceMoves state (Tile i j (Piece ptype owner))
            | Tile i j (Piece ptype owner) <- concat board,
            owner == player]

-- plays a move.
-- - Has 3 types: normal, castle, and enpassant
play :: Move -> State -> State
play (Move Normal (Tile i j p) (Tile i' j' _)) (State board flags player) = State
    (setTile (Tile i' j' p) (setTile (Tile i j Empty) board))
    (case (i, j, i', j', p) of
        (0,0,_,_,_) -> flags { whiteCanQueenSide = False }
        (0,7,_,_,_) -> flags { whiteCanKingSide = False }
        (7,0,_,_,_) -> flags { blackCanQueenSide = False }
        (7,7,_,_,_) -> flags { blackCanKingSide = False }
        (_,_,_,_,Piece King White) ->
            flags { whiteCanQueenSide = False, whiteCanKingSide = False }
        (_,_,_,_,Piece King Black) ->
            flags { blackCanQueenSide = False, blackCanKingSide = False }
        (1,_,3,_,Piece Pawn White) ->
            flags { enPassant = j }
        (6,_,4,_,Piece Pawn Black) ->
            flags { enPassant = j }
        _ -> flags { enPassant = -1 })
    (oppPlayer player)

play (Move Castle (Tile i j p) (Tile i' j' _)) state =
        play (Move Normal (Tile i j p) (Tile i' j' Empty)) (State board' flags player)
    where
        board' = setTile (Tile i j1 Empty) (setTile (Tile i j2 (Piece Rook player)) board)
        j1 = if j' == 2 then 0 else 7
        j2 = if j' == 2 then 3 else 5
        State board flags player = state

play (Move EnPassant (Tile i j p) (Tile i' j' _)) state =
        play (Move Normal (Tile i j p) (Tile i' j' Empty)) (State board' flags player)
    where
        board' = setTile (Tile i j' Empty) board
        State board flags player = state


-- is a move valid?
-- move is valid if
-- - it does not result in the playing player's king in check
-- - it does not land on a non-target tile
-- - it is a move performable by the piece
isValid :: Move -> State -> Bool
isValid move state =
    not (player `isChecked` board') && any (\(Move _ t1 t2) -> t1 == from && t2 == to) (pieceMoves state from)
    where
        (Move _ from to) = move
        (State board' _ _) = play move state
        (State _ _ player) = state

-- corrects the move type
idMove :: Move -> State -> Move
idMove (Move _ from to) state = head 
    (filter (\(Move _ t1 t2) -> t1 == from && t2 == to) (pieceMoves state from))


isPromotion :: Move -> Bool
isPromotion (Move _ (Tile _ _ (Piece Pawn player)) (Tile i' _ _)) =
    player == White && i' == 7 || player == Black && i' == 0
isPromotion _ = False



{- HELPERS -}

-- gets tile at coord
tileAt :: Board -> Int -> Int -> Tile
tileAt board i j = if 0 <= i && i <= 7 && 0 <= j && j <= 7
    then board!!i!!j else OutOfBoard

-- sets tile at coord
setTile :: Tile -> Board -> Board
setTile (Tile i1 j1 p1) board =
    [[ if i == i1 && j == j1 then
            Tile i j p1
        else Tile i j p | (Tile i j p) <- rows ] | rows <- board ]

-- gets the opposite player
oppPlayer :: Player -> Player
oppPlayer White = Black
oppPlayer Black = White

-- pieces are the opposite colors?
isOppColor :: Piece -> Piece -> Bool
isOppColor (Piece _ a) (Piece _ b) = a /= b
isOppColor _ _ = False

-- gets tiles in "line of sight" along some direction from a starting tile
alongTiles :: Board -> Tile -> Direction -> [Tile]
alongTiles board (Tile i j _) dir =
    case nextTile of
        Tile _ _ Empty -> nextTile : alongTiles board nextTile dir
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


knightTiles :: Board -> Tile -> [Tile]
knightTiles board (Tile i j _) = filter (/= OutOfBoard) [
    tileAt board (i+2) (j+1), tileAt board (i+1) (j+2),
    tileAt board (i-1) (j+2), tileAt board (i-2) (j+1),
    tileAt board (i-2) (j-1), tileAt board (i-1) (j-2),
    tileAt board (i+1) (j-2), tileAt board (i+2) (j-1)]

kingTiles :: Board -> Tile -> [Tile]
kingTiles board (Tile i j _) = filter (/= OutOfBoard) [
    tileAt board (i+1) j, tileAt board (i+1) (j+1),
    tileAt board i (j+1), tileAt board (i-1) (j+1),
    tileAt board (i-1) j, tileAt board (i-1) (j-1),
    tileAt board i (j-1), tileAt board (i+1) (j-1)]

queenTiles :: Board -> Tile -> [Tile]
queenTiles board tile =
    concat [alongTiles board tile dir | dir <- [NO, NE, EA, SE, SO, SW, WE, NW]]

bishopTiles :: Board -> Tile -> [Tile]
bishopTiles board tile =
    concat [alongTiles board tile dir | dir <- [NE, SE, SW, NW]]

rookTiles :: Board -> Tile -> [Tile]
rookTiles board tile =
    concat [alongTiles board tile dir | dir <- [NO, SO, EA, WE]]

-- a pawn's capture tiles
pawnTakeTiles :: Board -> Tile -> [Tile]
pawnTakeTiles board (Tile i j (Piece _ player)) =
        filter (/= OutOfBoard) [tileAt board (i+r) (j-1), tileAt board (i+r) (j+1)]
    where r = if player == White then 1 else -1

-- a pawn's movement tiles
pawnMoveTiles :: Board -> Tile -> [Tile]
pawnMoveTiles board (Tile i j (Piece _ player)) =
    takeWhile (\(Tile _ _ p) -> p == Empty)
        (filter (/= OutOfBoard) [tileAt board (i+di*r) j | di <- [1..n]])
    where (n, r) = case (i, player) of
            (1, White) -> (2,1)
            (6, Black) -> (2,-1)
            (_, White) -> (1,1)
            (_, Black) -> (1,-1)

-- a pawn's en passant tiles
enPassantTiles :: State -> Tile -> [Tile]
enPassantTiles (State board flags player) (Tile i j (Piece _ owner)) =
        if
            player == owner &&
            enPassant flags /= -1 &&
            abs (enPassant flags - j) == 1 &&
            i == r
        then
            [Tile i' (enPassant flags) Empty]
        else []
    where
        (i',r) = if player == White then (i+1,4) else (i-1,3)

-- a king's castle tiles
castleTiles :: State -> Tile -> [Tile]
castleTiles (State board flags player) (Tile i j (Piece _ owner)) =
    if owner /= player then [] else
        (if canQueenSide && queenSide == queenConfig && not queenAttacked then
            [Tile i 2 Empty]
        else [])++
        (if canKingSide && kingSide == kingConfig && not kingAttacked then
            [Tile i 6 Empty]
        else [])
    where
        canQueenSide =
            if player == White then whiteCanQueenSide flags else blackCanQueenSide flags
        canKingSide =
            if player == White then whiteCanKingSide flags else blackCanKingSide flags
        queenSide = take 5 (board!!i)
        kingSide = drop 4 (board!!i)
        i' = if player == White then 0 else 7
        queenConfig =
            [Tile i' 0 (Piece Rook player), Tile i' 1 Empty, Tile i' 2 Empty, Tile i' 3 Empty, Tile i 4 (Piece King player)]
        kingConfig =
            [Tile i' 4 (Piece King player), Tile i' 5 Empty, Tile i' 6 Empty, Tile i' 7 (Piece Rook player)]
        queenAttacked = any (\t -> (oppPlayer player `isAttacking` t) board) (tail queenSide)
        kingAttacked = any (\t -> (oppPlayer player `isAttacking` t) board) (take 3 kingSide)





{- MAINS -}

main :: IO State
main = playGame startingState

playGame :: State -> IO State
playGame state = do
    print state
    if isCheckmate state then do
            putStrLn ("Checkmate! " ++ show (oppPlayer player) ++ " wins")
            return state
    else
        if isStalemate state then do
            putStrLn "Stalemate..."
            return state
        else do
            when (player `isChecked` board) (putStrLn "You are in check!")
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
                            print (filter (\m -> isValid m state) (pieceMoves state tile))
                        Nothing -> do
                            putStrLn "wrong format!"
                    playGame state
                _ -> -- oh my goodness the indent
                    case readMove board line of
                        Just move ->
                            if isValid move state then
                                let move' = idMove move state
                                    Move mtype (Tile i j _) to = move' 
                                in if isPromotion move' then do
                                    putStrLn "Promote pawn to? (r|n|b|q):"
                                    pieceStr <- getLine
                                    let piece = (case pieceStr of
                                            "r" -> Piece Rook player
                                            "n" -> Piece Knight player
                                            "b" -> Piece Bishop player
                                            "q" -> Piece Queen player
                                            _ -> Empty)
                                    if piece == Empty then do
                                        putStrLn "invalid piece!"
                                        playGame state
                                    else
                                        playGame (play (Move mtype (Tile i j piece) to) state)
                                else
                                    playGame (play move' state)
                            else do
                                putStrLn "Illegal move!"
                                playGame state
                        Nothing -> do
                            putStrLn "wrong format!"
                            playGame state
    where (State board flags player) = state




{- IO UTILITIES -}

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
            EnPassant -> " ep. "
        ++ show to

instance Show State where
    show :: State -> String
    show (State board flags player) =
        printBoard board ++ "\nflags: " ++ show flags ++ "\n" ++ show player ++"'s turn"

-- prints out the board
printBoard :: Board -> String
printBoard board = concat [intToDigit i : ' ' : intersperse ' ' (printTile <$> board!!i) ++ "\n" | i <- [7,6..0]] ++ "/ 0 1 2 3 4 5 6 7"

-- prints out a tile
printTile :: Tile -> Char
printTile OutOfBoard = 'X'
printTile (Tile _ _ Empty) = '☐'
printTile (Tile _ _ (Piece ptype owner)) = chr ((if owner == Black then 6 else 0) + ord (
    case ptype of
        Pawn -> '♙'
        Rook -> '♖'
        Knight -> '♘'
        Bishop -> '♗'
        Queen -> '♕'
        King -> '♔'))