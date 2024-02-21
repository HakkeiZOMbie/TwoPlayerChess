{- DATA and TYPES -}

-- the game's state is its board state, additional flags, and the current 
-- move's player
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
data State = State Board [Flag] Player
    deriving (Eq, Show)

type Board = [[Tile]]

-- a move moves a piece from one tile to another tile
data Move = Move Tile Tile
    deriving (Eq, Show)

-- Points to 8 other tiles and may have a piece on it
data Tile = Tile Int Int Piece | OutOfBoard
    deriving (Eq, Show)

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
starting_board = [[Tile i j Empty | j <- [0..7]] | i <- [0..7]]
starting_state = State starting_board [] White


-- {- FUNCTIONS -}

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
