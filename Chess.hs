{- DATA and TYPES -}

-- the game's state is its board state, additional flags, and the current 
-- move's player
data State = State [[Tile]] [Flag] Player
    deriving (Eq, Show)

-- a move moves a piece from one tile to another tile
data Move = Move Tile Tile
    deriving (Eq, Show)

-- just a coordinate with optionally a piece on it for now
data Tile = Tile Int Int Piece 
    deriving (Eq, Show)

-- players. 
-- Note that we can add other colors and the logic shouldn't change...
data Player = White | Black

-- constructor takes in the player the piece belongs to
data Piece = Pawn Player | Rook Player | Knight Player 
    | Bishop Player | Queen Player | King Player 
    | Empty
    deriving (Eq, Show)

-- additional flags. Not sure how we represent these yet.
data Flag = Castled | EnPassant | FiftyMoves
    deriving (Eq, Show)

{- CONSTANTS -}

turnOrder = [White, Black]
startingBoard = State [[]] [] White -- undecided on what Tiles should be

{- FUNCTIONS -}

-- plays a given move
play :: Move -> State -> State

-- reads move from string
readMove :: String -> Maybe Move

-- checks whether a move is valid
isValidMove :: Move -> State -> Bool

-- checks whether a move results in check
isCheck :: Move -> State -> Bool

-- checks whether a move results in checkmate
isMate :: Move -> State -> Bool
