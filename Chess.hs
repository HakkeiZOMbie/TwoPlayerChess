data State = State [[Tile]] [Flag]

-- a move moves a piece from one tile to another tile
data Move = Move Tile Tile

-- just a coordinate with optionally a piece on it for now
data Tile = Tile Int Int Piece 

data Piece = Pawn | Rook | Knight | Bishop | Queen | King | Nothing
data Flag = Castled | EnPassant | FiftyMoves

-- plays a given move
play :: Move -> State -> State

-- checks whether a move is valid
checkMove :: Move -> State -> Bool