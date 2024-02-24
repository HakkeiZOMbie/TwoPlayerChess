# TwoPlayerChess
Two player chess in Haskell for UBC CPSC 312.

# Game Interface
For now, a terminal app that starts the game, asks white for the next move, plays it, asks black for the next move, repeat.

# Game State
Should be in a canonical form. Aside from the board state, we must keep track of:
  1. Castling - castling can no longer be done once the king has moved or the rook has moved.
  2. En Passant - can only be done immediately after the target pawn moves ranks in one move.
  3. 50 move draw rule.

Additionally we need to keep track of which tiles are under attack, as you can't move the king into check.

# TODO:
- [x] make king move during castle
- [x] make rook move during castle
- [x] properly set castle flags after each move
- [ ] properly check for attacked tiles during castle
- [ ] pawn promotion
- [ ] en passant
- [ ] 50 move rule
