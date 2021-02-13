module Controller (executeMove) where

import Model
import Helper
import Control.Monad.State (State, get, modify)
import Control.Lens ((^.), set, over, _1, _2)
import Data.List (delete, find, nub)

executeMove :: Move -> State GameState ()
executeMove (pc, _) = do
  modify (updatePosition pc)
  capturePieces $ pc ^. position
  endTurn

updatePosition :: Piece -> GameState -> GameState
updatePosition pc gs = over (currentPlayer gs) (nub . (pc:)) gs

capturePieces :: Position -> State GameState ()
capturePieces pos = do
  gs <- get
  let opponent = gs ^. otherPlayer gs
      capturedPiece = find ((== pos) . (^. position)) opponent
  modify (maybe id (over (otherPlayer gs) . delete) capturedPiece)
  modify (maybe id (over (currentPlayer gs) . (:) . deactivate) capturedPiece)

deactivate :: Piece -> Piece
deactivate = set position (0,0)

endTurn :: State GameState ()
endTurn = modify (over turnTracker not)
  