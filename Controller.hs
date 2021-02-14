module Controller (executeMove) where

import Model
import Helper
import Control.Monad.State (State, get, modify)
import Control.Lens ((^.), set, over, _1, _2)
import Data.List (delete, find, nub)

turn :: State GameState ()
turn = do
  mvs <- getValidMoves
  exectuteMove (head mvs)
  won <- checkIfWon
  lost <- checkIfLost
  endTurn
  unless (won || lost) turn

executeMove :: Move -> State GameState ()
executeMove (pc, _) = do
  modify (updatePosition pc)
  capturePieces $ pc ^. position
  promoteMan

executePlace :: Piece -> Position -> State GameState ()
executePlace pc@(P id r (0,0)) pos = do
  gs <- get
  let allPieces = gs ^. player1 ++ gs ^. player2
  if isPlaceable pos 
    then modify (updatePosition (P id r pos))
    else error "Piece cannot be placed there!"
executePlace _ _ = error "Placed piece must be inactive"

updatePosition :: Piece -> GameState -> GameState
updatePosition pc gs = over (currentPlayer gs) (nub . (pc:)) gs

capturePieces :: Position -> State GameState ()
capturePieces pos = do
  gs <- get
  let opponent = gs ^. otherPlayer gs
      capturedPiece = find ((== pos) . (^. position)) opponent
  modify (maybe id (over (otherPlayer gs) . delete) capturedPiece)
  modify (maybe id (over (currentPlayer gs) . (:) . deactivate) capturedPiece)

promoteMan :: State GameState ()
promoteMan = do
  gs <- get
  let isPromotable pc = pc ^. role == Man 
        && pc ^. (position . _1) == enemyTerritory gs
      promote pc = if isPromotable pc then set role FeudalLord pc else pc
  modify (over (currentPlayer gs) (map promote))

deactivate :: Piece -> Piece
deactivate = set position (0,0)

endTurn :: State GameState ()
endTurn = modify (over turnTracker not)
  
checkIfWon :: State GameState Bool
checkIfWon = do
  gs <- get
  let you = gs ^. currentPlayer gs
  return $ length (filter (== King . (^. role)) you) > 1

checkIfLost :: State GameState Bool
checkIfLost = do
  gs <- get
  let opponent = gs ^. otherPlayer gs
      opponentsKing = find (== King . (^. role)) opponent
  return $ maybe false (== yourTerritory gs . (^. (position . _1))) opponentsKing