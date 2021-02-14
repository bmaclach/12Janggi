module Helper (isPlaceable, getValidMoves, currentPlayer, otherPlayer, 
  yourTerritory, enemyTerritory) where

import Model
import Control.Lens ((^.), set, over, _1)
import Control.Monad.State (State, get)
import Prelude hiding (Left, Right)

isPlaceable :: Position -> State GameState Bool
isPlaceable pos = do
  gs <- get
  let allPieces = gs ^. player1 ++ gs ^. player2
  return $ not $ 
    pos `elem` map (^. position) allPieces || pos ^. _1 == enemyTerritory gs

getValidMoves :: State GameState [Move]
getValidMoves = do
  gs <- get
  let yourTurn = gs ^. turnTracker
      movementModifier = if yourTurn then 1 else -1
      you = gs ^. currentPlayer gs
      opponent = gs ^. otherPlayer gs
      getValidMovesForPiece :: [Move] -> Piece -> [Move]
      getValidMovesForPiece mvs pc = 
        mvs ++ map (over _1 (flip (set position) pc)) (getPossibleMoves pc)
      getPossibleMoves :: Piece -> [(Position, Direction)]
      getPossibleMoves (P _ _ (0,0)) = []
      getPossibleMoves (P _ r (x,y)) = let
        direcs = getMovableDirections r
        newPos = map (\d -> 
          (x + (movementModifier * xMovement d), 
          y + (movementModifier * yMovement d))) direcs
        in [(p, d) | (p, d) <- zip newPos direcs,
        isValidPosition p, p `notElem` map (^. position) you]
  return $ foldl getValidMovesForPiece [] you

getMovableDirections :: Role -> [Direction]
getMovableDirections Man = [Forward]
getMovableDirections General = [Forward, Back, Right, Left]
getMovableDirections Minister = [ForwardRight, ForwardLeft, BackRight, BackLeft]
getMovableDirections King = [Forward, Back, Right, Left, ForwardRight,
  ForwardLeft, BackRight, BackLeft]
getMovableDirections FeudalLord = [Forward, Back, Right, Left, ForwardRight,
  ForwardLeft]

isValidPosition :: Position -> Bool
isValidPosition (x,y) = 0 < x && x < 5 && 0 < y && y < 4

isActive :: Piece -> Bool
isActive pc = pc ^. position /= (0,0)

isForward :: Direction -> Bool
isForward Forward = True
isForward ForwardRight = True
isForward ForwardLeft = True
isForward _ = False

isBack :: Direction -> Bool
isBack Back = True
isBack BackRight = True
isBack BackLeft = True
isBack _ = False

xMovement :: Direction -> Int
xMovement d
  | isForward d = 1
  | isBack d = -1
  | otherwise = 0

isRight :: Direction -> Bool
isRight Right = True
isRight ForwardRight = True
isRight BackRight = True
isRight _ = False

isLeft :: Direction -> Bool
isLeft Left = True
isLeft ForwardLeft = True
isLeft BackLeft = True
isLeft _ = False

yMovement :: Direction -> Int
yMovement d
  | isRight d = 1
  | isLeft d = -1
  | otherwise = 0

currentPlayer gs = if gs ^. turnTracker then player1 else player2
otherPlayer gs = if gs ^. turnTracker then player2 else player1
yourTerritory gs = if gs ^. turnTracker then 1 else 4
enemyTerritory gs = if gs ^. turnTracker then 4 else 1