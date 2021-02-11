{-# LANGUAGE TemplateHaskell #-}

module Model (Role, Direction, Piece(..), Player, GameState, 
  role, position, player1, player2, turnTracker, initialState) where

import Control.Lens ((^.), _1, _2, makeLenses)
import Control.Monad.State (State, get, modify)
import Prelude hiding (Left, Right)

data Role = Man | General | Minister | King | FeudalLord deriving (Eq, Show)

data Direction = Right | Left | Forward | Back |
  ForwardRight | ForwardLeft | BackRight | BackLeft deriving (Eq, Show)

data Piece = P {
  idnum :: Int,
  _role :: Role,
  _position :: Position
}
makeLenses ''Piece

instance Eq Piece where
  p1 == p2 = idnum p1 == idnum p2

instance Show Piece where
  show p = show (p ^. role) ++ " at (" ++ show (p ^. (position . _1)) ++ "," ++
    show (p ^. (position . _2)) ++ ")"

makePiece :: Int -> Role -> Int -> Int -> Piece
makePiece i r x y = P i r (x, y)

type Player = [Piece]
type Move = (Piece, Direction, Position)
type Position = (Int, Int)

-- If your turn ends and the opponent's king is in your territory, you lose

data GameState = GS {
  _player1 :: Player,
  _player2 :: Player,
  _turnTracker :: Bool
}
makeLenses ''GameState

initialState :: GameState
initialState = GS {
  _player1 = [makePiece 1 Man 2 2, makePiece 2 King 1 2, 
    makePiece 3 General 1 3, makePiece 4 Minister 1 1],
  _player2 = [makePiece 5 Man 3 2, makePiece 6 King 4 2, 
    makePiece 7 General 4 1, makePiece 8 Minister 4 3],
  _turnTracker = True
}

isValidPosition :: Position -> Bool
isValidPosition (x,y) = 0 < x < 5 && 0 < y < 4

getMovableDirections :: Role -> [Direction]
getMovableDirections Man = [Forward]
getMovableDirections General = [Forward, Back, Right, Left]
getMovableDirections Minister = [ForwardRight, ForwardLeft, BackRight, BackLeft]
getMovableDirections King = [Forward, Back, Right, Left, ForwardRight,
  ForwardLeft, BackRight, BackLeft]
getMovableDirections FeudalLord = [Forward, Back, Right, Left, ForwardRight,
  ForwardLeft]

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

getValidMoves :: State GameState [Move]
getValidMoves = do
  gs <- get
  let yourTurn = gs ^. turnTracker
      movementModifier = if yourTurn then 1 else -1
      you = if yourTurn then gs ^. player1 else gs ^. player2
      opponent = if yourTurn then gs ^. player2 else gs ^. player1 
      getValidMovesForPiece :: [Move] -> Piece -> [Move]
      getValidMovesForPiece mvs p = mvs ++ map (p,) (getPossibleDirections p)
      getPossibleMoves :: Piece -> [(Direction, Position)]
      getPossibleMoves (P _ r (x,y)) = let
        direcs = getMovableDirections r
        newPos = map (\d -> 
          (x + (movementModifier * xMovement d), 
          y + (movementModifier * yMovement d))) direcs
        in [(d, p) | (d, p) <- zip direcs newPos,
        isValidPosition p, p `notElem` map (^. position) you]
  return $ foldl getValidMovesForPiece [] you

{-
  1  2  3  4
1 _  _  _  _
2 _  _  _  _
3 _  _  _  _
-}