{-# LANGUAGE TemplateHaskell #-}

module Model (Position, Role(..), Direction(..), Piece(..), makePiece, Move(..),
  Player, GameState(..), role, position, player1, player2, turnTracker) where

import Control.Lens ((^.), set, over, _1, _2, makeLenses)

type Position = (Int, Int)

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

data Move = Mv {
  _pieceAfterMove :: Piece,
  _direction :: Direction
}
makeLenses ''Move

instance Show Move where
  show (Mv pc dir) = show (pc ^. role) ++ " " ++ show dir ++ " to " ++ show (pc ^. position)

type Player = [Piece]

data GameState = GS {
  _player1 :: Player,
  _player2 :: Player,
  _turnTracker :: Bool
}
makeLenses ''GameState