module Main where

import Model
import Controller
import View
import Control.Lens
import Control.Monad.State
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runTurn initialState

initialState :: GameState
initialState = GS {
  _player1 = [makePiece 1 Man 2 2, makePiece 2 King 1 2, 
    makePiece 3 General 1 3, makePiece 4 Minister 1 1],
  _player2 = [makePiece 5 Man 3 2, makePiece 6 King 4 2, 
    makePiece 7 General 4 1, makePiece 8 Minister 4 3],
  _turnTracker = True
}
