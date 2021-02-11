module Main where

import Model
import Control.Lens ((^.))

main :: IO ()
main = print $ initialState ^. player1
