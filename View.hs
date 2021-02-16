module View (runTurn) where

import Model
import Controller
import Helper

import Control.Monad (unless, forM_)
import Control.Monad.State (runState)
import Control.Lens ((^.))
import Data.Maybe (isJust)
import Data.List (partition)

runTurn :: GameState -> IO ()
runTurn gs = do
  willMove <- requestMoveOrPlace gs
  if willMove
    then requestMove gs
    else requestPlace gs

requestMoveOrPlace :: GameState -> IO Bool
requestMoveOrPlace gs = 
  let (active, inactive) = partition isActive (gs ^. currentPlayer gs)
      requestDecision = do
        putStrLn "Will you:\n1 Move a piece\n2 Place a piece\nEnter 1 or 2 below:"
        decision <- getLine
        if decision == "1"
          then return True
          else if decision == "2"
            then return False
            else do
              putStrLn "You must type 1 or 2"
              requestDecision
  in if null inactive 
    then return True
    else if null (getValidMoves gs)
      then return False
      else requestDecision

requestPlace :: GameState -> IO ()
requestPlace gs = 
  let captives = filter (not . isActive) (gs ^. currentPlayer gs)
  in do
    pc <- requestSelection captives "Which piece do you want to place?"
    pos <- requestPosition gs
    let (result, newState) = runState (executePlace pc pos) gs
    unless (isJust result) $ runTurn newState
    checkResult result (gs ^. turnTracker)
    
requestMove :: GameState -> IO ()
requestMove gs = do
  let mvs = getValidMoves gs
  mv <- requestSelection mvs "How do you want to move?"
  let (result, newState) = runState (executeMove mv) gs
  unless (isJust result) $ runTurn newState
  checkResult result (gs ^. turnTracker)

requestSelection :: (Show a) => [a] -> String -> IO a
requestSelection [] _ = error "Code error: requestSelection called but no choices."
requestSelection [c] _ = return c
requestSelection cs msg = do
  putStrLn msg
  forM_ (zip [1..] cs) (\(num,c) -> putStrLn $ show num ++ " " ++ show c)
  nstr <- getLine
  let n = read nstr
  if 0 < n && n <= length cs then
    return (cs !! (n-1))
  else do
    putStrLn "Invalid input. Try again."
    requestSelection cs msg

requestPosition :: GameState -> IO Position
requestPosition gs = do
  putStrLn "Where do you want to place the piece?"
  x <- requestXCoord
  y <- requestYCoord
  let pos = (x,y)
  if isPlaceable pos gs then
    return pos 
  else do
    putStrLn "Invalid position. Try again."
    requestPosition gs

requestXCoord :: IO Int
requestXCoord = do
  putStrLn "Enter x-coordinate:"
  xstr <- getLine
  let x = read xstr
  if 0 < x && x < 5 then 
    return x 
  else do
    putStrLn "x-coordinate must be between 1 and 4. Try again."
    requestXCoord

requestYCoord :: IO Int
requestYCoord = do
  putStrLn "Enter y-coordinate:"
  ystr <- getLine
  let y = read ystr
  if 0 < y && y < 4 then
    return y
  else do
    putStrLn "y-coordinate must be between 1 and 3. Try again."
    requestYCoord

checkResult :: Maybe Bool -> Bool -> IO ()
checkResult Nothing _ = return ()
checkResult (Just r) tt = let you = "Player " ++ if tt then "1" else "2" in
  putStrLn $ you ++ if r then " won!" else " lost!"
