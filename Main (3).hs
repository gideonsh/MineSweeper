{-# OPTIONS -Wall  #-}
--Daniel Kalmykov
--207117730
--Gideon Shachar
--204713689

module Main
    ( 
    gameStep,
    playGame,
    main,
    ) where

--import Lib
--import Data.List
import System.Environment 
import qualified MineSweeper
import Safe
import Data.Maybe(fromJust)
 

main :: IO ()
main = do
    begining <- getArgs
    let ro = readMay (begining !! 0) :: Maybe Int 
    let co = readMay (begining !! 1) :: Maybe Int 
    let mi = readMay (begining !! 2) :: Maybe Int
    if ro /= Nothing && co /= Nothing && mi /= Nothing && (length begining) == 3 && (MineSweeper.validateArgs (fromJust ro) (fromJust co) (fromJust mi))
        then playGame (begining !! 0) (begining !! 1) (begining !! 2)
         else putStrLn "Sorry wrong input \n Goodbye\n"


playGame :: String -> String -> String -> IO () 
playGame r c m = do
    let board = MineSweeper.newGame (read r) (read c) (read m)
    putStrLn ("initial state of a game with the size board of " ++ r ++ "X" ++ c ++ ":")
    finishedBoard <- gameStep board True
    if MineSweeper.sameBoard board finishedBoard 
        then putStrLn "Wrong Input goodbye..." 
        else (if MineSweeper.winTheGame finishedBoard 
                then putStrLn ((MineSweeper.printCulNumber (length (finishedBoard !! 0))) ++ (MineSweeper.printGame finishedBoard 1) ++  "You win! all mines cleared")
                 else MineSweeper.gameOver finishedBoard)


gameStep :: [[MineSweeper.Cell]] -> Bool -> IO [[MineSweeper.Cell]] 
gameStep b False = return b  
gameStep b True = do  
    putStrLn ((MineSweeper.printCulNumber (length (b !! 0))) ++ (MineSweeper.printGame b 1) ++ "what is your next move?")
    turn <- getLine
    let ro = readMay ((words turn) !! 1) :: Maybe Int
    let co = readMay ((words turn) !! 2) :: Maybe Int
    valid <- (MineSweeper.checkAction b ((words turn) !! 0) (fromJust ro) (fromJust co))
    if valid 
        then (if (ro /= Nothing && co /= Nothing) 
                then (if (MineSweeper.notWinTheGame (MineSweeper.act b ((words turn) !! 0) (fromJust ro) (fromJust co))) && (MineSweeper.isGameOn (MineSweeper.act b ((words turn) !! 0) (fromJust ro) (fromJust co)) (fromJust ro) (fromJust co)) 
                        then gameStep (MineSweeper.act b ((words turn) !! 0) (fromJust ro) (fromJust co)) True 
                        else (if (MineSweeper.isValidAction b (fromJust ro) (fromJust co) ((words turn) !! 0)) && (MineSweeper.isGameOn (MineSweeper.act b ((words turn) !! 0) (fromJust ro) (fromJust co)) (fromJust ro) (fromJust co))
                                then gameStep (MineSweeper.act b ((words turn) !! 0) (fromJust ro) (fromJust co)) False
                                else (if MineSweeper.isValidAction b (fromJust ro) (fromJust co) ((words turn) !! 0) 
                                        then gameStep (MineSweeper.act b ((words turn) !! 0) (fromJust ro) (fromJust co)) False 
                                        else gameStep b True))) 
                else do
                putStrLn "Invalid input, try again..\n"
                gameStep b True)
        else gameStep b True