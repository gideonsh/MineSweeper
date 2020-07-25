{-# OPTIONS -Wall  #-}
--Daniel Kalmykov
--207117730
--Gideon Shachar
--204713689

module MineSweeper
    ( 
    Cell
    , validateArgs
    , validateMove
    , newGame
    , printGame
    , showGame
    , showRows
    , showData
    , dig
    , toggleFlag
    , checkFlag
    , act
    , isGameOn
    , gameOver
    , isValidAction
    , sameBoard
    , winTheGame
    , notWinTheGame
    , printCulNumber
    , printPlayAgain
    --, fromBoolIO
    , checkAction
    ) where

import System.Random.Shuffle
import System.Random

data Action = Dig Int Int 
 | Flag Int Int 
 deriving(Read)

data Cell = Cell{
    sign :: Char,
    num :: Int, 
    flag :: Char,
    played :: Bool
    }deriving(Read, Eq, Show)


validateArgs :: Int -> Int -> Int -> Bool 
validateArgs r c m = if ((validateRows r) && (validateCols c) && (validateMines r c m)) then True else False 

validateRows :: Int -> Bool
validateRows r 
  | r < 10 = False
  | r > 20 = False
  | otherwise = True

validateCols :: Int -> Bool
validateCols c 
  | c < 10 = False
  | c > 20 = False
  | otherwise = True

validateMines :: Int -> Int -> Int -> Bool
validateMines r c m
  | m < 4 = False
  | m > 199 = False
  | m > (r * c - 1) = False
  | otherwise = True

validateMove :: [[Cell]] -> Int -> Int -> Bool
validateMove b r c = if(length b < r || r < 1 || length (b !! 0) < c) || c < 1 then False else True

intToChar :: Int -> Char
intToChar number
  | number == 1 = '1'
  | number == 2 = '2'
  | number == 3 = '3' 
  | number == 4 = '4' 
  | number == 5 = '5'  
  | number == 6 = '6' 
  | number == 7 = '7' 
  | number == 8 = '8' 
  | otherwise = '0'

makeList :: Int -> [Cell]
makeList c = take c (repeat (makeCell '0' 0 ' ' False))

makeBoard :: Int -> Int -> [[Cell]]
makeBoard r c = take r (repeat (makeList c)) 

updateList :: [Cell] -> Int -> Char -> Int -> Char -> Bool -> [Cell]
updateList l c ch i f bl = (take (c - 1) l) ++ [(makeCell ch i f bl)] ++ (drop c l)

updateBoard :: [[Cell]] -> Int -> Int -> Char -> Int -> Char-> Bool -> [[Cell]]
updateBoard b r c ch i f bl = (take (r - 1) b) ++ [(updateList (b !! (r-1)) c ch i f bl)] ++ (drop r b)

makeCell :: Char -> Int -> Char -> Bool -> Cell
makeCell c i f b = Cell{ sign = c, num = i, flag = f, played = b}

generateMines :: Int -> Int -> Int -> [(Int,Int)]
generateMines r c m = take m (shuffle' (generateMines2 [1..r] [1..c]) (r*c) (mkStdGen 10)) 

generateMines2 :: [Int] -> [Int] -> [(Int,Int)]
generateMines2 [] _ = []
generateMines2 [x] lst = generateMines3 [x] lst
generateMines2 (x:xs) lst = (generateMines3 [x] lst) ++ (generateMines2 xs lst) 

generateMines3 :: [Int] -> [Int] -> [(Int,Int)]
generateMines3 [x] [y] = [(x,y)]
generateMines3 [x] (y:ys) = (generateMines3 [x] [y]) ++ (generateMines3 [x] ys)
generateMines3 _ _ = []

setMines :: [[Cell]] -> [(Int,Int)] -> [[Cell]]
setMines b [] = b
setMines b [x] = setNums (updateBoard b (fst x) (snd x) '*' 0 ' ' False) (fst x) (snd x)
setMines b (x:xs) = setMines (setNums (updateBoard b (fst x) (snd x) '*' 0 ' ' False) (fst x) (snd x)) xs

setNums :: [[Cell]] -> Int -> Int -> [[Cell]] 
setNums b r c = (setNums8 (setNums7 (setNums6 (setNums5 (setNums4 (setNums3 (setNums2 (setNums1 b r c) r c) r c) r c) r c) r c) r c) r c)

setNums1 :: [[Cell]] -> Int -> Int -> [[Cell]]
setNums1 b r c = if validateMove b (r-1) (c-1) then updateBoard b (r-1) (c-1) (sign (b !! (r-2) !! (c-2))) ((num (b !! (r-2) !! (c-2)))+1) ' ' False else b 

setNums2 :: [[Cell]] -> Int -> Int -> [[Cell]]
setNums2 b r c = if validateMove b (r-1) c then updateBoard b (r-1) c (sign (b !! (r-2) !! (c-1))) ((num (b !! (r-2) !! (c-1)))+1) ' ' False else b 

setNums3 :: [[Cell]] -> Int -> Int -> [[Cell]]
setNums3 b r c = if validateMove b (r-1) (c+1) then updateBoard b (r-1) (c+1) (sign (b !! (r-2) !! c)) ((num (b !! (r-2) !! c))+1) ' ' False else b 

setNums4 :: [[Cell]] -> Int -> Int -> [[Cell]]
setNums4 b r c = if validateMove b r (c-1) then updateBoard b r (c-1) (sign (b !! (r-1) !! (c-2))) ((num (b !! (r-1) !! (c-2)))+1) ' ' False else b 

setNums5 :: [[Cell]] -> Int -> Int -> [[Cell]]
setNums5 b r c = if validateMove b (r+1) (c-1) then updateBoard b (r+1) (c-1) (sign (b !! r !! (c-2))) ((num (b !! r !! (c-2)))+1) ' ' False else b 

setNums6 :: [[Cell]] -> Int -> Int -> [[Cell]]
setNums6 b r c = if validateMove b (r+1) c then updateBoard b (r+1) c (sign (b !! r !! (c-1))) ((num (b !! r !! (c-1)))+1) ' ' False else b 

setNums7 :: [[Cell]] -> Int -> Int -> [[Cell]]
setNums7 b r c = if validateMove b (r+1) (c+1) then updateBoard b (r+1) (c+1) (sign (b !! r !! c)) ((num (b !! r !! c))+1) ' ' False else b 

setNums8 :: [[Cell]] -> Int -> Int -> [[Cell]]
setNums8 b r c = if validateMove b r (c+1) then updateBoard b r (c+1) (sign (b !! (r-1) !! c)) ((num (b !! (r-1) !! c))+1) ' ' False else b 

setBoard :: [[Cell]] -> [[Cell]]
setBoard b = (setBoard2 b (length b) (length (b !! 0)))

setBoard2 :: [[Cell]] -> Int -> Int -> [[Cell]]
setBoard2 b r c = if r == 1 then setBoard3 b 1 c else setBoard2 (setBoard3 b r c) (r-1) c 

setBoard3 :: [[Cell]] -> Int -> Int -> [[Cell]]
setBoard3 b r c = if c == 1 then setCell b r c else setBoard3 (setCell b r c) r (c-1)

setCell :: [[Cell]] -> Int -> Int -> [[Cell]]
setCell b r c = if (sign (b !! (r-1) !! (c-1))) == '0' then updateBoard b r c (intToChar (num (b !! (r-1) !! (c-1)))) (num (b !! (r-1) !! (c-1))) ' ' False else b

exposeZero :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeZero b r c = (exposeBottomRight (exposeTopRight (exposeBottomLeft (exposeTopLeft (exposeLeft (exposeRight (exposeBottom (exposeTop b r c) r c) r c) r c) r c) r c) r c) r c)

exposeTopLeft :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeTopLeft b r c 
  | validateMove b (r-1) (c-1) && (sign (b !! (r-2) !! (c-2))) == '0' && (flag (b !! (r-2) !! (c-2))) == ' ' && (played (b !! (r-2) !! (c-2))) = dig (updateBoard b (r-1) (c-1) '0' (num (b !! (r-2) !! (c-2))) ' ' True) (r-1) (c-1)
  | validateMove b (r-1) (c-1) && (sign (b !! (r-2) !! (c-2))) /= '*' && (flag (b !! (r-2) !! (c-2))) == ' ' = (updateBoard b (r-1) (c-1) (sign (b !! (r-2) !! (c-2))) (num (b !! (r-2) !! (c-2))) ' ' True)
  | otherwise = b 

exposeLeft :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeLeft b r c   
  | validateMove b r (c-1) && (sign (b !! (r-1) !! (c-2))) == '0' && (flag (b !! (r-1) !! (c-2))) == ' ' && (played (b !! (r-1) !! (c-2))) == False = dig (updateBoard b r (c-1) '0' (num (b !! (r-1) !! (c-2))) ' ' True) r (c-1)
  | validateMove b r (c-1) && (sign (b !! (r-1) !! (c-2))) /= '*' && (flag (b !! (r-1) !! (c-2))) == ' ' = (updateBoard b r (c-1) (sign (b !! (r-1) !! (c-2))) (num (b !! (r-1) !! (c-2))) ' ' True)
  | otherwise = b 

exposeBottomLeft :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeBottomLeft b r c   
  | validateMove b (r+1) (c-1) && (sign (b !! r !! (c-2))) == '0' && (flag (b !! r !! (c-2))) == ' ' && (played (b !! r !! (c-2))) == False = dig (updateBoard b (r+1) (c-1) '0' (num (b !! r !! (c-2))) ' ' True) (r+1) (c-1)
  | validateMove b (r+1) (c-1) && (sign (b !! r !! (c-2))) /= '*' && (flag (b !! r !! (c-2))) == ' ' = (updateBoard b (r+1) (c-1) (sign (b !! r !! (c-2))) (num (b !! r !! (c-2))) ' ' True)
  | otherwise = b 

exposeBottom :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeBottom b r c  
  | validateMove b (r+1) c && (sign (b !! r !! (c-1))) == '0' && (flag (b !! r !! (c-1))) == ' ' && (played (b !! r !! (c-1))) == False = dig (updateBoard b (r+1) c '0' (num (b !! r !! (c-1))) ' ' True) (r+1) c
  | validateMove b (r+1) c && (sign (b !! r !! (c-1))) /= '*' && (flag (b !! r !! (c-1))) == ' ' = (updateBoard b (r+1) c (sign (b !! r !! (c-1))) (num (b !! r !! (c-1))) ' ' True)
  | otherwise = b 

exposeBottomRight :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeBottomRight b r c   
  | validateMove b (r+1) (c+1) && (sign (b !! r !! c)) == '0' && (flag (b !! r !! c)) == ' ' && (played (b !! r !! c)) == False = dig (updateBoard b (r+1) (c+1) '0' (num (b !! r !! c)) ' ' True) (r+1) (c+1)
  | validateMove b (r+1) (c+1) && (sign (b !! r !! c)) /= '*' && (flag (b !! r !! c)) == ' ' = (updateBoard b (r+1) (c+1) (sign (b !! r !! c)) (num (b !! r !! c)) ' ' True)
  | otherwise = b 

exposeRight :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeRight b r c  
  | validateMove b r (c+1) && (sign (b !! (r-1) !! c)) == '0' && (flag (b !! (r-1) !! c)) == ' ' && (played (b !! (r-1) !! c)) == False = dig (updateBoard b r (c+1) '0' (num (b !! (r-1) !! c)) ' ' True) r (c+1)
  | validateMove b r (c+1) && (sign (b !! (r-1) !! c)) /= '*' && (flag (b !! (r-1) !! c)) == ' ' = (updateBoard b r (c+1) (sign (b !! (r-1) !! c)) (num (b !! (r-1) !! c)) ' ' True)
  | otherwise = b 

exposeTopRight :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeTopRight b r c   
  | validateMove b (r-1) (c+1) && (sign (b !! (r-2) !! c)) == '0' && (flag (b !! (r-2) !! c)) == ' ' && (played (b !! (r-2) !! c)) == False = dig (updateBoard b (r-1) (c+1) '0' (num (b !! (r-2) !! c)) ' ' True) (r-1) (c+1)
  | validateMove b (r-1) (c+1) && (sign (b !! (r-2) !! c)) /= '*' && (flag (b !! (r-2) !! c)) == ' ' = (updateBoard b (r-1) (c+1) (sign (b !! (r-2) !! c)) (num (b !! (r-2) !! c)) ' ' True)
  | otherwise = b 

exposeTop :: [[Cell]] -> Int -> Int -> [[Cell]]
exposeTop b r c   
  | validateMove b (r-1) c && (sign (b !! (r-2) !! (c-1))) == '0' && (flag (b !! (r-2) !! (c-1))) == ' ' && (played (b !! (r-2) !! (c-1))) == False = dig (updateBoard b (r-1) c '0' (num (b !! (r-2) !! (c-1))) ' ' True) (r-1) c
  | validateMove b (r-1) c && (sign (b !! (r-2) !! (c-1))) /= '*' && (flag (b !! (r-2) !! (c-1))) == ' ' = (updateBoard b (r-1) c (sign (b !! (r-2) !! (c-1))) (num (b !! (r-2) !! (c-1))) ' ' True)
  | otherwise = b 

newGame :: Int -> Int -> Int -> [[Cell]]  
newGame r c m = (setBoard (setMines (makeBoard r c) (generateMines r c m)))

printGame :: [[Cell]] -> Int -> String
printGame [] _ = ""
printGame [x] curr = ((printRowNumber curr) ++ printRow x ++ "\n")
printGame (x:xs) curr = (printRowNumber curr) ++ printRow x ++ "\n" ++ printGame xs (curr+1)

printRow :: [Cell] -> String
printRow [] = ""
printRow [x] 
  | played x == True && flag x == '!' = "[!] "
  | played x == True = '[':(sign x):']':' ':[]
  | otherwise = "[ ] "
printRow (x:xs) = printRow [x] ++ printRow xs

printRowNumber :: Int -> String
printRowNumber curr 
  | curr < 10 = "00" ++ (show curr) ++ " "
  | otherwise = "0" ++ (show curr) ++ " "

printCulNumber :: Int -> String
printCulNumber c = "    " ++ (printCul (take c [1..])) ++ "\n"

printCul :: [Int] -> String
printCul [] = " "
printCul [x] = "0" ++ (show x) ++ " "
printCul (x:xs) 
  | x < 10 = "00" ++ (show x) ++ " " ++ (printCul xs)
  | otherwise = "0" ++ (show x) ++ " " ++ (printCul xs)

showGame :: [[Cell]] -> IO()
showGame x = putStrLn (showRows x)

showRows :: [[Cell]] -> String
showRows [] = ""
showRows [x] = showData x ++ "\n"
showRows (x:xs) = showData x ++ "\n" ++ showRows xs

showData :: [Cell] -> String
showData [] = ""
showData [x] = '[':(sign x):']':[]
showData (x:xs) = showData [x] ++ showData xs

dig :: [[Cell]] -> Int -> Int -> [[Cell]] 
dig b r c 
  | validateMove b r c == True && (flag (b !! (r-1) !! (c-1))) == '!' = b 
  | validateMove b r c == True && (sign (b !! (r-1) !! (c-1))) == '*' = (updateBoard b r c '*' (num (b !! (r-1) !! (c-1))) ' ' True) 
  | validateMove b r c == True && (sign (b !! (r-1) !! (c-1))) == '0' = exposeZero (updateBoard b r c '0' (num (b !! (r-1) !! (c-1))) ' ' True) r c
  | validateMove b r c == True && (played (b !! (r-1) !! (c-1))) == True = b 
  | validateMove b r c == True = (updateBoard b r c (sign (b !! (r-1) !! (c-1))) 0 ' ' True) 
  | otherwise = b 

toggleFlag :: [[Cell]] -> Int -> Int -> [[Cell]]
toggleFlag b r c = if ((checkFlag b r c) == True) then (updateBoard b r c (sign (b !! (r-1) !! (c-1))) (num (b !! (r-1) !! (c-1))) ' ' False) else (if ((sign (b !! (r-1) !! (c-1))) /= '*') && ((played (b !! (r-1) !! (c-1))) == True) then b else (updateBoard b r c (sign (b !! (r-1) !! (c-1))) (num (b !! (r-1) !! (c-1))) '!' True))

checkFlag :: [[Cell]] -> Int -> Int -> Bool
checkFlag b r c = if((flag (b !! (r-1) !! (c-1))) == '!') then True else False

act :: [[Cell]] -> String -> Int -> Int -> [[Cell]]
act b ac r c 
  | ac == "Flag" = toggleFlag b r c 
  | ac == "Dig" = dig b r c 
  | otherwise = b 

isGameOn :: [[Cell]] -> Int -> Int -> Bool
isGameOn b r c 
  | (sign (b !! (r-1) !! (c-1))) == '*' && (flag (b !! (r-1) !! (c-1))) == ' ' && (played (b !! (r-1) !! (c-1))) == True = False
  | otherwise = True 

printPlayAgain :: Int -> IO Bool
printPlayAgain _ = do
  putStrLn "wrong input play again"
  return True

gameOver :: [[Cell]] -> IO()
gameOver x = putStrLn ((printCulNumber (length (x !! 0))) ++ (gameOver1 x 1 ) ++ "Boom! game is over")

gameOver1 :: [[Cell]] -> Int -> String
gameOver1 [] _ = ""
gameOver1 [x] curr = (printRowNumber curr) ++ gameOver2 x ++ "\n"
gameOver1 (x:xs) curr = (printRowNumber curr) ++ gameOver2 x ++ "\n" ++ gameOver1 xs (curr+1)

gameOver2 :: [Cell] -> String
gameOver2 [] = ""
gameOver2 [x] 
  | played x == True && flag x == ' ' = '[':(sign x):']':' ':[]
  | flag x == '!' = "[!] "
  | sign x == '*' = "[*] "
  | otherwise = "[ ] "
gameOver2 (x:xs) = gameOver2 [x] ++ gameOver2 xs

isValidAction :: [[Cell]] -> Int -> Int -> String -> Bool 
isValidAction b r c ac 
  | r > 0 && r < ((length b)+1) && c > 0 && (c < ((length (b !! 0))+1)) && (ac == "Dig" || ac == "Flag") = True
  | otherwise = False

sameBoard :: [[Cell]] -> [[Cell]] -> Bool
sameBoard [x] [y] = sameBoard2 x y
sameBoard (x:xs) (y:ys) = ((sameBoard2 x y) && (sameBoard xs ys))
sameBoard _ _ = True

sameBoard2 :: [Cell] -> [Cell] -> Bool
sameBoard2 [x] [y] = sameBoard3 x y
sameBoard2 (x:xs) (y:ys) = ((sameBoard3 x y) && (sameBoard2 xs ys)) 
sameBoard2 _ _ = True

sameBoard3 :: Cell -> Cell -> Bool
sameBoard3 x y 
  | (played x) /= (played y) = False
  | (flag x) /= (flag y) = False
  | otherwise = True

winTheGame :: [[Cell]] -> Bool
winTheGame [] = True
winTheGame [x] = winTheGame2 x
winTheGame (x:xs) = ((winTheGame2 x) && (winTheGame xs)) 

winTheGame2 :: [Cell] -> Bool
winTheGame2 [] = True
winTheGame2 [x] = winTheGame3 x
winTheGame2 (x:xs) = ((winTheGame3 x) && (winTheGame2 xs)) 

winTheGame3 :: Cell -> Bool
winTheGame3 c 
  | (sign c) == '*' && (played c) == True && (flag c) == ' ' = False
  | (sign c) /= '*' && (played c) == False = False
  | (sign c) /= '*' && (played c) == True && (flag c) == '!' = False
  | otherwise = True

notWinTheGame :: [[Cell]] -> Bool
notWinTheGame [] = True
notWinTheGame [x] = notWinTheGame2 x
notWinTheGame (x:xs) = if notWinTheGame2 x then True else notWinTheGame xs 

notWinTheGame2 :: [Cell] -> Bool
notWinTheGame2 [] = True
notWinTheGame2 [x] = notWinTheGame3 x
notWinTheGame2 (x:xs) = if notWinTheGame3 x then True else notWinTheGame2 xs  

notWinTheGame3 :: Cell -> Bool
notWinTheGame3 c
  | (sign c) == '*' && (played c) == True && (flag c) == ' ' = True
  | (sign c) /= '*' && (played c) == False = True
  | (sign c) /= '*' && (played c) == True && (flag c) == '!' = True
  | otherwise = False

checkAction :: [[Cell]] -> String -> Int -> Int -> IO Bool 
checkAction b ac r c 
  | (r > 0) && (r < ((length b)+1)) && (c > 0) && (c < ((length (b !! 0))+1)) && (ac == "Dig" || ac == "Flag") = return True 
  | otherwise = do
    putStrLn "Invalid input, please try again"
    return False