module Main where

import System.IO
import Data.Char (digitToInt)

data Players = EmptyTile Int
                | PlayerX
                | PlayerO
                deriving Eq

instance Show Players where
    show (EmptyTile n)   = show n
    show PlayerX    = "X"
    show PlayerO    = "O"

main :: IO ()
main =
    do 
        hSetBuffering stdin NoBuffering
        playGame board 0

playGame :: [Players] -> Int -> IO ()
playGame currentBoard currentMove = 
    let 
        lastMove = isLastMove currentBoard
    in
    do
        putStrLn ""
        printBoard currentBoard
        if lastMove
        then
            do
                putStrLn "no moves left: Draw"
                return ()
        else
            if even currentMove 
            then
                do
                    putStrLn "player1 choose place to move" 
                    playerMove <- getChar
                    let validMove = isValidMove (digitToInt playerMove) currentBoard

                    if validMove
                    then
                        do
                            let
                                newBoard = replaceElement PlayerX (digitToInt playerMove) currentBoard
                                won = isWon PlayerX newBoard
                            if won
                            then
                                do
                                    putStrLn ""
                                    putStrLn "Player1 wins"
                                    printBoard newBoard
                                    return ()
                            else 
                                playGame newBoard (currentMove+1)
                    else
                        do
                            putStrLn "invalid move try again"
                            playGame currentBoard currentMove
            else
                do
                    putStrLn "player2 choose place to move" 
                    playerMove <- getChar
                    let validMove = isValidMove (digitToInt playerMove) currentBoard

                    if validMove
                    then
                        do
                            let
                                newBoard = replaceElement PlayerO (digitToInt playerMove) currentBoard
                                won = isWon PlayerO newBoard
                            if won
                            then
                                do
                                    putStrLn ""
                                    putStrLn "Player2 wins"
                                    printBoard newBoard
                                    return ()
                            else 
                                playGame newBoard (currentMove+1)
                    else
                        do
                            putStrLn "invalid move try again"
                            playGame currentBoard currentMove

printBoard xs = foldr1 (>>) (map print [bottom, middle, top])
    where
        top = take 3 xs
        middle = take 3 $ drop 3 xs
        bottom = take 3 $ drop 6 xs

board :: [Players]
board = [EmptyTile x | x <- [1..9]]


isValidMove :: Int -> [Players] -> Bool
-- isValidMove n xs = xs!!n /= PlayerX && xs!!n /= PlayerO
isValidMove n xs = last (take n xs) /= PlayerX && last (take n xs) /= PlayerO

isLastMove :: [Players] -> Bool
isLastMove xs = length movesLeft == 9
    where
        movesLeft = [x | x <- xs, x == PlayerX || x == PlayerO]

replaceElement :: Players -> Int -> [Players] -> [Players]
replaceElement p move xs = firstElem ++ [p] ++ lastElem
    where
        splitList = splitAt move xs
        firstElem = init $ fst splitList
        lastElem = snd splitList

isWon :: Players -> [Players] -> Bool
isWon p xs = diagWin p xs || rowWin p xs || colWin p xs

diagWin :: Players -> [Players] -> Bool
diagWin p xs = left || right
    where
        topLeftCorner = head xs
        topRightCorner = last $ take 3 xs
        bottomLeftCorner = last $ take 7 xs
        bottomRightCorener = last xs
        middle = last $ take 5 xs
        left = topLeftCorner == p && middle == p && bottomRightCorener == p
        right = topRightCorner == p && middle == p && bottomRightCorener == p

-- colWin :: Players -> [Players] -> Bool
-- colWin p xs = 

rowWin :: Players -> [Players] -> Bool
rowWin p xs = length top == 3 || length middle == 3 || length bottom == 3
    where
        top     = [x | x <- take 4 xs, x == p]
        middle  = [x | x <- take 3 $ drop 3 xs, x == p]
        bottom  = [x | x <- take 3 $ drop 6 xs, x == p]

colWin :: Players -> [Players] -> Bool
colWin p xs 
    | head xs == p && xs!!3 == p && xs!!6 == p    = True
    | xs!!1 == p && xs!!4 == p && xs!!7 == p    = True
    | xs!!2 == p && xs!!5 == p && xs!!8 == p    = True
    |otherwise                                  = False
