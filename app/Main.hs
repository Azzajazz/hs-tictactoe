module Main where

import Control.Concurrent (threadDelay)

import Board
import Player

main :: IO ()
main = do
    putStrLn "Welcome to TicTacToe!\n"
    putStrLn "X starts.\n"
    (winner, board) <- playGame emptyBoard P1
    case winner of
        Nothing -> putStrLn "It's a draw!"
        Just player -> putStrLn ((show player) ++ " wins!")
    print board

playGame :: Board -> Player -> IO (Maybe Player, Board)
playGame board player = do
    sqr <- getUserInput board
    let newBoard = writeSquare sqr pSqr board
    if isRowFull pSqr newBoard ||
       isColFull pSqr newBoard ||
       isDiagFull pSqr newBoard then
        return (Just player, newBoard)
    else if isFull newBoard then
        return (Nothing, newBoard)
    else do
        let nextPlayer = otherPlayer player
        playGame newBoard nextPlayer
    where
        pSqr = playerSquare player
    

getUserInput :: Board -> IO Int
getUserInput board = do
    print board
    putStrLn "Which square do you want to take? (0-8): "
    input <- getLine
    if length input /= 1 || head input < '0' || head input > '8' then do
        putStrLn "Not a valid input."
        threadDelay 1000000
        getUserInput board
    else do
        let sqr = fromEnum (head input) - fromEnum '0'
        if not (isSquareEmpty sqr board) then do
            putStrLn "That square is already taken!"
            threadDelay 1000000
            getUserInput board
        else
            return sqr