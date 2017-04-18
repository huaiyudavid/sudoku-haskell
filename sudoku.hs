{-
    Names: Huaiyu Yang and Caleb Proffitt
    VUnet ids: yangh9 and 
    Emails: huaiyu.yang@vanderbilt.edu and 
    Class: CS3270
    Date: 4/17/2017
    Honor Statement: We did not give nor receive aid on this assignment.

    Description: A program to solve a sudoku puzzle using recursive backtracking search.
-}

import Text.Printf
import Data.Time
import Data.Sequence

{-
    main
    The main entry point of the program. 
    Handles user input and orchestrates the solving process.
-}
main = do 
    putStrLn "Please input file name of board:"
    filename <- getLine
    start <- getCurrentTime
    file <- readFile filename
    let board = getBoardFromFile file
    printBoard board
    end <- getCurrentTime
    putStrLn "Time Elapsed:"
    print (diffUTCTime end start)

{-
    getBoardFromFile
    Loads a board as a Sequence of Sequences from a given file.
-}
getBoardFromFile :: String -> Seq (Seq Int)
getBoardFromFile file = fromList $ map fromList $ map (map read) $ map words $ lines file

{-
    printBoard
    Prints a board.
-}
printBoard :: Seq (Seq Int) -> IO ()
printBoard board = do
    printRow (index board 0)
    printRow (index board 1)
    printRow (index board 2)
    putStrLn "------+-------+------"
    printRow (index board 3)
    printRow (index board 4)
    printRow (index board 5)
    putStrLn "------+-------+------"
    printRow (index board 6)
    printRow (index board 7)
    printRow (index board 8)

{-
    printRow
    Prints a row of the board with dividing lines.
-}
printRow :: Seq Int -> IO ()
printRow row = printf "%d %d %d | %d %d %d | %d %d %d \n"
           (index row 0)
           (index row 1)
           (index row 2)
           (index row 3)
           (index row 4)
           (index row 5)
           (index row 6)
           (index row 7)
           (index row 8)

{-
    getValue
    Return the value on the board at the specified row & col.
-}
getValue :: Seq (Seq Int) -> Int -> Int -> Int
getValue board row col = index (index board row) col

{-
    setValue
    Place a given value in the specified row & col and return
    the new board.
-}
setValue :: Seq (Seq Int) -> Int -> Int -> Int -> Seq (Seq Int)
setValue board row col val = update row (update col val (index board row)) board

