{-
    Names: Huaiyu Yang and Caleb Proffitt
    VUnet ids: yangh9 and proffich
    Emails: huaiyu.yang@vanderbilt.edu and caleb.h.proffitt@vanderbilt.edu
    Class: CS3270
    Date: 4/17/2017
    Honor Statement: We did not give nor receive aid on this assignment.

    Description: A program to solve a sudoku puzzle using recursive backtracking search.
-}

import Prelude hiding (take, drop, null)
import Text.Printf
import Data.Time
import Data.Sequence
import Data.Foldable hiding (null)

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
    let solved = solve board 0 0 1
    if null solved then putStrLn "Board is unsolvable." else printBoard solved
    end <- getCurrentTime
    putStrLn "Time Elapsed:"
    print (diffUTCTime end start)

{-
    solve
    Returns the solved board if possible, otherwise an empty sequence
-}
solve :: Seq (Seq Int) -> Int -> Int -> Int-> Seq (Seq Int)
solve board x y num
    | (null board) = board
    | (y > 8) = board
    | (x > 8) = (solve board 0 (y+1) 1)
    | (num > 9) = empty
    | ((getValue board y x) > 0) = (solve board (x+1) y 1)
    | (not (valid board x y num)) = (solve board x y (num + 1))
    | otherwise = (let s = (solve (setValue board y x num) x y 1)
        in (if (null s) then (solve board x y (num + 1)) else (s)))
{-
    valid
    Returns true iff the square, row, col are all valid
-}
valid :: Seq (Seq Int) -> Int -> Int -> Int -> Bool
valid board x y num = (rowValid board y num) && (colValid board x num) && (squareValid board (sqI x) (sqI y) num)

{-
    squareValid
    Returns true iff num does not exist in the square containing
        the specified spot (x, y)
-}
squareValid :: Seq (Seq Int) -> Int -> Int -> Int -> Bool
squareValid board x y num = not (elem num (getSquare board x y))

sqI :: Int -> Int
sqI i = (quot i 3) * 3

{-
    rowValid
    Returns true iff num does not exist in row 'row' of board
-}
rowValid :: Seq (Seq Int) -> Int -> Int -> Bool
rowValid board row num = not (elem num (getRow board row))

{-
    colValid
    Returns true iff num does not exist in column 'col' of board
-}
colValid :: Seq (Seq Int) -> Int -> Int -> Bool
colValid board col num = not (elem num (getCol board col))


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
    getSquare
    Return the 3x3 square with (x,y) as the top left (inclusive)
-}
getSquare :: Seq (Seq Int) -> Int -> Int -> Seq Int
getSquare board x y = (take 3 (drop x (getRow board y))) >< (take 3 (drop x (getRow board (y + 1)))) >< (take 3 (drop x (getRow board (y + 2))))

{-
    getRow
    Return the specified row
-}
getRow :: Seq (Seq Int) -> Int -> Seq Int
getRow board row = index board row

{-
    getCol
    Return the specified column
-}
getCol :: Seq (Seq Int) -> Int -> Seq Int
getCol board col = fromList (map (`index` col) (toList board))

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

