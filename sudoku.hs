import Text.Printf
import Data.Time

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

getBoardFromFile :: String -> [[Int]]
getBoardFromFile file = map (map read) $ map words $ lines file

printBoard :: [[Int]] -> IO ()
printBoard board = do
    printRow (board!!0)
    printRow (board!!1)
    printRow (board!!2)
    putStrLn "------+-------+------"
    printRow (board!!3)
    printRow (board!!4)
    printRow (board!!5)
    putStrLn "------+-------+------"
    printRow (board!!6)
    printRow (board!!7)
    printRow (board!!8)

printRow :: [Int] -> IO ()
printRow row = printf "%d %d %d | %d %d %d | %d %d %d \n"
           (row!!0)
           (row!!1)
           (row!!2)
           (row!!3)
           (row!!4)
           (row!!5)
           (row!!6)
           (row!!7)
           (row!!8)

getValue :: [[Int]] -> Int -> Int -> Int
getValue board row col = (board!!row)!!col

--setValue :: [[Int]] -> [[Int]]
--not sure how to do this yet