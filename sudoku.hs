import Text.Printf
import Data.Time
import Data.Sequence

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

getBoardFromFile :: String -> Seq (Seq Int)
getBoardFromFile file = fromList $ map fromList $ map (map read) $ map words $ lines file

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

getValue :: Seq (Seq Int) -> Int -> Int -> Int
getValue board row col = index (index board row) col

setValue :: Seq (Seq Int) -> Int -> Int -> Int -> Seq (Seq Int)
setValue board row col val = update row (update col val (index board row)) board