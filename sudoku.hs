import Text.Printf

main = do 
    putStrLn "Please input file name of board:"
    filename <- getLine
    file <- readFile filename
    let board = getBoardFromFile file
    printBoard board

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