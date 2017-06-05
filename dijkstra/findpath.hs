
-- data structures for our graph
data Node = String [Path] deriving (Show, Read, Eq)
data Path = Path { to :: Node, from :: Node, weight :: Float } deriving (Show, Read, Eq)

type Graph = [Node]

-- main loop
main = do
        content <- getContents
        let paths = [ x | x <- content, not $ isgarbage x ]
            source = last $ init paths
            target = last paths
        putStrLn $ show source
        putStrLn $ show target
        putStrLn $ show paths

-- check if a given character is useful
isgarbage x = if x == '\n' || x == ' ' then True else False
