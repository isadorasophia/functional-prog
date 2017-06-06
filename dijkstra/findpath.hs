--
--  dijkstra implementation!
--      written by: Isadora Sophia Garcia Rodopoulos       158018
--                  Matheus Mortatti Diamantino            156740
--
-- --------------------------------------------------------------

-- mocking and testing functions:
-- let edges = readEdges ["d c 343.3","d c 34.3","a d 34.3","d","s"]
-- let graph = convertGraph edges

-- data structures for our graph
data Edge = Edge { from :: String, to :: String, weight :: Float } deriving (Show, Read, Eq)
data Node = Node { key :: String, edges :: [Edge] } deriving (Show, Read, Eq)
type Graph = [Node]

main = do
        input <- getContents
        let content = lines input
            source = last $ init content
            target = last content
            graph = insertIfNull target $ convertGraph $ readEdges content

        putStrLn $ show source
        putStrLn $ show target
        putStrLn $ show graph

-- #######       helper functions        ####### --

initN :: Int -> [String] -> [String]
initN n p = if n > 0 then initN (n-1) $ init p
                     else p

-- #######     data parsing and blabla   ####### --

readEdges :: [String] -> [Edge]
readEdges input = map (\[a, b, w] -> Edge a b (read w :: Float)) content 
                    where content = map (words) $ initN 2 $ input

-- ####### where the real work gets done ####### --

-- convert input from user to a graph
convertGraph :: [Edge] -> Graph
convertGraph [] = []
convertGraph (e:r) = (extractAdj source r:convertGraph (removeEdgesFromSource source r))
                        where source = (Node (from e) [e])

-- extract all edges that are adjacent to a node and add it as a new edge from node
extractAdj :: Node -> [Edge] -> Node
extractAdj node [] = node
extractAdj node (e:r) = let updateEdges = if key node == from e then (e:edges node) else edges node
                        in extractAdj (Node (key node) updateEdges) r

-- remove all the edges that have NODE as source
removeEdgesFromSource :: Node -> [Edge] -> [Edge]
removeEdgesFromSource node [] = []
removeEdgesFromSource node (e:r) = if key node == from e then rest else e:rest
                                    where rest = removeEdgesFromSource node r

-- insert node value if it doesnt exist in graph
insertIfNull :: String -> Graph -> Graph
insertIfNull target [] = [Node target []]
insertIfNull target (h:r) = if key h == target then (h:r) else h:insertIfNull target r

-- retrieve a given node (return Nothing if it doesn't exists)
retrieve :: String -> Graph -> Maybe Node
retrieve _ [] = Nothing
retrieve target (h:r) = if key h == target then Just h else retrieve target r

-- deletes an Edge from the array
deleteEdge :: Edge -> [Edge] -> [Edge]
deleteEdge _ [] = []
deleteEdge target (h:r) = if h == target then r else h:deleteEdge target r

-- WAKE ME UP INSIDEEEEEEEEEEEE
