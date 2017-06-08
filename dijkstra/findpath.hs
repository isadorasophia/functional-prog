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

data Cost = Cost { idf :: String, cost :: Float, bgraph :: Bool} deriving (Show, Read, Eq)
data Parent = Parent { child :: String, parent :: String } deriving (Show, Read, Eq)

type Costs = [Cost]
type Parents = [Parent]

main = do
        input <- getContents
        let content = lines input
            source = last $ init content
            target = last content
            graph = insertIfNull target $ convertGraph $ readEdges content
            tuple = dijkstra graph ([], (updateCost source 0 (initCost graph)))

        putStrLn $ show source
        putStrLn $ show target
        putStrLn $ show graph
        putStrLn $ show tuple

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

-- retrieve id of minimal cost node
getMinNode :: Costs -> String
getMinNode costs = snd ( foldl (\ (min, string) c -> if and [or [(min > cost c), (min == -1)], bgraph c] then (cost c, idf c) else (min, string)) (-1, []) costs )

-- add new parent to the list if it doesn´t exists and replace if it exists
addParent :: String -> String -> Parents -> Parents
addParent c p [] = [(Parent c p)]
addParent c p ((Parent child parent):r) = if child == c then ((Parent c p):r)
                                          else ((Parent child parent):(addParent c p r))

-- update cost of a given node, creating new cost if node isn't there
updateCost :: String -> Float -> Costs -> Costs
updateCost i nc [] = [(Cost i nc True)]
updateCost i nc (c:r) =  if idf c == i then 
                                        ((Cost i nc (bgraph c)):r)
                                    else
                                        (c:(updateCost i nc r))
initCost :: Graph -> Costs
initCost graph = foldl (\ costs x -> updateCost (key x) (-1) costs) [] graph


-- returns cost of a given id
findCost :: String -> Costs -> Float
findCost _ [] = -1
findCost s (c:r) = if idf c == s then cost c
                                else findCost s r

-- returns the node of a given id
retrieveNode :: String -> Graph -> Node
retrieveNode target [] = (Node target [])
retrieveNode target (h:r) = if key h == target then h else retrieveNode target r

-- removes node from graph
removeNode :: Node -> Graph -> Graph
removeNode _ [] = []
removeNode (Node n i) ((Node k e):r) = if n == k then r else ((Node k e):(removeNode (Node n i) r))

-- dijkstra's algorithm
dijkstra :: Graph -> (Parents, Costs) -> (Parents, Costs)
dijkstra [] (parents, costs) = (parents, costs)
dijkstra graph (parents, costs) = 
                    dijkstra (removeNode (minNode) graph) (iterateEdges minNode parents (setBoolCost minCost False costs))
                    where 
                        minCost = (getMinNode costs)
                        minNode = retrieveNode minCost graph


-- iterate throught all edges updating the values of the costs and parents of edge's destination
iterateEdges :: Node -> Parents -> Costs -> (Parents, Costs)
iterateEdges (Node _ []) parents costs = (parents, costs)
iterateEdges (Node key ((Edge s d w):r)) parents costs  
    | and [or [destCost > sourceCost + w, destCost == -1], sourceCost /= -1] =
            iterateEdges (Node key r) (addParent d s parents) (updateCost d (sourceCost + w) costs)
    | otherwise = iterateEdges (Node key r) parents costs

    where destCost = findCost d costs
          sourceCost = findCost key costs

-- set boolean value that tells if that id is still on the graph or not
setBoolCost :: String -> Bool -> Costs -> Costs
setBoolCost i b [] = []
setBoolCost i b (c:r) =  if idf c == i then 
                            ((Cost i (cost c) b):r)
                        else
                            (c:(setBoolCost i b r))
