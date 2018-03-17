module HERBInterpreter where
import System.IO
import System.Environment
import Control.Monad
import HERBGrammar
import HERBTokens
import Text.ParserCombinators.Parsec
import Data.List
import Data.Typeable
import Data.Maybe
import Data.Ord

{-==============================================================================-}
{-=============================== DATA STRUCTURES ==============================-}
{-==============================================================================-}

-- Left hand side of algebra (variables or arguments)
-- VARIABLES TREE
data VarTree = CommaNode (VarNode) (VarTree) --Added to force tree structure to right recurse
    | SingleNode (VarNode)  -- VarNode location actualData
    | EmptyVT (EmptyTree)
    deriving Show
    
data VarNode = Vari (String) (String) (String) -- loc dat name
    deriving Show

type OrderedVars = IndVars [(Int, VarNode)] 
    deriving Show --To make tree readability easier

-- Right hand side of algebra (query or request)
-- OPERATORS TREE
data OpTree = ConjunctionNode (OpTree) (OpTree)
    | RelationNode (String) (VarTree) -- String is Table name.
    | EquateNode (OpTree) (OpTree)
    | LSubNode (OpTree) (OpTree)
    | RSubNode (OpTree) (OpTree)
    | BoolNode (Bool)
    | VarOp (VarTree)
    | EmptyOT (EmptyTree)
    deriving Show

-- Seperate exisitential operator.
-- EXISITENTIAL TREE ***TODO** add case for nested eTree
data ExistTree = ExistVar (VarTree) (OpTree) 
    | EmptyET (EmptyTree)
    deriving Show

-- All expressions available within language.
-- PARSE TREE
data ParseTree = Marker (OrderedVars) (OpTree)
    | MarkerNested (OrderedVars) (ExistTree)
    | MarkerExtended (OrderedVars) (ExistTree) (OpTree)
    | EmptyPT (EmptyTree)
    deriving Show
  --  (1,10,3)E.( ( (1,2)E.Q(x1,x2) ^ (x1 = x2) ) ^ (x3=foo) )

data EmptyTree = Nothing
    deriving Show

type Cell = String
    deriving Show

type Column = [Cell]
    deriving Show

type Row = [Cell]
    deriving Show

type Table = [Columns] | [Rows] | [[Cell]]
    deriving Show


{-==============================================================================-}
{-===================================== MAIN ===================================-}
{-==============================================================================-}
main :: IO()
main = do 
    a <- getArgs
    b <- readFile (head a)
    let content = head (splitAt "\n" b)
    let alex = alexScanTokens (content)
    let happy = parseCalc (alex)

    tree <- buildParseTree (happy)
    stack <- traverseDF(tree)
    tableNames <- extractTableNames (liftRelationNodesOut(stack))
    tableData <- crossProductMulti(buildTables (tableNames))
    answer <- executeHERB (stack) (tableData)
    prettyPrint (answer) -- answer contains all false rows as well as true. Just output true.

{-==============================================================================-}
{-=============================== CSV EXTRACTION ===============================-}
{-==============================================================================-}

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

buildTables :: [String] -> [Table] 
buildTables (x:xs) | (buildTable (x ++ ".csv")) : buildTables (xs)

buildTable :: String -> Table
buildTable tableName = parseCSV(readFile tableName)

parseCSV :: String -> Table
parseCSV input = parse csvFile "(unknown)" input

{-==============================================================================-}
{-============================== TABLE OPERATIONS ==============================-}
{-==============================================================================-}

countColumns :: [Column] -> Int
countColumns input = countColumnsFromRows (transpose input)

countColumnsFromRows :: [Row] -> Int
countColumnsFromRows (x:xs) = length x

countRows :: [Row] -> Int
countRows input = countRowsFromColumns (transpose input)

countRowsFromColumns :: [Column] -> Int
countRowsFromColumns (x:xs) = length x

getNthEl :: [a] -> Int -> Int -> a
getNthEl [] _ _ = []
getNthEl (x:xs) goal current | goal == current = x
                             | goal /= current = getNthEl xs goal (current+1)

crossProductMulti :: [Table] -> Table
crossProductMulti [] = []
crossProductMulti (x:y:xs) = crossProductMulti([crossProductTwo (x) (y)) : xs)

-- input columns, transpose will change to rows
crossProductTwo :: [Column] -> [Column] -> Table
crossProductTwo [] [] = []
crossProduct xs ys = crossProduct' [(x,y) | x <- (transpose xs), y <- (transpose ys)] 

crossProduct' :: [(Column,Column)] -> Table
crossProduct' xs ys = xs : ys

orderOutput :: [VarNode] -> [[VarNode]] -> [[VarNode]]
orderOutput (o:os) (list) = orderOutput' o (list) : orderOutput os (list)

--              LHS ORDER      filterTrueOutput            TRUE ROWS
orderOutput' :: VarNode -> [[VarNode]] -> [VarNode]
orderOutput o [] = []
orderOutput o (t:ts) = orderOutput'' (o) (t) : orderOutput' (o) (ts)

orderOutput'' :: VarNode -> [VarNode] -> [VarNode]
orderOutput'' v (w:ws) | equateNodeNames v w == True = w : orderOutput'' v ws
orderOutput'' v (w:ws) | equateNodeNames v w == False = orderOutput'' v ws

filterTrue :: [(Bool, [VarNode])] -> [[VarNode]]
filterTrue ((_, [])) = [[]]
filterTrue ((bool, vars):xs) | bool == True = vars : filterTrue xs
filterTrue ((bool, vars):xs) | bool == False = filterTrue xs

{-==============================================================================-}
{-=========================== TREE & NODE OPERATIONS ===========================-}
{-==============================================================================-}

-- *** TODO ** IMPORTANT: Implement a rule ensuring the children of an equality is 2 var nodes. Do we need to do this in our grammar/tree? See next commenr
checkEquality :: OpTree -> Bool
checkEquality (EquateNode (l) (r)) = equateNodes left right
    where   left = convertOpToVarNode (l)
            right = convertOpToVarNode (r)

equateNodes :: VarNode -> VarNode -> Bool
equateNodes (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))   | datA == datB = True
                                                                        | datA /= datB = False

equateNodesName :: VarNode -> VarNode -> Bool
equateNodesName (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))   | nameA == nameB = True
                                                                            | nameA /= nameB = False


isTreeAssigned :: VarTree -> Bool
isTreeAssigned (SingleNode vNode) = isNodeAssigned vNode
isTreeAssigned (CommaNode vNode remTree) = isNodeAssinged vNode && isTreeAssigned remTree 

isNodeAssigned :: VarNode -> Bool
isNodeAssigned (Vari (loc) (dat) (name))    | loc == "*"  = False-- Represents unassiged null value 
                                            | otherwise = True
                                            
{-==============================================================================-}
{-=============================== TREE TRAVERSAL ===============================-}
{-==============================================================================-}

traverseDF :: ParseTree -> [a]
traverseDF (EmptyPT) = []
traverseDF (Marker list op) = Marker : traverseDF list : traverseDF op
traverseDF (Marker list exisit) = Marker : traverseDF list : traverseDF exisit
traverseDF (Marker list exisit op) = Marker : traverseDF list : traverseDF exisit : traverseDF op

traverseDFOp :: OpTree -> [a]
traverseDFOp (EmptyOT) = []
traverseDFOp (ConjunctionNode left right) = traverseDFOp left : traverseDFOp right
traverseDFOp (RelationNode string variables) = string ++ "(" : traverseDFVar : ")"
traverseDFOp (EquateNode left right) = traverseDFOp left : traverseDFOp right
traverseDFOp (LSubNode left right) = traverseDFOp left : traverseDFOp right
traverseDFOp (RSubNode left right) = traverseDFOp left : traverseDFOp right
traverseDFOp (BoolNode f) = toString f
    
traverseDFEx :: ExistTree -> [a]
traverseDFEx (EmptyET) = []
traverseDFEx (ExistVar var op) = traverseDFVar var : traverseDFOp op

traverseDFVar :: VarTree -> [VarNode] --Int represents ORDER (NB: this is why I decided to add VarNode)
traverseDFVar (SingleNode (node) )  = (treeToNode (SingleNode (node)))  : []
traverseDFVar (CommaNode (nextVar) (remainingTree)) = nextVar : traverseDFVar remainingTree

{-========================== HANDLING FOR TREES ================================-}

treeToNode :: VarTree -> VarNode
treeToNode (SingleNode (Vari (loc) (dat) (name))) = Vari (loc) (dat) (name)
treeToNode (CommaNode (node) (remainingTree)) = error "Variable tree contains multiple nodes. Cannot convert to single node."--Unsure of error notation or if this will work but throw an error here (***TODO***)

toIndexedList :: (VarTree) -> [(Int, VarNode)]
toIndexedList lst = zip [1..] traverseDFVar(lst)

-- Blindly assumes OpTree contains a VarTree containing only one VarNode.
convertOpToVarNode :: OpTree -> VarNode
convertOpToVarNode VarOp (varTree) = treeToNode (varTree)


