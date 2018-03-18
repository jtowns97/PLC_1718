module HERBInterpreter where
import System.IO
import System.Environment
import Control.Monad
import HERBGrammar
import HERBTokens
import Text.ParserCombinators.Parsec
import Data.List
import Data.List.Split
import Data.Typeable
import Data.Maybe
import Data.Ord

{-==============================================================================-}
{-=============================== DATA STRUCTURES ==============================-}

-- Left hand side of algebra (variables or arguments)
-- VARIABLES TREE
data VarTree = CommaNode (VarNode) (VarTree) --Added to force tree structure to right recurse
    | SingleNode (VarNode)  -- VarNode location actualData
    | EmptyVT (EmptyTree)
    deriving Show
    
data VarNode = Vari (String) (String) (String) -- loc dat name
    deriving Show

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
    | ExistNest (VarTree) (ExistTree)
    | EmptyET (EmptyTree)
    deriving Show

-- All expressions available within language.
-- PARSE TREE
data ParseTree = Marker ([VarNode]) (OpTree)
    | MarkerNested ([VarNode]) (ExistTree)
    | MarkerExtended ([VarNode]) (ExistTree) (OpTree)
    | EmptyPT (EmptyTree)
    deriving Show
  --  (1,10,3)E.( ( (1,2)E.Q(x1,x2) ^ (x1 = x2) ) ^ (x3=foo) )

data EmptyTree = Nothing
    deriving Show

{-==============================================================================-}
{-===================================== MAIN ===================================-}
{-==============================================================================-}
main :: IO()
main = do 
    a <- getArgs
    b <- readFile (head a)
    let content = head (splitOn "\n" b)
    let alex = alexScanTokens (content)

    stack <- liftTraverseDF(alex)
    tableNames <- liftExtractTableNames(stack)
    tableData <- liftCrossProductMulti(tableNames)
    answer <- liftExecuteHERB (stack) (tableData)
    liftPrettyPrint (answer) -- answer contains all false rows as well as true. Just output true.



{-==============================================================================-}
{-============================== LIFTING TO MONADS =============================-}
{-==============================================================================-}

<<<<<<< HEAD
<<<<<<< HEAD
--liftBPT :: Exp -> IO ParseTree
--liftBPT exp = liftM (buildParseTree (exp))

--liftTraverseDF :: 
=======
liftBuildParseTree :: Exp -> IO ParseTree
liftBuildParseTree exp = liftM (buildParseTree (exp))
=======
--BPT
liftBuildParseTree :: [Token] -> ParseTree
liftBuildParseTree alex = liftM buildParseTree parseCalc(alex)
>>>>>>> elliott

liftTraverseDF :: [Token] -> [OpTree]
liftTraverseDF alex = liftM traverseDF liftBuildParseTree(alex)

liftExtractTableNames :: [OpTree] -> [String]
liftExtractTableNames stack = liftM extractTableNames liftRelationNodesOut(stack)

liftCrossProduct :: [String] -> [[String]]
liftCrossProduct tableNames = liftM crossProd buildTables(tableNames)

liftExecuteHERB :: [a] -> [[String]] -> IO String
liftExecuteHERB stack tableData = liftM(executeHERB (stack) (tableData))

liftPrettyPrint :: String -> IO String
<<<<<<< HEAD
liftPrettyPrint answer = liftM2(prettyPrint (answer))
>>>>>>> 51a71e24b38c94f46986d0e443f4ec85c70f9de3
=======
liftPrettyPrint answer = liftM2 prettyPrint answer
>>>>>>> elliott

{-==============================================================================-}
{-================================= BUILDING ===================================-}
{-==============================================================================-}

buildParseTree :: Exp -> ParseTree
buildParseTree (Evaluate (vars) (query)) = Marker (traverseDFVar(buildVarTree(vars))) (buildOpTree(query))
buildParseTree (Eval vars exis) = MarkerNested (traverseDFVar(buildVarTree(vars))) (buildExisTree(exis))
buildParseTree (EvalExisExt vars exis quer) = MarkerExtended (traverseDFVar(buildVarTree(vars))) (buildExisTree(exis)) (buildOpTree(quer))

buildVarTree :: Variables -> VarTree
buildVarTree (VarSingle strName) = SingleNode (Vari ("*") ("*") (strName))
buildVarTree (Comma (VarSingle (nextStrName)) remVars) = CommaNode (Vari ("*") ("*") (nextStrName)) (buildVarTree remVars)

buildExisTree :: Existential -> ExistTree 
buildExisTree (ExistentialSingle (vars) (quer)) = (ExistVar (buildVarTree(vars)) (buildOpTree (quer)))
buildExisTree (ExistentialNested (vars) (exisNest)) = (ExistNest (buildVarTree(vars)) (buildExisTree (exisNest)))

buildOpTree :: Query -> OpTree 
buildOpTree (Conjunction querA querB) = ConjunctionNode (buildOpTree querA) (buildOpTree querB)
buildOpTree (Relation tblN varis) = RelationNode (tblN) (assignVarTreeLoc (buildVarTree varis) (tblN) )
--Below (TODO) add type checker for querA/B, checking its a VarTree
buildOpTree (Equality querA querB) = (EquateNode (varToOpTree(buildVarTree(queryToVariables(querA)))) (varToOpTree(buildVarTree(queryToVariables(querB)))))
buildOpTree (V varis) = VarOp (buildVarTree(varis))
buildOpTree _ = EmptyOT (HERBInterpreter.Nothing)

{-==============================================================================-}
{-=============================== CSV EXTRACTION ===============================-}
{-==============================================================================-}

csvFile = Text.ParserCombinators.Parsec.endBy line eol
line = Text.ParserCombinators.Parsec.sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

buildTables :: [String] -> [[[String]]] 
buildTables (x:xs) = (buildTable (x ++ ".csv")) : buildTables (xs)

buildTable :: FilePath-> [[String]]
buildTable tableName = parseCSV(readFile tableName)

parseCSV :: IO String -> [[String]]
parseCSV input = parse csvFile "(unknown)" input

{-==============================================================================-}
{-============================== TABLE OPERATIONS ==============================-}
{-==============================================================================-}

colToRows :: [[String]] -> [[String]]
colToRows input = transpose input

countCols :: [[String]] -> Int
countCols input = countColsFromRows (transpose input)

countColsFromRows :: [[String]] -> Int
countColsFromRows (x:xs) = length x

countRows :: [[String]] -> Int
countRows input = countRowsFromCols (transpose input)

countRowsFromCols :: [[String]] -> Int
countRowsFromCols (x:xs) = length x

getNthEl :: [[String]] -> Int -> Int -> [String]
getNthEl [] _ _ = []
getNthEl (x:xs) goal current | goal == current = x
                             | goal /= current = getNthEl xs goal (current+1)

--INPUT ROWS                            
getNthRow :: [[String]] -> Int -> Int -> [String]
getNthRow [] _ _ = []
getNthRow (x:xs) goal current | goal == current = x
                             | goal /= current = getNthRow xs goal (current+1)

--INPUT ROWS
getNRowFromCrossProd :: [[String]] -> Int -> [String]
getNRowFromCrossProd table goalRow = getNthRow table  goalRow 0

-- input list of tables, returns tables of rows
crossProd :: [[[String]]] -> [[[String]]]
crossProd = foldr
    (\xs as ->
        [ x : a
        | x <- xs
        , a <- as ])
    [[]]

orderOutput :: [VarNode] -> [[VarNode]] -> [VarNode]
orderOutput (o:os) (list) = orderOutput' o (list) ++ orderOutput os (list)

--              LHS ORDER      filterTrueOutput            TRUE ROWS
orderOutput' :: VarNode -> [[VarNode]] -> [VarNode]
orderOutput' o [] = []
orderOutput' o (t:ts) = orderOutput'' (o) (t) ++ orderOutput' (o) (ts)

orderOutput'' :: VarNode -> [VarNode] -> [VarNode]
orderOutput'' v (w:ws) | equateNodesName v w == True = w : orderOutput'' v ws
orderOutput'' v (w:ws) | equateNodesName v w == False = orderOutput'' v ws

filterTrue :: [(Bool, [VarNode])] -> [[VarNode]]
filterTrue ((bool, vars):xs) | length xs == 0 = [[]]
filterTrue ((bool, vars):xs) | bool == True = vars : filterTrue xs
filterTrue ((bool, vars):xs) | bool == False = filterTrue xs

{-==============================================================================-}
{-=========================== TREE & NODE OPERATIONS ===========================-}
{-==============================================================================-}

queryToVariables :: Query -> Variables
queryToVariables (V (Comma varA varB))  = (Comma varA varB)
queryToVariables (V (VarSingle string)) = (VarSingle string)

varToOpTree :: VarTree -> OpTree
varToOpTree (CommaNode varN varT) = (VarOp (CommaNode varN varT))
varToOpTree (SingleNode varN) = (VarOp (SingleNode varN))
varToOpTree (EmptyVT emptyT) = (VarOp (EmptyVT emptyT))

countVarNodes :: VarTree -> Int
countVarNodes (CommaNode varN varTree) = 1 + countVarNodes varTree
countVarNodes (SingleNode varN) = 1
countVarNodes (EmptyVT empty) = 0

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


extractTableNames :: [OpTree] -> [String] -- takes output from liftRelationNodesOut, possibly needs to be reverse
extractTableNames [] = []
extractTableNames ( (RelationNode (tbl) (vTree)) :xs) = (tbl) : extractTableNames xs

assignVarTreeLoc :: VarTree -> String -> VarTree
assignVarTreeLoc (SingleNode (Vari (loc) (dat) (name))) x = (SingleNode (Vari (x) (dat) (name)))
assignVarTreeLoc (CommaNode (Vari (loc) (dat) (name)) (remTree)) x = (CommaNode (Vari (x) (dat) (name)) (assignVarTreeLoc (remTree) (x)))

assignAssignment :: OpTree -> String -> OpTree
assignAssignment (RelationNode (ass) (vTree) ) newAss = (RelationNode (newAss) (vTree))

assignVarNodeVal :: VarNode -> String -> VarNode
assignVarNodeVal (Vari (loc) (dat) (name)) newDat = (Vari (loc) (newDat) (name))

assignRelation :: OpTree -> String -> OpTree --Will only assign if not signed before. If loc already exists then....what??
assignRelation (RelationNode (tbl) (vTree)) relName | isTreeAssigned (vTree) == False = RelationNode (tbl) (assignVarTreeLoc (vTree) (tbl))
                                                    | otherwise = (RelationNode (tbl) (vTree)) --check node equality here

isTreeAssigned :: VarTree -> Bool
isTreeAssigned (SingleNode vNode) = isNodeAssigned vNode
isTreeAssigned (CommaNode vNode remTree) = isNodeAssigned vNode && isTreeAssigned remTree 

isNodeAssigned :: VarNode -> Bool
isNodeAssigned (Vari (loc) (dat) (name))    | loc == "*"  = False-- Represents unassiged null value 
                                            | otherwise = True

liftRelationNodesOut :: OpTree -> [OpTree] --Creates list of single node OpTree's 
liftRelationNodesOut (RelationNode (tbl) (vTree)) = (RelationNode (tbl) (vTree) ) ++ liftRelationNodesOut xs 
liftRelationNodesOut  (x:xs) = liftRelationNodesOut xs ++ []
                                            
{-==============================================================================-}
{-=============================== TREE TRAVERSAL ===============================-}
{-==============================================================================-}

-- liftTraversal :: [VarTree] -> IO VarTree
-- liftTraversal [a] = liftM [a]

-- traverseDF :: ParseTree -> [VarTree]
-- traverseDF (EmptyPT e) = []
-- traverseDF (Marker list op) = Marker : traverseDF list : traverseDF op
-- traverseDF (Marker list exisit) = Marker : traverseDF list : traverseDF exisit
-- traverseDF (Marker list exisit op) = Marker : traverseDF list : traverseDF exisit : traverseDF op

-- traverseDFOp :: OpTree -> [AllTree]
-- traverseDFOp (EmptyOT e) = []
-- traverseDFOp (ConjunctionNode left right) = traverseDFOp left : traverseDFOp right
-- traverseDFOp (RelationNode string variables) = traverseDFVar
-- traverseDFOp (EquateNode left right) = traverseDFOp left : traverseDFOp right
-- traverseDFOp (LSubNode left right) = traverseDFOp left : traverseDFOp right
-- traverseDFOp (RSubNode left right) = traverseDFOp left : traverseDFOp right
-- traverseDFOp (BoolNode f) = toString f
    
<<<<<<< HEAD
traverseDFEx :: ExistTree -> [a]
traverseDFEx (EmptyET e) = []
traverseDFEx (ExistVar var op) = traverseDFVar var : traverseDFOp op
=======
-- traverseDFEx :: ExistTree -> [AllTree]
-- traverseDFEx (EmptyET e) = []
-- traverseDFEx (ExistVar var op) = traverseDFVar var ++ traverseDFOp op
>>>>>>> elliott

-- traverseDFVar :: VarTree -> [VarTree] --Int represents ORDER (NB: this is why I decided to add VarNode)
-- traverseDFVar (SingleNode (node) )  = [(treeToNode (SingleNode (node)))]  ++ []
-- traverseDFVar (CommaNode (nextVar) (remainingTree)) = [nextVar] ++ traverseDFVar remainingTree

{-========================== HANDLING FOR TREES ================================-}

treeToNode :: VarTree -> VarNode
treeToNode (SingleNode (Vari (loc) (dat) (name))) = Vari (loc) (dat) (name)
treeToNode (CommaNode (node) (remainingTree)) = error "Variable tree contains multiple nodes. Cannot convert to single node."--Unsure of error notation or if this will work but throw an error here (***TODO***)

<<<<<<< HEAD
--toIndexedList :: (VarTree) -> [(Int, VarNode)]
--toIndexedList lst = zip [1..] traverseDFVar(lst)

=======
>>>>>>> 51a71e24b38c94f46986d0e443f4ec85c70f9de3
-- Blindly assumes OpTree contains a VarTree containing only one VarNode.
convertOpToVarNode :: OpTree -> VarNode
convertOpToVarNode (VarOp (vTree)) = treeToNode (vTree)


