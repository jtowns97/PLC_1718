-- module HERBInterpreter where
-- import System.IO
-- import System.Environment
-- import Control.Monad
-- import HERBGrammar
-- import HERBTokens
-- import Text.ParserCombinators.Parsec
-- import Data.List
-- import Data.Typeable

-- {-=============================== DATA STRUCTURES ==============================-}

-- -- Left hand side of algebra (variables or arguments)
-- -- VARIABLES TREE
-- data VarTree = CommaNode (VarNode) (VarTree) --Added to force tree structure to right recurse
--     | SingleNode (VarNode)  -- VarNode location actualData
--     | EmptyVT (EmptyTree)
--     deriving Show
    
-- data VarNode = Vari (String) (String) (String) -- loc dat name
--     deriving Show

-- -- Right hand side of algebra (query or request)
-- -- OPERATORS TREE
-- data OpTree = ConjunctionNode (OpTree) (OpTree)
--     | RelationNode (String) (VarTree) -- String is Table name.
--     | EquateNode (OpTree) (OpTree)
--     | LSubNode (OpTree) (OpTree)
--     | RSubNode (OpTree) (OpTree)
--     | BoolNode (Bool)
--     | VarOp (VarTree)
--     | EmptyOT (EmptyTree)
--     deriving Show

-- -- Seperate exisitential operator.
-- -- EXISITENTIAL TREE ***TODO** add case for nested eTree
-- data ExisitTree = ExisitVar (VarTree) (OpTree) 
--     | EmptyET (EmptyTree)
--     deriving Show

-- -- All expressions available within language.
-- -- PARSE TREE
-- data ParseTree = Marker (OrderedVars) (OpTree)
--     | MarkerNested (OrderedVars) (ExisitTree)
--     | MarkerExtended (OrderedVars) (ExisitTree) (OpTree)
--     | EmptyPT (EmptyTree)
--     deriving Show
--   --  (1,10,3)E.( ( (1,2)E.Q(x1,x2) ^ (x1 = x2) ) ^ (x3=foo) )

-- data EmptyTree = Nothing
--     deriving Show

-- type Cell = String
--     deriving Show

-- type Column = [Cell]
--     deriving Show

-- type Row = [Cell]
--     deriving Show

-- type Table = [Columns] | [Rows] | [[Cell]]
--     deriving Show

-- newtype OrderedVars = IndVars [(Int, [VarNode])] deriving Show --To make tree readability easier, ***TODO*** test


-- varToInt :: [Char] -> Int
-- varToInt (x:xs) = read xs

-- toString :: Bool -> String
-- toString bool = if bool then "True" else "False"

-- -- Read input from command line with IO monad.
-- main = do 
--     a <- getArgs
--     b <- readFile (head a)
--     let content = head (splitAt "\n" b)
--     let alex = alexScanTokens (content)
--     let happy = parseCalc (alex)

--     tree <- buildParseTree (happy)
--     stack <- traverseDF(tree)
--     tableNames <- extractTableNames (liftRelationNodesOut(stack))
--     tableData <- crossProduct(buildTables (tableNames))
--     answer <- executeHERB (stack) (tableData)
--     prettyPrint (answer) -- answer contains all false rows as well as true. Just output true.
    
-- readContents :: String -> IO [[String]]
-- readContents filepath = do
--  contents <- readFile filepath
--  let lines = chunksOf 1 (splitOn "\n" contents)
--  return lines

--getVarLocation      

executeHERB :: [a] -> Table -> [(Bool, Row)]
executeHERB parseTree [] = []
executeHERB parseTree (t:ts) = actuallyExecute(assignVariables(t)) : executeHERB(ts) 
{-=============================== CSV HANDLING ==============================-}
--A source: http://book.realworldhaskell.org/read/using-parsec.html

-- csvFile = endBy line eol
-- line = sepBy cell (char ',')
-- cell = many (noneOf ",\n")
-- eol = char '\n'

-- parseCSV :: String -> [[String]]
-- parseCSV input = parse csvFile "(unknown)" input

-- --extractCSVCol fileContents specifiedCol; returns (ColumnNumber, [all, instances, of, specified, column])
-- extractCSVCol :: [[String]] -> Int -> (Int, Column)
-- extractCSVCol [] _ = (-1, []) --Add error here
-- extractCSVCol (x:xs) ind = (ind, getNthEl x ind 1) ++ extractCSVCol xs ind


-- --Auxilliary function, basically a safe version of !!
-- getNthEl :: [a] -> Int -> Int -> a
-- getNthEl [] _ _ = []
-- getNthEl (x:xs) goal current | goal == current = x
--                                    | goal /= current = getNthEl xs goal (current+1)

-- --Return number of columns
-- countCSVCol :: [Column] -> Int
-- countCSVCol (x:xs) = length x

-- --Collate columns into one data structure
-- gatherCSVdata :: [Column] -> Int -> [(Int, [Column])]
-- gatherCSVdata inp count | count > colNum = []
--                         | count <= colNum = extractCSVCol inp count : gatherCSVdata inp (count+1)
--                         where
--                             colNum = countCSVCol inp

-- crossProduct :: [Column] -> [Column] -> Table
-- crossProduct xs ys = crossProduct' [(x,y) | x <- (transpose xs), y <- (transpose ys)] -- input columns, transpose will change to rows

-- -- crossProduct' :: [(Column,Column)] -> Table
-- crossProduct' xs ys = xs : ys

{-

Currently not working due to Either error wrt IO()

--Iterate through columns of CSV file, ensuring all data is collected. Throw error for empty file
getCSV :: [[String]] -> Either IO() a [(Int, [String])]
getCSV inp | inp == [] = Left( hPutStrLn stderr "Error: Missing CSV data" )
           | otherwise = Right( gatherCSVdata inp 1 )  
-}



{-=============================== OPERATOR CHECKING FUNCTIONS  ==============================-}
{-
methodToMaybeCreateAST ::

checkExistential:: soomething -> Bool
checkConjunction :: something -> Bool
x1,x2 |- P(x1) ^ Q(x2)
verifyFreeVars :: --check that all free vars are assigned to >= 1 tables/relations
checkOutputSequence :: --potentially not necessary
checkSubset :: ??? is this needed ? Dont think we use subsets.  
 ::
checkTableName ::
treeToStack :: -- R -> L , DF
--methodToGetDataFromActualCSVFile :: IO () -> String

-}

-- *** TODO *** ALSO REALLY IMPORTANT, see spec problem 2, (1 2 3 |- A(x1,x2)^B(x2,x3) we gotta have an equality check for both x2's
-- ALSO another point, does the order of the output matter? Spec says ordered lex'ally but idk if that means a specific order or just however its implemented
-- checkRelation :: OpTree -> [String] -> (Bool, [VarNode])
-- checkRelation (RelationNode (tblNme) (vTree)) thisRow | 

assignRelation :: OpTree -> [String] -> OpTree --Will only assign if not signed before. If loc already exists then....what??
assignRelation (RelationNode (tbl) (vTree)) (x:xs)  | isTreeAssigned (vTree) == False = assignVarTreeLoc (vTree) (tbl)
                                                    | otherwise = (RelationNode (tbl) (vTree)) --check node equality here

liftRelationNodesOut :: [OpTree] -> [OpTree] --Creates list of single node OpTree's 
liftRelationNodesOut ((RelationNode (tbl) (vTree)):xs) = (RelationNode (tbl) (vTree) ) : liftRelationNodesOut xs 
liftRelationNodesOut  (_:xs) = liftRelationNodesOut xs : []


extractTableNames :: [OpTree] -> [String] -- takes output from liftRelationNodesOut, possibly needs to be reversed
extractTableNames [] = []
extractTableNames ( (RelationNode (tbl) (vTree)) :xs) = (tbl) : extractTableNames xs


extractRelNodeLocation :: OpTree -> String
extractRelNodeLocation (RelationNode (thisLoc) (vTree)) = thisLoc

-- assignRelation :: OpTree -> [String] -> OpTree
-- assignRelation (RelationNode (tbl) (vTree)) (x:xs) = 

assignVarTreeLoc :: VarTree -> [String] -> VarTree
assignVarTreeLoc (SingleNode (Vari (loc) (dat) (name))) (x:xs) = (SingleNode (Vari (x) (dat) (name)))
assignVarTreeLoc (CommaNode (Vari (loc) (dat) (name)) (remTree)) (x:xs) = (CommaNode (Vari (x) (dat) (name)) (assignVarTreeLoc (remTree) (xs)))

assignAssignment :: OpTree -> String -> OpTree
assignAssignment (RelationNode (ass) (vTree) ) newAss = (RelationNode (newAss) (vTree))

assignVarNodeVal :: VarNode -> String -> VarNode
assignVarNodeVal (Vari (loc) (dat) (name)) newDat = (Vari (loc) (newDat) (name))


--doesRelationExist :: 

-- *** TODO *** : Non bounded var w/ Exis

--Below for indexed var list:

{-=============================== TREE HANDLING & TRAVERSAL ==============================-}

-- --TRAVERSAL--
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
    
traverseDFEx :: ExisitTree -> [a]
traverseDFEx (EmptyET) = []
traverseDFEx (ExisitVar var op) = traverseDFVar var : traverseDFOp op

traverseDFVar :: VarTree -> [VarNode] --Int represents ORDER (NB: this is why I decided to add VarNode)
traverseDFVar (SingleNode (node) )  = (treeToNode (SingleNode (node)))  : []
traverseDFVar (CommaNode (nextVar) (remainingTree)) = nextVar : traverseDFVar remainingTree
 

--updateNodeValue 

--Converts VarTree with one node associated with it to a VarNode; ***TODO: Test this function I have no idea if this works ***


-- *** TODO *** ALSO REALLY IMPORTANT, see spec problem 2, (1 2 3 |- A(x1,x2)^B(x2,x3) we gotta have an equality check for both x2's
-- ALSO another point, does the order of the output matter? Spec says ordered lex'ally but idk if that means a specific order or just however its implemented
--checkRelation :: OpTree -> Bool
--checkRelation RelationNode tbl (x:xs) | 

-- *** TODO *** : Non bounded var w/ Exis

{-=============================== MAIN EVALUATION ==============================-}

evaluateParseTree :: ParseTree a -> [String] -> (Bool, [VarNode])
evaluateParseTree (Marker ordVars oTree)
evaluateParseTree (MarkerNested eTree )

evaluate :: OpTree -> [String] -> (Bool, [VarNode])--evaluate opTree freeVarList
evaluate (EquateNode (l) (r)) freeVars =( ( checkEquality (EquateNode (l) (r)) ), freeVars )
evaluate (RelationNode (loc) (varTr)) freeVars = checkRelation ( (RelationNode (loc) (varTr)) freeVars )
evaluate (ConjunctionNode (l) (r)) freeVars = checkConjunction ( (ConjunctionNode (l) (r)) freeVars )
--evaluate (VarTree ) varRow = 
--evaluate (VarOp tree) freeVars = assignVars ((traverseDFVar (tree)) freeVars)
evaluate _ freeVars = (True, [(Vari ("loc") ("dat") ("col"))])

-- evaluateE :: ExisitTree -> Bool
-- evaluateE varTree opTree | (traverseDFVar (varTree)) --TODO

-- *** TODO ** IMPORTANT: Implement a rule ensuring the children of an equality is 2 var nodes. Do we need to do this in our grammar/tree? See next commenr
checkEquality :: OpTree -> Bool
checkEquality (EquateNode (l) (r)) = equateNodes left right
    where   left = convertOpToVarNode (l)
            right = convertOpToVarNode (r)

--Compares the data currently asigned to 2 different VarNodes. If they are the same, return true, else false.
-- *** TODO *** maybe implement type error here?
equateNodes :: VarNode -> VarNode -> Bool
equateNodes (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))   | datA == datB = True
                                                                        | datA /= datB = False

equateNodesName :: VarNode -> VarNode -> Bool
equateNodesName (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))   | nameA == nameB = True
                                                                        | nameA /= nameB = False

equateNodeNameMulti :: VarNode -> [VarNode] -> Bool
equateNodeNameMulti var (v:vs) = 
isTreeAssigned :: VarTree -> Bool
isTreeAssigned (SingleNode vNode) = isNodeAssigned vNode
isTreeAssigned (CommaNode vNode remTree) = isNodeAssinged vNode && isTreeAssigned remTree 

isNodeAssigned :: VarNode -> Bool
isNodeAssigned (Vari (loc) (dat) (name))    | loc == "*"  = False-- Represents unassiged null value 
                                            | otherwise = True

{-=============================== ERROR HANDLING ==============================-}

isInteger :: (Typeable a) => a -> Bool
isInteger n = typeOf n == typeOf 1

isString :: (Typeable a) => a -> Bool
isString n = typeOf n == typeOf "HERB"

isChar :: (Typeable a) => a -> Bool
isChar n = typeOf n == typeOf 'c'



{-================================================     Probably not gonna use:      ======================================== -}

-- assignFreeLocs :: OpTree -> [VarNode] -> [VarNode]
-- assignFreeLocs (RelationNode (thisLoc) (vTree)) ((Vari (locA) (datA) (nameA)):xs) =  (Vari (thisLoc) (datA) (nameA) ) : assignFreeLocs' vTree xs thisLoc

-- assignFreeLocs' :: VarTree -> [VarNode] -> String -> [VarNode]
-- assignFreeLocs' (SingleNode (Vari (locA) (datA) (nameA) ))  ( (Vari (locB) (datB) (nameB) ) :xs) tblName = (Vari (tblName) (datB) (nameA)) : xs : []
-- assignFreeLocs' (CommaNode (Vari (locNext) (datNext) (nameNext) ) (remTree)) ( (Vari (locB) (datB) (nameB) ) :xs) tblName = (Vari (tblName) (datB) (nameNext)) : assignFreeLocs' remTree xs tblName

-- doesNameExistInVar :: [VarNode] -> String -> Maybe VarNode
-- doesNameExistInVar [] targString = EmptyTree
-- doesNameExistInVar ((Vari (loc) (dat) (name)):xs) targString    | name == targString = (Vari (loc) (dat) (name))
--                                                                 | name /= targString = doesNameExistInVar xs 

-- findAllVarsMatchingName :: [VarNode] -> String -> Maybe [VarNode]
-- findAllVarsMatchingGame [] str = []
-- findAllVarsMatchingName ((Vari (loc) (dat) (name)) : xs) str    | name == str = (Vari (loc) (dat) (name)) :  findAllVarsMatchingName xs str
--                                                                 | name /= str = findAllVarsMatchingName xs str

--if name is identical and loc is different, check equality


--checkConjunction :: OpTree -> [VarNode] -> (Bool, [VarNode])

-- compareNodeNameLoc :: VarNode -> VarNode -> Bool
-- compareNodeNameLoc (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))    | (nameA == nameB) && (locA /= locB) = True
--                                                                                 | (nameA /= nameB) = False
