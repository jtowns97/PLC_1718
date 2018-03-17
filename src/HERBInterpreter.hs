module HERBInterpreter where
import System.IO
import System.Environment
import Control.Monad
import HERBGrammar
import HERBTokens
import Text.ParserCombinators.Parsec
import Data.List
import Data.Typeable

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
=======
    | VarOp VarTree
    | EmptyOT (EmptyTree)
>>>>>>> d8734f21bf1fe6327f69bed22a939f523bc4cb9d
    deriving Show

-- Seperate exisitential operator.
-- EXISITENTIAL TREE ***TODO** add case for nested eTree
data ExisitTree = ExisitVar (VarTree) (OpTree) 
    | EmptyET (EmptyTree)
    deriving Show

-- All expressions available within language.
-- PARSE TREE
data ParseTree = Marker (OrderedVars) (OpTree)
    | MarkerNested (OrderedVars) (ExisitTree)
    | MarkerExtended (OrderedVars) (ExisitTree) (OpTree)
    | EmptyPT (EmptyTree)
    deriving Show
  --  (1,10,3)E.( ( (1,2)E.Q(x1,x2) ^ (x1 = x2) ) ^ (x3=foo) )

data EmptyTree = Nothing
    deriving Show


-- Memory and location storage for interpreter.
type Location = Int
--type Map = [(Int, Value)]
type Stack = [Int]
newtype OrderedVars = IndVars [(Int, [VarNode])] deriving Show --To make tree readability easier, ***TODO*** test


varToInt :: [Char] -> Int
varToInt (x:xs) = read xs

toString :: Bool -> String
toString bool = if bool then "True" else "False"

-- Read input from command line with IO monad.
main :: IO()
main = do
    --FILE TO MAKE A LIST OF TABLES.
    tableA <- readFile "A.csv"
    tableB <- readFile "B.csv"
    filePath <- getArgs -- Take file path from command line.
    file <- readFile (head filePath) -- read file
    content <- (splitOn "\n" file)
    let alex = alexScanTokens (content)
    let happy = parseCalc (alex)
    --output <- (happy))

    --mapM_ putStrLn (c)


    --TEST: showing parse tree.
    putStrLn ("ParseTree: " ++ show(string))
    print "... tree parsed."

--getVarLocation                       





{-----------------------------------------JAMES FROM HERE ON-----------------------------------------------}

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
treeToStack :: -- R -> L , DFS
--methodToGetDataFromActualCSVFile :: IO () -> String





-}


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

-- *** TODO *** ALSO REALLY IMPORTANT, see spec problem 2, (1 2 3 |- A(x1,x2)^B(x2,x3) we gotta have an equality check for both x2's
-- ALSO another point, does the order of the output matter? Spec says ordered lex'ally but idk if that means a specific order or just however its implemented
-- checkRelation :: OpTree -> [String] -> (Bool, [VarNode])
-- checkRelation (RelationNode (tblNme) (vTree)) thisRow | 

<<<<<<< HEAD
assignRelation :: OpTree -> [String] -> OpTree --Will only assign if not signed before. If loc already exists then....what??
assignRelation (RelationNode (tbl) (vTree)) (x:xs)  | isTreeAssigned (vTree) == False = assignVarTreeLoc (vTree) (tbl)
                                                    | otherwise = (RelationNode (tbl) (vTree)) --check node equality here

liftRelationNodesOut :: [OpTree] -> [OpTree] --Creates list of single node OpTree's 
liftRelationNodesOut ((RelationNode (tbl) (vTree)):xs) = (RelationNode (tbl) (vTree) ) : liftRelationNodesOut xs 
liftRelationNodesOut  (_:xs) = liftRelationNodesOut xs : []


extractTableNames :: [OpTree] -> [String] -- takes output from liftRelationNodesOut, possibly needs to be reversed
extractTableNames [] = []
extractTableNames ( (RelationNode (tbl) (vTree)) :xs) = (tbl) : extractTableNames xs

-- assignRelation :: OpTree -> [String] -> OpTree
-- assignRelation (RelationNode (tbl) (vTree)) (x:xs) = 
>>>>>>> d8734f21bf1fe6327f69bed22a939f523bc4cb9d

assignVarTreeLoc :: VarTree -> [String] -> VarTree
assignVarTreeLoc (SingleNode (Vari (loc) (dat) (name))) (x:xs) = (SingleNode (Vari (x) (dat) (name)))
assignVarTreeLoc (CommaNode (Vari (loc) (dat) (name)) (remTree)) (x:xs) = (CommaNode (Vari (x) (dat) (name)) (assignVarTreeLoc (remTree) (xs)))

assignAssignment :: OpTree -> String -> OpTree
assignAssignment (RelationNode (ass) (vTree) ) newAss = (RelationNode (newAss) (vTree))


isTreeAssigned :: VarTree -> Bool
isTreeAssigned (SingleNode vNode) = isNodeAssigned vNode
isTreeAssigned (CommaNode vNode remTree) = isNodeAssinged vNode && isTreeAssigned remTree 

isNodeAssigned :: VarNode -> Bool
isNodeAssigned (Vari (loc) (dat) (name))    | loc == "*"  = False-- Represents unassiged null value 
                                            | otherwise = True



assignVarNodeVal :: VarNode -> String -> VarNode
assignVarNodeVal (Vari (loc) (dat) (name)) newDat = (Vari (loc) (newDat) (name))

<<<<<<< HEAD
--doesRelationExist :: 
--extractAllTableNames :: OpTree -> [String] --In order of traversal
=======
-- doesRelationExist :: 


-- extractAllTableNames :: OpTree -> [String] --In order of traversal
>>>>>>> d8734f21bf1fe6327f69bed22a939f523bc4cb9d

extractRelNodeLocation :: OpTree -> String
extractRelNodeLocation (RelationNode (thisLoc) (vTree)) = thisLoc



{-=========================== Probably wont use this ======================================-}
{-
assignFreeLocs :: OpTree -> [VarNode] -> [VarNode]
assignFreeLocs (RelationNode (thisLoc) (vTree)) ((Vari (locA) (datA) (nameA)):xs) =  (Vari (thisLoc) (datA) (nameA) ) : assignFreeLocs' vTree xs thisLoc

assignFreeLocs' :: VarTree -> [VarNode] -> String -> [VarNode]
assignFreeLocs' (SingleNode (Vari (locA) (datA) (nameA) ))  ( (Vari (locB) (datB) (nameB) ) :xs) tblName = (Vari (tblName) (datB) (nameA)) : xs : []
assignFreeLocs' (CommaNode (Vari (locNext) (datNext) (nameNext) ) (remTree)) ( (Vari (locB) (datB) (nameB) ) :xs) tblName = (Vari (tblName) (datB) (nameNext)) : assignFreeLocs' remTree xs tblName
-}
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




-- *** TODO *** : Non bounded var w/ Exis



--Below for indexed var list:

{-========================================================================================-}















{-=============================== TREE HANDLING & TRAVERSAL ==============================-}

evaluateParseTree :: ParseTree a -> [String] -> (Bool, [VarNode])
evaluateParseTree (Marker ordVars oTree)
evaluateParseTree (MarkerNested eTree )

<<<<<<< HEAD
evaluate :: OpTree -> [String] -> (Bool, [VarNode])--evaluate opTree freeVarList
evaluate (EquateNode (l) (r)) freeVars =( ( checkEquality (EquateNode (l) (r)) ), freeVars )
evaluate (RelationNode (loc) (varTr)) freeVars = checkRelation ( (RelationNode (loc) (varTr)) freeVars )
evaluate (ConjunctionNode (l) (r)) freeVars = checkConjunction ( (ConjunctionNode (l) (r)) freeVars )
--evaluate (VarTree ) varRow = 
--evaluate (VarOp tree) freeVars = assignVars ((varTreeToList (tree)) freeVars)
=======
-- evaluate :: OpTree -> [VarNode] -> (Bool, [VarNode])--evaluate opTree freeVarList
-- evaluate (EquateNode (l) (r)) freeVars =( ( checkEquality (EquateNode (l) (r)) ), freeVars )
-- evaluate (RelationNode (loc) (varTr)) freeVars = checkRelation ( (RelationNode (loc) (varTr)) freeVars )
-- evaluate (ConjunctionNode (l) (r)) freeVars = checkConjunction ( (ConjunctionNode (l) (r)) freeVars )
-- evaluate (VarOp tree) freeVars = assignVars ((varTreeToList (tree)) freeVars)
>>>>>>> d8734f21bf1fe6327f69bed22a939f523bc4cb9d


--updateNodeValue 

--COnvert VarTree to list of nodes in tree
-- varTreeToList :: VarTree -> [(VarNode)] --Int represents ORDER (NB: this is why I decided to add VarNode)
-- varTreeToList (SingleNode (node) )  = (treeToNode (SingleNode (node)))  : []
-- varTreeToList (CommaNode (nextVar) (remainingTree)) = nextVar : varTreeToList remainingTree

--Converts VarTree with one node associated with it to a VarNode; ***TODO: Test this function I have no idea if this works ***
-- treeToNode :: VarTree -> VarNode
-- treeToNode (SingleNode (Vari (loc) (dat) (name))) = Vari (loc) (dat) (name)
-- --treeToNode (CommaNode (node) (remainingTree)) = parseError --Unsure of error notation or if this will work but throw an error here (***TODO***)

-- toIndexedList :: (VarTree) -> [(Int, VarNode)]
-- toIndexedList lst = zip [1..] varTreeToList( lst )
                    

-- *** TODO ** IMPORTANT: Implement a rule ensuring the children of an equality is 2 var nodes. Do we need to do this in our grammar/tree? See next commenr
-- checkEquality :: OpTree -> Bool
-- checkEquality (EquateNode (l) (r))  | l == r = True
--                                     | otherwise = False

--Compares the data currently asigned to 2 different VarNodes. If they are the same, return true, else false.
-- *** TODO *** maybe implement type error here?
-- equateNodes :: VarNode -> VarNode -> Bool
-- equateNodes (Vari (locA) (datA)) (Vari (locB) (datB))   | datA == datB = True
--          `                                               | datA /= datB = False

-- *** TODO *** ALSO REALLY IMPORTANT, see spec problem 2, (1 2 3 |- A(x1,x2)^B(x2,x3) we gotta have an equality check for both x2's
-- ALSO another point, does the order of the output matter? Spec says ordered lex'ally but idk if that means a specific order or just however its implemented
--checkRelation :: OpTree -> Bool
--checkRelation RelationNode tbl (x:xs) | 


-- *** TODO *** : Non bounded var w/ Exis
                            
{-=============================== CSV HANDLING ==============================-}
--A source: http://book.realworldhaskell.org/read/using-parsec.html

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

--extractCSVCol fileContents specifiedCol; returns (ColumnNumber, [all, instances, of, specified, column])
extractCSVCol :: [[String]] -> Int -> (Int, [String])
extractCSVCol [] _ = (-1, []) --Add error here
extractCSVCol (x:xs) ind = (ind, extractCSVCol' x ind 1) ++ extractCSVCol xs ind


--Auxilliary function, basically a safe version of !!
extractCSVCol' :: [String] -> Int -> Int -> String
extractCSVCol' [] _ _ = []
extractCSVCol' (x:xs) goal current | goal == current = x
                                   | goal /= current = extractCSVCol' xs goal (current+1)

--Return number of columns
countCSVCol :: [[String]] -> Int
countCSVCol (x:xs) = length x

--Collate columns into one data structure
gatherCSVdata :: [[String]] -> Int -> [(Int, [String])]
gatherCSVdata inp count | count > colNum = []
                        | count <= colNum = extractCSVCol inp count : gatherCSVdata inp (count+1)
                        where
                            colNum = countCSVCol inp

{-

Currently not working due to Either error wrt IO()

--Iterate through columns of CSV file, ensuring all data is collected. Throw error for empty file
getCSV :: [[String]] -> Either IO() a [(Int, [String])]
getCSV inp | inp == [] = Left( hPutStrLn stderr "Error: Missing CSV data" )
           | otherwise = Right( gatherCSVdata inp 1 )  
-}
----------------------------------------------------------------------------------------------------------------
------------------------------------------------- ELLIOTT ------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

crossProduct :: [[String]] -> [[String]] -> [([String],[String])]
crossProduct xs ys = [(x,y) | x <- (transpose xs), y <- (transpose ys)] -- input columns, transpose will change to rows

outputCross :: [([String],[String])] -> [[String]]
outputCross xs ys = xs : ys

stringToVarNode :: String -> String -> String -> VarNode
stringToVarNode loc dat name = VarNode (loc) (dat) (name)

-- evaluateE :: ExisitTree -> Bool
-- evaluateE varTree opTree | (varTreeToList (varTree)) --TODO

traverseDFS :: ParseTree -> [a]
traverseDFS (EmptyPT) = []
traverseDFS (Marker list op) = Marker : traverseDFS list : traverseDFS op
traverseDFS (Marker list exisit) = Marker : traverseDFS list : traverseDFS exisit
traverseDFS (Marker list exisit op) = Marker : traverseDFS list : traverseDFS exisit : traverseDFS op

traverseDFSOp :: OpTree -> [a]
traverseDFSOp EmptyOT = []
traverseDFSOp (ConjunctionNode left right) = traverseDFSOp left : traverseDFSOp right
traverseDFSOp (RelationNode string variables) = string ++ "(" : traverseDFSVar : ")"
traverseDFSOp (EquateNode left right) = traverseDFSOp left : traverseDFSOp right
traverseDFSOp (LSubNode left right) = traverseDFSOp left : traverseDFSOp right
traverseDFSOp (RSubNode left right) = traverseDFSOp left : traverseDFSOp right
traverseDFSOp (BoolNode f) = toString f
    
traverseDFSEx :: ExisitTree -> [a]
traverseDFSEx (EmptyET) = []
traverseDFSEx (ExisitVar var op) = traverseDFSVar var : traverseDFSOp op

isInteger :: (Typeable a) => a -> Bool
isInteger n = typeOf n == typeOf 1

isString :: (Typeable a) => a -> Bool
isString n = typeOf n == typeOf "HERB"

isChar :: (Typeable a) => a -> Bool
isChar n = typeOf n == typeOf 'c'

-- Blindly assumes OpTree contains a VarTree containing only one VarNode.
convertOpToVarNode :: OpTree -> VarNode
convertOpToVarNode VarOp (varTree) = treeToNode (varTree)

{-================================================     Probably not gonna use:      ======================================== -}

{-

evaluateParseTree :: ParseTree -> [String]
evaluateParseTree (Marker var op) = [evaluateVar var] ++ [evaluateOp op]
evaluateParseTree (Marker var exisit) = [evaluateVar var] ++ [evaluateExisit exisit]
evaluateParseTree (Marker var exisit op) = [evaluateVar var] ++ [evaluateExisit exisit] ++ [evaluateOp op]

evaluateExisit :: ExisitTree -> [String]
evaluateExisit (ExisitVar var op) = evaluateVar var ++ evaluateOp op

evaluateOp :: (OpTree) -> [String]
evaluateOp (ConjunctionNode left right) = evaluateOp left ++ evaluateOp right
evaluateOp (RelationNode string variables) = [string] ++ evaluateVar variables ++ [")"]
evaluateOp (EquateNode left right) = ["("] ++ evaluateOp left ++ ["="] ++ evaluateOp right ++ [")"]
evaluateOp (LSubNode left right) = evaluateOp left ++ evaluateOp right
evaluateOp (RSubNode left right) = evaluateOp left ++ evaluateOp right
evaluateOp (BoolNode f) = [toString f]

evaluateVar :: (VarTree) -> [String]
evaluateVar (CommaNode a b) = evaluateVar a ++ evaluateVar b
evaluateVar (VarNode s) = [s]









-- All commands available within language.
-- COMMAND TREE
data ComTree = Assign String ParseTree
    | Seq ComTree ComTree -- sequence commands together so that a parser can parse long sequences.
    | If ParseTree ComTree
    | IfElse ParseTree ComTree ComTree
    | While ParseTree ComTree
    | Declare Int ParseTree ComTree
    | Print ParseTree
    | List ParseTree
    | Place Int ParseTree
    deriving Show
-}