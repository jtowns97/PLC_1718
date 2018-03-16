module HERBInterpreter where
import System.IO
import System.Environment
import Control.Monad
import HERBGrammar
import HERBTokens
import Text.ParserCombinators.Parsec
    
-- Left hand side of algebra (variables or arguments)
-- VARIABLES TREE
data VarTree = CommaNode (VarNode) (VarTree) --Added to force tree structure to right recurse
    | SingleNode (VarNode)  -- VarNode location actualData
    deriving Show
    
data VarNode = Vari (String) (String) -- loc dat
    deriving Show

-- Right hand side of algebra (query or request)
-- OPERATORS TREE
data OpTree = ConjunctionNode (OpTree) (OpTree)
    | RelationNode (String) (VarTree) -- String is Table name.
    | EquateNode (OpTree) (OpTree)
    | LSubNode (OpTree) (OpTree)
    | RSubNode (OpTree) (OpTree)
    | BoolNode (Bool)
    | VarTree
    deriving Show

-- Seperate exisitential operator.
-- EXISITENTIAL TREE
data ExisitTree = ExisitVar (VarTree) (OpTree)
    deriving Show

-- All expressions available within language.
-- PARSE TREE
data ParseTree = Marker (OrderedVars) (OpTree)
    | MarkerNested (OrderedVars) (ExisitTree)
    | MarkerExtended (OrderedVars) (ExisitTree) (OpTree)
    deriving Show
  --  (1,2,3)E.( ( (1,2)E.Q(x1,x2) ^ (x1 = x2) ) ^ (x3=foo) )



-- Memory and location storage for interpreter.
type Location = Int
--type Map = [(Int, Value)]
type Stack = [Int]
newtype OrderedVars = IndVars [(Int, [VarNode])] deriving Show --To make tree readability easier, ***TODO*** test


varToInt :: [Char] -> Int
varToInt (x:xs) = read xs

toString :: Bool -> String
toString bool = if bool then "True" else "False"
{-
-- Read input from command line with IO monad.
main :: IO()
main = do
    filePath <- getArgs -- Take file path from command line.
    contents <- readFile (head filePath) -- read file
    let string = parseCalc (alexScanTokens contents) -- build parse tree from the contents.

    --TEST: showing parse tree.
    putStrLn ("ParseTree: " ++ show(string))
    print "... tree parsed."

--getVarLocation                       
-} 




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

{-
data indexedVars =  | vars indexedVars : (varTuple) : []
                    | (varTuple)
                    
data varTuple = tup (Int, VarNode)
-}
--COnvert VarTree to list of nodes in tree
varTreeToList :: VarTree -> [(VarNode)] --Int represents ORDER (NB: this is why I decided to add VarNode)
varTreeToList (SingleNode (node) )  = (treeToNode (SingleNode (node)))  : []
varTreeToList (CommaNode (nextVar) (remainingTree)) = nextVar : varTreeToList remainingTree

--Converts VarTree with one node associated with it to a VarNode; ***TODO: Test this function I have no idea if this works ***
treeToNode :: VarTree -> VarNode
treeToNode (SingleNode (Vari (loc) (dat))) = Vari (loc) (dat)
treeToNode (CommaNode (node) (remainingTree)) = parseError --Unsure of error notation or if this will work but throw an error here (***TODO***)

toIndexedList :: [(VarTree)] -> [(Int, VarNode)]
toIndexedList [] = []
toIndexedList lst = zip [1..] treeToNode( lst )
                    

-- *** TODO ** IMPORTANT: Implement a rule ensuring the children of an equality is 2 var nodes. Do we need to do this in our grammar/tree? See next commenr
checkEquality :: OpTree -> Bool
checkEquality (EquateNode (l) (r))  | l == r = True
                                    | otherwise = False

--Compares the data currently asigned to 2 different VarNodes. If they are the same, return true, else false.
-- *** TODO *** maybe implement type error here?
equateNodes :: VarNode -> VarNode -> Bool
equateNodes (Vari (locA) (datA)) (Vari (locB) (datB))   | datA == datB = True
                                                        | datA /= datB = False

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