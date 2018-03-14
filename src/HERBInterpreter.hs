module HERBInterpreter where
import System.IO
import System.Environment
import Control.Monad
import HERBGrammar
import HERBTokens
import Text.ParserCombinators.Parsec
    
-- Left hand side of algebra (variables or arguments)
-- VARIABLES TREE
data VarTree = CommaNode (VarTree) (VarTree)
    | VarNode (String)
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
data ParseTree = Marker (VarTree) (OpTree)
    | MarkerNested (VarTree) (ExisitTree)
    | MarkerExtended (VarTree) (ExisitTree) (OpTree)
    deriving Show

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

-- Memory and location storage for interpreter.
type Location = Int
--type Map = [(Int, Value)]
type Stack = [Int]

--evaluateParseTree :: ParseTree -> [String]
--evaluateParseTree 


--evaluateCom ::

evaluateOp :: (OpTree) -> [String]
evaluateOp (ConjunctionNode left right) = evaluateOp left ++ evaluateOp right
evaluateOp (RelationNode string variables) = [string] ++ evaluateVar variables ++ [")"]
evaluateOp (EquateNode left right) = ["("] ++ evaluateOp left ++ ["="] ++ evaluateOp right ++ [")"]
--evaluateOp (ExNodeisitTree string) = -- TODO:
--evaluateOp (ExisitTree var op) = -- TODO:
evaluateOp (LSubNode left right) = evaluateOp left ++ evaluateOp right
evaluateOp (RSubNode left right) = evaluateOp left ++ evaluateOp right
evaluateOp (BoolNode f) = [toString f]


evaluateVar :: (VarTree) -> [String]
evaluateVar (CommaNode a b) = evaluateVar a ++ evaluateVar b
evaluateVar (VarNode s) = [s]

varToInt :: [Char] -> Int
varToInt (x:xs) = read xs

toString :: Bool -> String
toString bool = if bool then "True" else "False"
{-
-- Read input from command line with IO monad.
main :: String -> IO()
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
extractCSVCol [] _ = []
extractCSVCol (x:xs) ind = (ind, extractCSVCol' x ind 1 : extractCSVCol xs ind)

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

Currently not working due to Either error

--Iterate through columns of CSV file, ensuring all data is collected. Throw error for empty file
getCSV :: [[String]] -> Either IO() a [(Int, [String])]
getCSV inp | inp == [] = Left( hPutStrLn stderr "Error: Missing CSV data" )
           | otherwise = Right( gatherCSVdata inp 1 )  
-}

                                   