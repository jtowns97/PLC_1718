module HERBInterpreter where
import System.IO
import System.Environment
import Control.Monad
import HERBGrammar
import HERBTokens
    
-- Left hand side of algebra (variables or arguments)
-- VARIABLES TREE
data VarTree = CommaNode (VarTree) (VarTree)
    | VarNode (String) (String) 
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

varToInt :: [Char] -> Int
varToInt (x:xs) = read xs

toString :: Bool -> String
toString bool = if bool then "True" else "False"

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
    