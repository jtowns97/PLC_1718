module Main where

import Tokens
import Grammar
import qualified Data.Text as T
import System.Environment
import Data.List.Split
import Data.List
import Control.Monad
strip  = T.unpack . T.strip . T.pack
-- I THINK PROBLEM IS TO DO WITH THE AMBIGUOUS GRAMMAR PART, ITS GIVING US THE ERROR THAT IT WANTS A CTREE (Defined here)
-- BUT its getting the g.Exp which is happy
main = do 
 a <- getArgs
 b <- readFile (head a)
 let content = head (splitOn "\n" b)
 let alex = alexScanTokens (content)
 let happy = parseCalc (alex)
 c <- evalC(happy)
 mapM_ putStrLn (c)

 
readContents :: String -> IO [[String]]
readContents filepath = do
 contents <- readFile filepath
 let lines = chunksOf 1 (splitOn "\n" contents)
 return lines
 
--data CTree = EntailL (VTree) (LTree) | EntailE (VTree) (ETree) | EntailB (VTree) (ETree) (LTree)
--data VTree = Comma (VTree) (VTree) | Var (String)
--data ETree = ExistentialV (VTree) | ExistentialS (String)      	
--data LTree = Conjunction (LTree) (LTree) | Predicate (String) (VTree) | Equality (LTree) (LTree) | LSub (LTree) (LTree) | RSub (LTree) (LTree) | Bool (Bool)
  
--evalC :: CTree -> IO [String]
evalC (EntailL v l) | length (splitEquality (evalL l)) == 1 = liftCombineList (evalL l) (evalV v)
                	| length (splitEquality (evalL l)) > 1 = eqLiftCombineList (head (getEqualityVariablesList (getPredicatesFromEquality (evalL l)))) (changeVariableNames (evalV v) (getEVar (evalL l))) (getEVar (evalL l))
evalC (EntailB v e l) = liftCombineList (evalL l) (evalV v)
evalV (Comma a b) = evalV a ++ evalV b
evalV (Var s) = [s]
evalE (ExistentialV v) = evalV v
evalE (ExistentialS s) = [s]
evalL (Conjunction l r) = evalL l ++ evalL r
evalL (Predicate s v) = [s] ++ evalV v ++ [")"]
evalL (Equality l r) = ["["] ++ [l] ++ [r]
evalL (LSub l r) = evalL l ++ evalL r
evalL (RSub l r) = evalL l ++ evalL r
evalL (Bool a) | a == True = ["True"]
           	| otherwise = ["False"]
----------------------------------------------------------------------------------------

--functions to add equality functionality
changeVariableNames :: [String] -> [[String]] -> [String]
changeVariableNames xs (y:ys) | length ys > 0 = changeVariableNames (changeVar xs y y) ys
                          	| length ys == 0 = changeVar xs y y
changeVar (x:xs) (y:z:_) (zs) | length xs > 0 && x == y = [z] ++ changeVar xs zs zs
                          	| length xs > 0 && x /= y = [x] ++ changeVar xs zs zs
                          	| length xs == 0 && x == y = [z]
                          	| length xs == 0 && x /= y = [x]
getEVar :: [String] -> [[String]]                 	
getEVar xs = getEqualityVariablesList (getEqualities xs)
getEqualityVariablesList (x:xs) | length xs > 0 = [removeSpaces(getEqualityVariable x)] ++ getEqualityVariablesList xs
                            	| length xs == 0 = [removeSpaces(getEqualityVariable x)]
removeSpaces (x:xs) | length xs > 0 && x == "" = removeSpaces xs
                	| length xs > 0 && x /= "" = [x] ++ removeSpaces xs
                	| length xs == 0 && x == "" = []
                	| length xs == 0 && x /= "" = [x]
getEqualityVariable x = splitOn "," x
getEqualities xs = drop 1 (splitEquality xs)
splitEquality xs =  splitOn "[" (intercalate "," xs)
getPredicateVariables xs = splitOn "," (head xs)
getPredicatesFromEquality :: [String] -> [String]
getPredicatesFromEquality xs = take 1 (splitEquality xs)

-- function to sort and print output
printer xs ys = liftM (mapM_ print) (liftCombineList xs ys)
--ALTERNATIVE LIFT using eq
eqLiftCombineList xs ys zs = liftM (sort) (liftM (combineStrings) (eqLiftOrderAll xs ys zs))
 
--function to print results
liftCombineList :: [String] -> [String] -> IO [String]
liftCombineList xs ys = liftM (sort) (liftM (combineStrings) (liftOrderAll xs ys))
combineStrings :: [[String]] -> [String]
combineStrings (x:xs) | length xs > 0 = [combineList x] ++ combineStrings xs
                  	| length xs == 0 = [combineList x]
combineList (x:xs) | length xs > 0 = x ++ "," ++ combineList xs
               	| length xs == 0 = x                  	
--ALTERNATIVE LIFT using equality
-- xs is for predicates, ys is variable order list, zs is equality list of list
eqLiftOrderAll xs ys zs= liftM2 (orderAllLines) (liftRemoveDuplicatesFully xs zs) (m2 (ys))
--lift orderAllLine to IO
liftOrderAll :: [String] -> [String] -> IO [[String]]
liftOrderAll xs ys = liftM2 (orderAllLines) (liftRemoveAllDuplicates xs) (m2 (ys))
--function to order all lines pair and remove variable names: does not check for equal variables 
orderAllLines :: [[(String,String)]] -> [String] -> [[String]]
orderAllLines [] ys = [[]]
orderAllLines (x:xs) (ys) | length xs > 0 = [orderLine x ys x] ++ orderAllLines xs ys
                      	| length xs == 0 = [orderLine x ys x]
                   	
orderLine :: [(String,String)] -> [String] -> [(String,String)] -> [String]                   	
orderLine ((a,b):xs) (y:ys) (z:zs) | length ys > 0 && length xs > 0 && y == a = [b] ++ orderLine (z:zs) (ys) (z:zs)
                               	| length ys > 0 && length xs > 0 && y /= a = orderLine xs (y:ys) (z:zs)
                               	| length ys > 0 && length xs == 0 && y == a = [b] ++ orderLine (z:zs) (ys) (z:zs)
                               	| length ys > 0 && length xs == 0 && y /= a = orderLine (z:zs) (ys) (z:zs)
                               	| length ys == 0 && length xs > 0 && y == a = [b]
                               	| length ys == 0 && length xs > 0 && y /= a = orderLine xs (y:ys) (z:zs)
                               	| length ys == 0 && length xs == 0 && y == a = [b]
                               	| length ys == 0 && length xs == 0 && y /= a = []                           	
                               	
--function  to check for equality
liftRemoveDuplicatesFully :: [String] -> [[String]] -> IO [[(String, String)]]
liftRemoveDuplicatesFully xs ys = liftM (checkForPairEquality) (liftSwapValues xs ys)
checkForPairEquality :: [[(String, String)]] -> [[(String,String)]]
checkForPairEquality (x:xs) | length xs > 0 && checkPairs x == True = [x] ++ checkForPairEquality xs
                        	| length xs > 0 && checkPairs x == False = checkForPairEquality xs
                        	| length xs == 0 && checkPairs x == True = [x]
                        	| length xs == 0 && checkPairs x == False = []
checkPairs (x:xs) | length xs > 0 = (checkPair x xs) && checkPairs xs
              	| length xs == 0 = True
checkPair :: (String,String) -> [(String,String)] -> Bool
checkPair (a,b) ((c,d):xs) | length xs > 0 && a == c && b == d = True && checkPair (a,b) xs
                       	| length xs > 0 && a == c && b /= d = False && checkPair (a,b) xs
                       	| length xs > 0 && a /= c = True && checkPair (a,b) xs
                       	| length xs == 0 && a == c && b == d = True
                       	| length xs == 0 && a == c && b /= d = False
                       	| length xs == 0 && a /= c = True
                       	| otherwise = True
liftSwapValues xs ys = liftM2 (swapValues) (liftRemoveAllDuplicates xs) (m ys)
swapValues :: [[(String, String)]]-> [[String]] -> [[(String,String)]]
swapValues (xs) (y:ys) | length ys > 0 = swapValues (swapSet xs y) ys
                   	| length ys == 0 = swapSet xs y
swapSet :: [[(String,String)]] -> [String] -> [[(String,String)]]                   	
swapSet (x:xs) ys | length xs > 0 = [swapInd x ys ys] ++ swapSet xs ys
              	| length xs == 0 = [swapInd x ys ys]
swapInd :: [(String,String)] -> [String] -> [String] -> [(String,String)]
swapInd ((a,b):xs) (y:z:_) zs | length xs > 0 && a == y = [(z,b)] ++ swapInd xs zs zs
                          	| length xs > 0 && a /= y = [(a,b)] ++ swapInd xs zs zs
                          	| length xs == 0 && a == y = [(z,b)]
                          	| length xs == 0 && a /= y = [(a,b)]	
                          	
--function to remove duplicate variables
liftRemoveAllDuplicates :: [String] -> IO [[(String, String)]]
liftRemoveAllDuplicates xs = liftM (removeAllDuplicates) (liftConjunction xs)
removeAllDuplicates :: [[(String, String)]] -> [[(String, String)]]
removeAllDuplicates xs = removeDuplicatesFully (removeDuplicates xs)
removeDuplicatesFully :: [[(String, String)]] -> [[(String, String)]]
removeDuplicatesFully (x:xs) | length xs > 0 && checkForDelete x == True = [applyRemove x] ++ removeDuplicatesFully xs
                         	| length xs > 0 && checkForDelete x == False = removeDuplicatesFully xs
                         	| length xs == 0 && checkForDelete x == True = [applyRemove x]
                         	| length xs == 0 && checkForDelete x == False = []
                         	| otherwise = []
removeDuplicates (x:xs) | length xs > 0 = [removeDuplicate x] ++ removeDuplicates xs
                    	| length xs == 0 = [removeDuplicate x]
removeDuplicate (x:xs) | length xs > 0 = [x] ++ removeDuplicate (removeIndividual x (xs))
                   	| length xs == 0 =  [x]
removeIndividual x (xs) = crossPair x (xs)
crossPair (a,b) ((c,d):ys) | length ys > 0 && a == c && b == d = [("R","")] ++ crossPair (a,b) ys
                       	| length ys > 0 && a == c && b /= d = [("D","")] ++ crossPair (a,b) ys
                       	| length ys > 0 && a /= c && b /= d = [(c,d)] ++ crossPair (a,b) ys
                       	| length ys == 0 && a == c && b == d = [("R","")]
                       	| length ys == 0 && a == c && b /= d = [("D","")]
                       	| length ys == 0 && a /= c && b /= d = [(c,d)]
                       	| otherwise = [(c,d)]
applyRemove ((a,b):xs) | length xs > 0 && a == "R" = applyRemove xs
                   	| length xs > 0 && a /= "R" = [(a,b)] ++ applyRemove xs
                   	| length xs == 0 && a == "R" = []
                   	| length xs == 0 && a /= "R" = [(a,b)]                       	
checkForDelete (x:xs) | length xs > 0 = checkDelete x && checkForDelete xs
                  	| length xs == 0 = checkDelete x
checkDelete (a,b) | a == "D" = False
              	| a /= "D" = True
                 	
-- function to lift getLinesForConjunction to IO
liftConjunction :: [String] -> IO [[(String, String)]]
liftConjunction xs = liftM (getLinesForConjunction) (liftZipVar xs)
--function to combine zippedVariableCSV values line by line for output
getLinesForConjunction :: [[[(String,String)]]] -> [[(String,String)]]
getLinesForConjunction (x:xs) | length xs > 0 = [cross x] ++ getLinesForConjunction xs
                          	| length xs == 0 = [cross x]
cross :: [[(String,String)]] -> [(String,String)]
cross (x:xs) | length xs > 0 =  x ++ cross xs
         	| length xs == 0 = x
--function to make zipvar liftable for IO from splitColumnList
liftZipVar :: [String] -> IO [[[(String, String)]]]
liftZipVar xs = liftM2 (zipVarCSV) (m (getVariablesLists(splitPredicates xs))) (splitColumnList (splitPredicates xs))
--function to zip variables against csv list: variable list first
zipVarCSV :: [[String]] -> [[[String]]] -> [[[(String, String)]]]
zipVarCSV (xs) (y:ys) | length ys > 0 = [zipIndividual xs y] ++ zipVarCSV xs ys
                  	| length ys == 0 = [zipIndividual xs y]
zipIndividual (x:xs) (y:ys) | length ys > 0 = [zip x y] ++ zipIndividual xs ys
                        	| length ys == 0 = [zip x y]             	
 	
--gets list of predicates with their variables, passes to getCSV, getVariables           	
splitPredicates :: [String] -> [String]
splitPredicates xs = dropEndListSpace (splitOn ")" (intercalate "," xs))

--get csv lines as list, separated into individual words in a each sub list, for all columns(predicates)
splitColumnList:: [String] -> IO [[[String]]]
splitColumnList (x:xs) | length xs > 0 = combineIO (liftTrimEntries x) (splitColumnList xs)
                   	| length xs == 0 = liftTrimEntries x
--function to trim column entries
liftTrimEntries :: String -> IO [[[String]]]
liftTrimEntries x = liftM (trimEntries) (liftSplitColumn x)
trimEntries :: [[[String]]] -> [[[String]]]
trimEntries (x:xs) | length xs > 0 = [subTrim x] ++ trimEntries xs
               	| length xs == 0 = [subTrim x]
subTrim (x:xs) | length xs > 0 = [subSubTrim x] ++ subTrim xs
           	| length xs == 0 = [subSubTrim x]
subSubTrim (x:xs) | length xs > 0 = [strip x] ++ subSubTrim xs
           	| length xs == 0 = [strip x]               	
-- gets csv column, divides string data into words to match against variables
liftSplitColumn :: String -> IO [[[String]]]        	
liftSplitColumn x = liftM (splitColumn) (getCSV x)               	
splitColumn (x:xs) | length xs > 0 = [[(splitOn "," (getString x))]] ++ splitColumn xs
               	| length xs == 0 = [[(splitOn "," (getString x))]]
getString (x:xs) = x
--read csv file
getCSV :: String -> IO [[String]]
getCSV x = do
 readContents ([getPredicate x] ++ ".csv")
getPredicate :: [Char] -> Char
getPredicate (x:xs) | x == ',' = getPredicate xs
                	| otherwise = x
                            	
--get list of variables for each predicate
getVariablesLists :: [String] -> [[String]]
getVariablesLists (x:xs) | length xs > 0 = [getVariablesList x] ++ getVariablesLists xs
                     	| length xs == 0 = [getVariablesList x]
getVariablesList :: String -> [String]
getVariablesList xs = dropEndListSpace (getVariables xs)                                	
getVariables :: [Char] -> [String]                	
getVariables (x:xs) | x == ',' = splitOn "," (drop 2 xs)
                	| otherwise = splitOn "," (drop 1 xs)
dropEndListSpace :: [String] -> [String]                	
dropEndListSpace xs = take ((length xs)-1) xs
  
--IO List combination function
--combineIO :: IO [[String]] -> IO [[String]] -> IO [[String]]
combineIO a b = do
  	w <- a
  	x <- b
  	let out = [ y ++ z | y <- w, z <- x ]
  	return out  	
m :: [[String]] -> IO [[String]]
m xs = do return xs
m2 :: [String] -> IO [String]
m2 xs = do return xs
--function to check for empty csv files
liftCheckEmpty xs = liftM (checkEmpty) (splitColumnList (splitPredicates xs))
checkEmpty :: [[[String]]] -> Bool
checkEmpty (x:xs) | length xs > 0 = subCheck x && checkEmpty xs
              	| length xs == 0 = subCheck x
subCheck (x:xs) | length xs > 0 = subSubCheck x && subCheck xs
            	| length xs == 0 = subSubCheck x
subSubCheck (x:xs) | length xs > 0 && x == "" || x == " " = False && subSubCheck xs
               	| length xs > 0 && x /= "" && x /= " " = True && subSubCheck xs
               	| length xs == 0 && x == "" || x == " " = False
               	| length xs == 0 && x /= "" || x /= " " = True