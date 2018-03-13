{-
data CTree = EntailL (VTree) (LTree) | EntailE (VTree) (ETree) | EntailB (VTree) (ETree) (LTree)
data VTree = Comma (VTree) (VTree) | Var (String)
data ETree = ExistentialV (VTree) | ExistentialS (String)
       	
data LTree = Conjunction (LTree) (LTree) | Predicate (String) (VTree) | Equality (LTree) (LTree) | LSub (LTree) (LTree) | RSub (LTree) (LTree) | Bool (Bool)
       	
evaluation x = evalC x
--evalC :: CTree -> [String]
evalC (EntailL v l) = [evalV v] ++ [evalL l] -- interpretEntailL (evalV v) (evalL l)
evalC (EntailE v e) = [evalV v] ++ [evalE e]
evalC (EntailB v e l) = [evalV v] ++ [evalE e] ++ [evalL l]
evalV :: VTree -> [String]
evalV (Comma a b) = evalV a ++ evalV b
evalV (Var s) = [s]
evalE :: ETree -> [String]
evalE (ExistentialV v) = evalV v
evalE (ExistentialS s) = [s]
evalL (Conjunction l r) = evalL l ++ evalL r
evalL (Predicate s v) = [s] ++ evalV v ++ [")"]
evalL (Equality l r) = ["("] ++ evalL l ++ ["="] ++ evalL r ++ [")"]
evalL (LSub l r) = evalL l ++ evalL r
evalL (RSub l r) = evalL l ++ evalL r
evalL (Bool a) | a == True = ["True"]
               | otherwise = ["False"]
               
____________________________________________________________
-}

-- All expressions available within language.
data Phi = Conjunction Phi Phi
    | Variable Int
    | Exisitential Variable Phi
    deriving Show

-- All commands available within language.
data Com = Assign String Int Phi
    | Seq Com Com -- sequence commands together so that a parser can parse long sequences.
    | Cond Phi Com Com
    | While Phi Com
    | Declare String Phi Com
    | Print Phi
    deriving Show

type Location = Int
type Index = [(String, Value)]
type Stack = [Int]
