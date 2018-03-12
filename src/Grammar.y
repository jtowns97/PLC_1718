{ 
module Grammar where 
import Tokens 
import Data.List
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    MARKER { TokenMarker _} 
    CONJ  { TokenConj _} 
    RELATION { TokenRel _} 
    var { TokenVar _ $$ } 
    '=' { TokenEq _} 
    '(' { TokenLParen _} 
    ')' { TokenRParen _} 


%right in 
%left '+' '-' 
%left '*' '/' 
%left '^'
%left NEG 
%% 
Query : var CONJ var { Conj $1 $3 }
    | QUANT var RELATION var var {Quant}
Exp : [var] MARKER Query { Run $1 $3 } 
    | Exp '+' Exp            { Plus $1 $3 } 
    | Exp '-' Exp            { Minus $1 $3 } 
    | Exp '*' Exp            { Times $1 $3 } 
    | Exp '/' Exp            { Div $1 $3 } 
    | Exp '^' Exp            { Expon $1 $3 }
    | '(' Exp ')'            { $2 } 
    | '-' Exp %prec NEG      { Negate $2 } 
    | int                    { Int $1 } 
    | var                    { Var $1 } 
    
{ 
parseError :: [Token] -> a
parseError ts = error $ "Error on tokens: " ++ (unwords $ map tokenPosn ts)
data Exp = Let String Exp Exp 
         | Plus Exp Exp 
         | Minus Exp Exp 
         | Times Exp Exp 
         | Div Exp Exp
         | Expon Exp Exp 
         | Negate Exp
         | Int Int 
         | Var String 
         deriving Show 
} 
