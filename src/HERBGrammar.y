{ 
module HERBGrammar where 
import HERBTokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token
    "E."     { TokenExistential _ $$ }   
    True     { TokenBool _  }
    False    { TokenBool _  }      
    var      { TokenVar _ $$ }  
    rel      { TokenRelation _ $$ }           
    "("      { TokenLParen _ }      
    ")"      { TokenRParen _ }   
    "^"      { TokenConjunction _ $$} 
    "="      { TokenEquality _ $$}    
    "<C"     { TokenLSubset _ $$}    
    ">C"     { TokenRSubset _ $$}    
    "|-"     { TokenEntailment _ $$ }          
    ","      { TokenComma _ }
               

%left "|-" "E." "," "^" "<C" ">C" 
%right "="
%% 

Exp : Variables "|-" Query                              { Evaluate $1 $3 }
	
Existential :  "(" Variables ")" "E." "(" Query ")"     { ExistentialSingle $2 $6 }
    | "(" Variables ")" "E." "(" Existential  ")"       { ExistentialNested $2 $6 }
	| "(" Variables ")" "E." "(" Existential  ")" Query { ExistentialExtended $2 $6 $8}

Variables : String "," Variables                     { Comma $1 $3 }
    | Variables                                          { VarSingle $1}

                                

Query : Query "^" Query                                 { Conjunction $1 $3}
    | rel "(" Variables ")"                             { Relation $1 $3 }
	| Query "=" Query                                   { Equality $1 $3 }
	| Query "<C" Query                                  { LSub $1 $3 }
	| Query ">C" Query                                  { RSub $1 $3 }
	| True                                              { Bool True }
	| False                                             { Bool False }
     
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = Evaluate Variables Query
    | Eval Variables Existential
    | EvalExisExt Variables Existential Query
    deriving Show


data Variables = Comma String Variables
    | VarSingle String
    deriving Show
data Query = Conjunction Query Query
    | Relation String Variables
    | Equality Query Query
    | LSub Query Query
    | RSub Query Query
    | Bool Bool
    | V Variables
    deriving Show
data Existential = ExistentialSingle Variables Query
    | ExistentialNested Variables Existential
    deriving Show		  
} 