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
    "}"      { TokenRCurly _ } 
    "^"      { TokenConjunction _ $$} 
    "="      { TokenEquality _ $$}    
    "<C"     { TokenLSubset _ $$}    
    ">C"     { TokenRSubset _ $$}    
    "|-"     { TokenEntailment _ $$ }          
    ","      { TokenComma _ }
               

%left "|-" "E." "," "^" 
%right "="
%% 

Exp : Variables "|-" Query                              { Evaluate $1 $3 }


Variables : var "," Variables                     { Comma $1 $3 }
    | var                                               { VarSingle $1}



Query : Query "^" Query                                 { Conjunction $1 $3}
    | rel Variables "}"                                 { Relation $1 $2 }
	| Query "=" Query                                   { Equality $1 $3 }
	| True                                              { Bool True }
	| False                                             { Bool False }
    | "(" Variables ")" "E." "(" Query ")"                  { ExistentialSingle $2 $6 }



     
{ 
parseError :: [Token] -> a
parseError token = error "Parse error"

data Exp = Evaluate Variables Query
    deriving Show

data Variables = Comma String Variables
    | VarSingle String
    deriving Show

data Query = Conjunction Query Query
    | Relation String Variables
    | Equality Query Query
    | Bool Bool
    | V Variables
    | ExistentialSingle Variables Query
    deriving Show
} 