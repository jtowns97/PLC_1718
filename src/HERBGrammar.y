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
               

%left "|-" "E." "^" 
%right "=" "," 
%% 

Exp : Variables "|-" Query                              { Evaluate $1 $3 }


Variables : var "," Variables                     { Comma $1 $3 }
    | var                                               { VarSingle $1}



Query : Query "^" Query                                 { Conjunction $1 $3}
    | Query "^" Query "^" Query                         { ConjunctionTriple $1 $3 $5}
    | rel Variables "}"                                 { Relation $1 $2 }
	| "(" Variables "=" Variables ")"                   { Equality $2 $4 }
	| True                                              { Bool True }
	| False                                             { Bool False }
    | Variables                                               { V $1 } 
    | "(" Variables ")" "E." "(" Query ")"     { ExistentialSingle $2 $6 }



     
{ 
parseError :: [Token] -> a
parseError token = error "Parse error : on token : " token

data Exp = Evaluate Variables Query
    deriving Show

data Variables = Comma String Variables
    | VarSingle String
    deriving Show

data Query = Conjunction Query Query
    | ConjunctionTriple Query Query Query
    | Relation String Variables
    | Equality Variables Variables
    | Bool Bool
    | V Variables
    | ExistentialSingle Variables Query
    deriving Show
} 