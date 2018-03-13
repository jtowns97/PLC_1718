{ 
module HERBGrammar where 
import HERBTokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    E         { TokenExistential _ }
    '.'       { TokenLinkLogic _ }   
    '^'       { TokenConjunction _ } 
    T         { TokenBool _  }
    F         { TokenBool _  }         
    '('       { TokenLParen _ }      
    ')'       { TokenRParen _ }      
    '='       { TokenEquality _ }    
    '<C'      { TokenLSubset _ }    
    '>C'      { TokenRSubset _ }    
    '|\-'     { TokenEntailment _ }          
    ','       { TokenComma _ }       
    var       { TokenVar _ $$ }  
    rel      { TokenRelation _ $$ }            

%left '|\-'
%left '.'
%left ',' 
%left '^' '=' '<C' '>C' 
%% 

Exp : Variables '|\-' Query                 { EntailL $1 $3 }
    | Variables '|\-' Existential          { EntailE $1 $3 }
	| Variables '|\-' Existential '.' Query { EntailB $1 $3 $5 }
	
Existential : E '(' Variables ')'          { ExistentialV $3 }
	
Variables : Variables ',' Variables        { Comma $1 $3 }
          | var                            { Var $1}
		  
Query : Query '^' Query                       { Conjunction $1 $3}
     | rel '(' Variables ')'              { Predicate $1 $3 }
	 | Query '=' Query                       { Equality $1 $3 }
	 | Query '<C' Query                      { LSub $1 $3 }
	 | Query '>C' Query                      { RSub $1 $3 }
	 | T                                   { Bool True }
	 | F                                   { Bool False }
    
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 
data Exp = EntailL Variables Query
         | EntailE Variables Existential
		 | EntailB Variables Existential Query
         deriving Show
data Variables = Comma Variables Variables
               | Var String
               deriving Show
data Query = Conjunction Query Query
          | Predicate String Variables
          | Equality Query Query
          | LSub Query Query
          | RSub Query Query
          | Bool Bool
          deriving Show
data Existential = ExistentialV Variables
                 deriving Show		  
} 