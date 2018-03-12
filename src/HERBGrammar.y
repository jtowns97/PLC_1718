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
    pred      { TokenPredicate _ $$ }            

%left '|\-'
%left '.'
%left ',' 
%left '^' '=' '<C' '>C' 
%% 

Exp : Variables '|\-' LExp                 { EntailL $1 $3 }
    | Variables '|\-' Existential          { EntailE $1 $3 }
	| Variables '|\-' Existential '.' LExp { EntailB $1 $3 $5 }
	
Existential : E '(' Variables ')'          { ExistentialV $3 }
            | E var                        { ExistentialS $2 }
	
Variables : Variables ',' Variables        { Comma $1 $3 }
          | var                            { Var $1}
		  
LExp : LExp '^' LExp                       { Conjunction $1 $3}
     | pred '(' Variables ')'              { Predicate $1 $3 }
	 | LExp '=' LExp                       { Equality $1 $3 }
	 | LExp '<C' LExp                      { LSub $1 $3 }
	 | LExp '>C' LExp                      { RSub $1 $3 }
	 | T                                   { Bool True }
	 | F                                   { Bool False }
    
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 
data Exp = EntailL Variables LExp
         | EntailE Variables Existential
		 | EntailB Variables Existential LExp
         deriving Show
data Variables = Comma Variables Variables
               | Var String
               deriving Show
data LExp = Conjunction LExp LExp
          | Predicate String Variables
          | Equality LExp LExp
          | LSub LExp LExp
          | RSub LExp LExp
          | Bool Bool
          deriving Show
data Existential = ExistentialV Variables
                 | ExistentialS String
                 deriving Show		  
} 