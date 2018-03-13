{ 
module HERBGrammar where 
import HERBTokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    'E\.'     { TokenExistential _ }  
    '^'       { TokenConjunction _ } 
    True      { TokenBool _  }
    False     { TokenBool _  }         
    '('       { TokenLParen _ }      
    ')'       { TokenRParen _ }   
    '='       { TokenEquality _ }    
    '<C'      { TokenLSubset _ }    
    '>C'      { TokenRSubset _ }    
    '|\-'     { TokenEntailment _ }          
    ','       { TokenComma _ }       
    var       { TokenVar _ $$ }  
    rel       { TokenRelation _ $$ }            

%left '|\-' '.' ',' '^' '<C' '>C' '='
%% 

Exp : Variables '|\-' Query                         { Evaluate $1 $3 }
	
Existential : '(' Variables ')' 'E\.' '(' Query ')' { ExistentialV $2 $6 }
	
Variables : Variables ',' Variables                 { Comma $1 $3 }
     | var                                          { Var $1}

Query : Query '^' Query                             { Conjunction $1 $3}
     | Existential                                  { EvaluateSingle $1 }
     | Existential Query                            { EvaluateNested $1 $2 }
     | rel '(' Variables ')'                        { Relation $1 $3 }
	 | Query '=' Query                              { Equality $1 $3 }
	 | Query '<C' Query                             { LSub $1 $3 }
	 | Query '>C' Query                             { RSub $1 $3 }
	 | True                                         { Bool True }
	 | False                                        { Bool False }
    
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 
data Exp = Evaluate Variables Query
     deriving Show
data Variables = Comma Variables Variables
     | Var String
     deriving Show
data Query = Conjunction Query Query
     | EvaluateSingle Existential
     | EvaluateNested Existential Query
     | Relation String Variables
     | Equality Query Query
     | LSub Query Query
     | RSub Query Query
     | Bool Bool
     deriving Show
data Existential = ExistentialV Variables Query
     deriving Show		  
} 