{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ; 
  "--".*        ; 
  let           { \p s -> TokenLet p} 
  in            { \p s -> TokenIn p}
  $digit+       { \p s -> TokenInt p (read s) }
  \=          { \p s -> TokenEq p }
  \+          { \p s -> TokenPlus p}
  \-          { \p s -> TokenMinus p}
  \*          { \p s -> TokenTimes p}
  \/          { \p s -> TokenDiv p}
  \(          { \p s -> TokenLParen p}
  \)          { \p s -> TokenRParen p}
  \^          { \p s -> TokenExpon p}
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenLet AlexPosn         | 
  TokenIn AlexPosn          |
  TokenInt AlexPosn Int     |
  TokenVar AlexPosn String  | 
  TokenEq AlexPosn          |
  TokenG AlexPosn           |
  TokenGEq AlexPosn         | 
  TokenPlus AlexPosn        |
  TokenMinus AlexPosn       |
  TokenTimes AlexPosn       |
  TokenDiv AlexPosn         |
  TokenExpon AlexPosn       |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      
  deriving (Eq,Show) 


tokenPosn :: Token -> String
tokenPosn (TokenLet (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenIn (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenInt (AlexPn _ line col) _) = tokenPosn' line col
tokenPosn (TokenVar (AlexPn _ line col) _) = tokenPosn' line col
tokenPosn (TokenEq (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenG (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenGEq (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenPlus (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenMinus (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenTimes (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenDiv (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenExpon (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenLParen (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenRParen (AlexPn _ line col)) = tokenPosn' line col

tokenPosn' line col = show line ++ "," ++ show col
}