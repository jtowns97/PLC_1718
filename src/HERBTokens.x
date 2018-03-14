{ 
module HERBTokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters
$alphaCap = [A-Z]
-- capital alphabetic characters


tokens :-
    $white+                             ; 
    "--".*                              ; 
    \|\-                               { \p s -> TokenEntailment p s} 
    \^                                { \p s -> TokenConjunction p s}
    \<C                               { \p s -> TokenLSubset p s}
    \>C                               { \p s -> TokenRSubset p s}
    E.                               { \p s -> TokenExistential p s} -- 2 variables needed here? ie. THERE EXISTS (x) WHERE (x + y = 1) etc
    \=                                { \p s -> TokenEquality p s}
    \(                                { \p s -> TokenLParen p}
    \)                                { \p s -> TokenRParen p}
    \,                                { \p s -> TokenComma p}
    \;
    True                                { \p s -> TokenBool p}
    False                               { \p s -> TokenBool p}
    $alphaCap '\('           { \p s -> TokenRelation p s} -- Relation defined as any capital letter followed by an open bracket
    $alpha $digit [$digit]*       { \p s -> TokenVar p s }  -- Char then Number, ie Valid = [x1,y22,b33,ddd]- Invalid = [1z,2]
{ 


-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
    TokenConjunction AlexPosn String|
    TokenVar AlexPosn String        | 
    TokenEquality AlexPosn String         |
    TokenLParen AlexPosn            |
    TokenRParen AlexPosn            |
    TokenLSubset AlexPosn String          |
    TokenRSubset AlexPosn String          |
    TokenExistential AlexPosn String|
    TokenBool AlexPosn              |
    TokenLink AlexPosn              |
    TokenComma AlexPosn      |
    TokenRelation AlexPosn String   |
    TokenEntailment AlexPosn String   
    deriving (Eq,Show) 


tokenPosn :: Token -> AlexPosn
tokenPosn (TokenEntailment p s) = p
tokenPosn (TokenConjunction p s) = p
tokenPosn (TokenVar p s ) = p
tokenPosn (TokenLParen p ) = p
tokenPosn (TokenRParen p ) = p
tokenPosn (TokenLSubset p s) = p
tokenPosn (TokenRSubset p s) = p
tokenPosn (TokenLink p ) = p
tokenPosn (TokenEquality p s) = p
tokenPosn (TokenComma p ) = p
tokenPosn (TokenExistential p s) = p
tokenPosn (TokenRelation p s) = p



{-
tokenPosn (TokenMarker (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenConjunction (AlexPn _ line col)) = tokenPosn' line col
--tokenPosn (TokenRel (AlexPn _ line col) _) = tokenPosn' line col
tokenPosn (TokenVar (AlexPn _ line col) _) = tokenPosn' line col
tokenPosn (TokenLParen (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenRParen (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenLSubset (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenRSubset (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenLList (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenRList (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenLink (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenExistential (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenEntailment (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenBool (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenEquality (AlexPn _ line col)) = tokenPosn' line col

tokenPosn' line col = show line ++ "," ++ show col
-}


}