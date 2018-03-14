{ 
module TESTTokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]
$alphaCap = [A-Z]  
-- alphabetic characters

tokens :-
    $white+                             ; 
    "--".*                              ; 
    '\|\-'                                { \p s -> TokenEntailment p} --TokenMarker
    '\^'                                { \p s -> TokenConjunction p}
    '\<C'                               { \p s -> TokenLSubset p}
    '\>C'                               { \p s -> TokenRSubset p}
    E                                   { \p s -> TokenExistential p} -- 2 variables needed here? ie. THERE EXISTS (x) WHERE (x + y = 1) etc
    '\='                                { \p s -> TokenEquality p }
    '\('                                { \p s -> TokenLParen p}
    '\)'                                { \p s -> TokenRParen p}
    '.'                                 { \p s -> TokenLinkLogic p}
    T                                   { \p s -> TokenBool p}
    F                                   { \p s -> TokenBool p}
    $alphaCap '\(' [$alpha \_]* '\)'    { \p s -> TokenRelation p s}
    $alpha [$alpha $digit \_ \’]*       { \p s -> TokenVar p s }  -- Char then Number, ie Valid = [x1,y22,b33,ddd] Invalid = [1z,2]


    --TODO: TokenRelation, TokenRelation,  Verify correctness
{ 


-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
    TokenMarker AlexPosn            | 
    TokenQuant AlexPosn             |
    TokenConjunction AlexPosn       |
    --TokenRel AlexPosn String      |
    TokenVar AlexPosn String        | 
    TokenEquality AlexPosn          |
    TokenLParen AlexPosn            |
    TokenRParen AlexPosn            |
    TokenLSubset AlexPosn           |
    TokenRSubset AlexPosn           |
    TokenExistential AlexPosn       |
    TokenBool AlexPosn              |
    TokenLinkLogic AlexPosn         |
    TokenComma AlexPosn             |
    TokenRelation AlexPosn String  |
    TokenEntailment AlexPosn      
    deriving (Eq,Show) 


tokenPosn :: Token -> AlexPosn
tokenPosn (TokenEntailment p ) = p
tokenPosn (TokenConjunction p ) = p
tokenPosn (TokenLSubset p ) = p
tokenPosn (TokenRSubset p ) = p
tokenPosn (TokenExistential p ) = p
tokenPosn (TokenEquality p ) = p
tokenPosn (TokenLParen p ) = p
tokenPosn (TokenRParen p ) = p
tokenPosn (TokenLinkLogic p ) = p
tokenPosn (TokenBool p) = p
tokenPosn (TokenRelation p s ) = p
tokenPosn (TokenVar p s ) = p


{-
tokenPosn (TokenMarker (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenConjunction (AlexPn _ line col)) = tokenPosn' line col
--tokenPosn (TokenRel (AlexPn _ line col) _) = tokenPosn' line col
tokenPosn (TokenVar (AlexPn _ line col) _) = tokenPosn' line col
tokenPosn (TokenLParen (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenRParen (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenLSubset (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenRSubset (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenLinkLogic (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenExistential (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenEntailment (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenBool (AlexPn _ line col)) = tokenPosn' line col
tokenPosn (TokenEquality (AlexPn _ line col)) = tokenPosn' line col

tokenPosn' line col = show line ++ "," ++ show col
-}


}