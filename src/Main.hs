module Main where
    import System.IO
    import Control.Monad
    import Grammar
    import Tokens

    main :: IO()
    main = do
            string <- readFile "testFile.txt"
            print (alexScanTokens string) 
            let absT = ( (parseCalc. alexScanTokens) string )
            print absT


tokenPosn :: Token -> String
tokenPosn (TokenLet (AlexPn _ line col)) = show line ++ "," ++ show col
tokenPosn _ = "" 