{-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


--{-# OPTIONS -F -pgmF debug-pp #-}
module Main where
    import System.IO
    import System.Environment
    import Control.Monad
    import HERBGrammar
    import HERBTokens
    import Text.ParserCombinators.Parsec
    import Text.Parsec.String
    import Text.Parsec
    import Data.List
    import Data.List.Split
    import Data.Typeable
    import Data.Maybe
    import Data.Ord
    import Debug.Trace
    import qualified Data.ByteString.Char8 as C
    import GHC.Exts
   


    {-==============================================================================-}
    {-==================================== TODO ====================================-}
    {-==============================================================================-}


    {-
    Hello. 
    
    I took the liberty of making automarker tables for pr6-10, look in the automarker foler.
    

    New tasks here:
        -- Double check my tests for each problem
        --Test EMPTY CASES as shown in spec pr1 through to pr10, copy over table contents etc
        --WORKING PROGRAMS: Pr1, Pr2, Pr3, Pr4, Pr5, Pr6, Pr9
        --BROKEN PROGRAMS: Pr7, Pr8, Pr10
        -- ^^ Basically any program that self references multiple times. This causes your BigTable to not reflect every assignment, bcos its the NUMBERR OF RELATIONS IN THE QUERY..
        --      ... THAT DECIDES THE BigTable, as far as I understand it. Not really sure tho, COULD BE as easy as cross producting itself as many times (see Pr7) as needed and checking all those rows?
        --Once thats fixed should all fall into place
        --My understanding: 
        --Pr10: actual .cql file parse error somewhere cba to fix it atm
        --2-3page manual
        -}


    -- Also still to look at:

    --PR6: Account for empty input files
    --Add functionality to define empty input file and cause function to add this null value to the xProd table for evaluation
    -- Deal with empty tree cases for pTrees and oTrees
    
   
    {-==============================================================================-}
    {-=============================== DATA STRUCTURES ==============================-}
    {-==============================================================================-}
    
    -- Left hand side of algebra (variables or arguments)
    -- VARIABLES TREE
    data VarTree = CommaNode (VarNode) (VarTree) --Added to force tree structure to right recurse
        | SingleNode (VarNode)  -- VarNode location actualData
        | EmptyVT (EmptyTree)
        deriving Show
        
    data VarNode = Vari (String) (String) (String) -- loc dat name
        deriving Show
    
    -- Right hand side of algebra (query or request)
    -- OPERATORS TREE
    data OpTree = ConjunctionNode (OpTree) (OpTree)
        | RelationNode (String) (VarTree) -- String is Table name.
        | EquateNode (OpTree) (OpTree)
        | BoolNode (Bool)
        | VarOp (VarTree)
        | EmptyOT (EmptyTree)
        | ExistVar (VarTree) (OpTree)
        deriving Show

    -- (EXIS REFORMAT) : Is EmptyPT ever actually needed?
    -- All expressions available within language.
    -- PARSE TREE
    data ParseTree = Marker ([VarNode]) (OpTree)
        | EmptyPT (EmptyTree)
        deriving Show
      --  (1,10,3)E.( ( (1,2)E.Q(x1,x2) ^ (x1 = x2) ) ^ (x3=foo) )
    
    data EmptyTree = Nothing
        deriving Show
    
    {-==============================================================================-}
    {-===================================== MAIN ===================================-}
    {-==============================================================================-}
    main :: IO()
    main = do 
        a <- getArgs
        b <- readFile (head a)
        let herbLang = head (splitOn "\n" b)
        let alex = alexScanTokens (herbLang)
        let happy = parseCalc (alex)
        putStr("_________alex____________")
        putStrLn("")
        print(alex)
        putStr("_____________________")
        let pTree = buildParseTree (happy)
        let pTreeLoc = locPTree (pTree) (getUniqueState (getOTree(pTree)) (False))
        let lhsVar = getOrderOfVars (pTree)
        let tableNames = extractTableNames (pTree)
        putStr("_________tab names____________")
        putStrLn("")
        print(tableNames)
        putStr("_____________________")
        putStr("________pTree____________")
        putStrLn("")
        print(pTree)
        putStr("_____________________")
        putStr("________pTree___loc assigned_________")
        putStrLn("")
        print(pTreeLoc)
        putStr("_____________________")
        allContents <- extractContents readContents tableNames
        let allTables = fmap buildTable allContents
        let noOfCols = getColumn (allTables)
        putStr("__________allTables___________")
        putStrLn("")
        --prettyPrintTable(allTables)
        print(allTables)
        putStr("_____________________")
        -- For future implementation where contentA-N and allTables is built dynamically.
        let exisTable = crossMulti(toVarnodeTables allTables tableNames)
        putStr("__________ExisTables___________")
        putStrLn("")
       -- prettyPrintTable (exisTable)
        print(exisTable)
        putStr("_____________________")
        let noDoubles = beGoneDbls exisTable noOfCols
        putStr("__________No Doubles___________")
        putStrLn("")
        --prettyPrintTable (noDoubles)
        print(noDoubles)
        putStr("_____________________")
        let answer = executeQuery (noDoubles) (exisTable) (pTreeLoc) 
        putStr("_______ANSWER______________")
        putStrLn("")
        prettyPrintTable(answer)
        putStr("_____________________")
        putStrLn("")
        let printable = readyOutput ( extractOutput (removeDups((orderTable (lhsVar) (answer))) ))
        putStr("_____________________")
        putStrLn("")
        --let output = ""
        --if anyEmptyFile (tableNames) then output = "" else output = printable
        mapM_ putStrLn printable                                    
    {-==============================================================================-}
    {-============================== SYNTAX CHECKER ================================-}
    {-==============================================================================-}

    -- *** TODO ** IMPORTANT: Implement a rule ensuring the children of an equality is 2 var nodes. Do we need to do this in our grammar/tree? See next commenr
{-
    --checkBounds
    --checkScope
    expSyntax :: Exp -> Bool
    (Conjunction queryA queryB) = True
    (Relation string variables) = True
    (Equality queryA queryB)) = True
    (Bool bool)) = True
    (V variables)) = True
    (ExistentialSingle variables query) = True
    (Conjunction queryA queryB) = True
    (Relation string variables) = True
    (Equality queryA queryB) = True
    (Bool bool)) = True
    (V variables) = True
    (ExistentialSingle variables query) = True
    expSyntax (Evaluate lefthandside query) = orderSyntax (lefthandSide) && querySyntax (query)

    orderSyntax :: Variables -> Bool
    orderSyntax (Comma string variables) = --?????
    orderSyntax (VarSingle string) = --?????

    querySyntax :: Query -> Bool
    querySyntax (Conjunction queryA queryB)) = True
    querySyntax (Relation string variables)) = True
    querySyntax (Equality queryA queryB)) = True
    querySyntax (Bool bool)) = True
    querySyntax (V variables)) = True
    querySyntax (ExistentialSingle variables query)) = True

    equalityChildren :: Query -> Bool
    equalityChildren (Evaluate (VarSingle string) (Var 
    -}






    prettyPrintTable :: Show a => [[a]] -> IO[()]
    prettyPrintTable (x:xs) = mapM (prettyPrintLine) (x:xs)

    prettyPrintLine :: Show a => [a] -> IO()
    prettyPrintLine [] = putStrLn("=======================================================================================")
    prettyPrintLine x = print(x)
    {-==============================================================================-}
    {-=============================== CSV EXTRACTION ===============================-}
    {-==============================================================================-}

    readContents :: String -> IO [[String]]
    readContents filepath = do
        contents <- readFile (fix filepath)
        let lines = chunksOf 1 (splitOn "\n" contents)
        return lines

    fix :: String -> String
    fix string = charToString(head string) ++ ".csv"

    buildTable :: [[String]] -> [[String]]
    buildTable [] = []
    buildTable ((x:xs):xss) = (splitOn "," x) : buildTable xss 

    anyEmptyFile :: [String] -> IO Bool
    anyEmptyFile [] = do
        let flag = False
        return flag
    anyEmptyFile [oneEl] = do
        bool <- emptyFile oneEl
        return bool
    anyEmptyFile (file:fs) = do
        let listempty = length (file:fs) == 0
        bool <- emptyFile file
        if bool then return bool else anyEmptyFile fs
        
    emptyFile :: String -> IO Bool
    emptyFile filepath = do
        contents <- readFile (filepath ++ ".csv")
        let flag = isFileEmpty contents
        return flag

    isFileEmpty :: String -> Bool
    isFileEmpty string  | length string /= 0 = False
                        | otherwise = True

    extractContents :: (String -> IO [[String]]) -> [String] -> IO [[[String]]]
    extractContents f [] = return []
   -- extractContents f (tableName:xs) = [ f  (if(length (tableName:xs) /= 0) then tableName else "*")] ++ extractContents f (xs)
    extractContents f (tableName:xs) = do
        table <- f tableName 
        rest <- extractContents f (xs)
        return $! table : rest
    
    removeDups :: [[VarNode]] -> [[VarNode]]
    removeDups [] = [[]]
    removeDups (x:xs) = [removeDups' (groupRepeats x)] ++ removeDups xs

    removeDups' :: [[VarNode]] -> [VarNode]
    removeDups' [] = []
    removeDups' (x:xs) =  [head x] ++ removeDups' xs

    {-==============================================================================-}
    {-================================= BUILDING ===================================-}
    {-==============================================================================-} 

    buildParseTree :: Exp -> ParseTree
    buildParseTree (Evaluate (vars) (query)) = Marker (traverseDFVar(buildVarTree(vars)))  (buildOpTree(query))
    --buildParseTree (Eval vars exis) = MarkerNested (traverseDFVar(buildVarTree(vars))) (buildExisTree(exis)) (EXIS REFORMAT)
    
    buildVarTree :: Variables -> VarTree
    buildVarTree (VarSingle strName) = SingleNode (Vari ("*") ("*") (strName))
    buildVarTree (Comma (string) remVars) = CommaNode (Vari ("*") ("*") (string)) (buildVarTree remVars)

    buildOpTree :: Query -> OpTree 
    buildOpTree (Conjunction querA querB) = ConjunctionNode (buildOpTree querA) (buildOpTree querB)
    buildOpTree (ConjunctionTriple querX querY querZ) = (ConjunctionNode (buildOpTree querX) (ConjunctionNode (buildOpTree querY) (buildOpTree querZ)))
    buildOpTree (Relation tblN varis) = RelationNode (removeBracket tblN) (assignVarTreeLoc (buildVarTree varis) (removeBracket tblN))
    --Below (TODO) add type checker for querA/B, checking its a VarTree
    buildOpTree (Equality querA querB) = (EquateNode (varToOpTree(buildVarTree(querA))) (varToOpTree(buildVarTree(querB))))
    buildOpTree (V varis) = VarOp (buildVarTree(varis))
    buildOpTree (ExistentialSingle vTree oTree) = (ExistVar (buildVarTree(vTree)) (buildOpTree (oTree))) --(EXIS REFORMAT)
    buildOpTree _ = EmptyOT (Main.Nothing)

    removeBracket :: String -> String
    removeBracket string | length string == 1 = "Grammar Error in buildOpTree: Table Name Length one."
    removeBracket string | length string == 2 = string
    removeBracket string | length string == 3 = init string
    removeBracket string = string
    {-==============================================================================-}
    {-============================== TABLE OPERATIONS ==============================-}
    {-==============================================================================-}

    getColumn :: [[[String]]] -> Int
    getColumn [] = 0
    getColumn (table:ts) = getColumn' table
    
    getColumn' :: [[String]] -> Int
    getColumn' [] = 0
    getColumn' (row:rs) = length row

    beGoneDbls :: [[VarNode]] -> Int -> [[VarNode]]
    beGoneDbls [] _ = []
    beGoneDbls (row:rs) noOfCols | dblRow row noOfCols == False = row : beGoneDbls rs noOfCols
    beGoneDbls (row:rs) noOfCols | dblRow row noOfCols == True = beGoneDbls rs noOfCols

    dblRow :: [VarNode] -> Int -> Bool
    dblRow [] _ = False
    dblRow list noOfCols = repeatedPhrase (unwrap list) noOfCols

    unwrap :: [VarNode] -> [String]
    unwrap [] = []
    unwrap ((Vari loc dat name):otherCells) = dat : unwrap otherCells

    repeatedPhrase :: [String] -> Int -> Bool
    repeatedPhrase [] _ = False
    repeatedPhrase (string:ss) noOfCols = anyListEqual(chunksOf noOfCols (string:ss))

    anyListEqual :: [[String]] -> Bool
    anyListEqual [] = False
    anyListEqual (list:ls) = checkList list ls || anyListEqual ls

    checkList :: [String] -> [[String]] -> Bool
    checkList _ [] = False
    checkList list (otherLists:ls) | list == otherLists = True
    checkList list (otherLists:ls) | list /= otherLists = False || checkList list ls

    getNthElement :: Int -> [String] -> String
    getNthElement _ [] = "THIS IS AN ERROR IN getNthElement" --remove this
    getNthElement 1 (x : _) = x
    getNthElement ind (_:xs) = getNthElement (ind - 1) xs
    
    getNthVNode :: Int -> [VarNode] -> Maybe VarNode
    getNthVNode _ []       = Data.Maybe.Nothing
    getNthVNode 1 (x : _)  = Just x
    getNthVNode n (_ : xs) = getNthVNode (n - 1) xs
    
    getNthElString :: [[String]] -> Int -> Int -> [String]
    getNthElString [] _ _ = []
    getNthElString (x:xs) goal current | goal == current = x
                                 | goal /= current = getNthElString xs goal (current+1)
    
    --INPUT ROWS                            
    getNthRow :: [[String]] -> Int -> Int -> [String]
    getNthRow [] _ _ = []
    getNthRow (x:xs) goal current | goal == current = x
                                 | goal /= current = getNthRow xs goal (current+1)
    
    --INPUT ROWS
    getNRowFromCrossProd :: [[String]] -> Int -> [String]
    getNRowFromCrossProd table goalRow = getNthRow table  goalRow 0


    {-==============================================================================-}
    {-====================== EXIS REFORMAT NEW TABLE OPERATIONS ====================-}
    {-==============================================================================-}

    extractTableNames :: ParseTree -> [String]
    extractTableNames (Marker ord oTree) = extractRNames oTree ++ extractRExisNames oTree

    extractRNames :: OpTree -> [String] 
    extractRNames (ConjunctionNode (oTA) (oTB)) = extractRNames (oTA) ++ extractRNames(oTB)
    extractRNames (RelationNode (tbl) (varTree)) = [tbl]
    extractRNames (EquateNode (oTA) (oTB)) = extractRNames (oTA) ++ extractRNames(oTB)
    extractRNames (ExistVar (vTree) (oTree)) = []
    extractRNames x = []

    countRNames :: OpTree -> Int
    countRNames oTree = length (extractRNames oTree)

    extractRExisNames :: OpTree -> [String] 
    extractRExisNames (ConjunctionNode (oTA) (oTB)) = extractRExisNames (oTA) ++ extractRExisNames(oTB)
    extractRExisNames (RelationNode (tbl) (varTree)) = []
    extractRExisNames (EquateNode (oTA) (oTB)) = extractRExisNames (oTA) ++ extractRExisNames(oTB)
    extractRExisNames (ExistVar (vTree) (oTree)) = extractRNames (oTree)
    extractRExisNames x = []

    countRExisNames :: OpTree -> Int
    countRExisNames oTree = length (extractRExisNames oTree)
    --Split popTree location assignment so it happens before population

    
    renameRelLocation :: OpTree -> String -> OpTree
    renameRelLocation (RelationNode (tbl) (vTree)) (newTbl) = (RelationNode (newTbl) (renameTreeLocation (vTree) (newTbl)))

    renameTreeLocation :: VarTree -> String -> VarTree 
    renameTreeLocation (SingleNode (Vari loc dat name)) newLoc = (SingleNode (Vari newLoc dat name))
    renameTreeLocation (CommaNode (Vari loc dat name) (remTree)) newLoc = (CommaNode (Vari newLoc dat name) (renameTreeLocation (remTree) (newLoc)) )
    



    toVarnodeTables :: [[[String]]] -> [String] -> [[[VarNode]]]
    toVarnodeTables [] [] = []
    toVarnodeTables (tableA:remTables) (x:xs) = assignTblNm tableA x : toVarnodeTables remTables xs

    assignTblNm :: [[String]] -> String -> [[VarNode]]
    assignTblNm [] _ = []
    assignTblNm (row:rs) name = assignDataAndName row name : assignTblNm rs name

    assignDataAndName :: [String] -> String -> [VarNode]
    assignDataAndName [] _ = []
    assignDataAndName (cell:cs) name = buildVarNode name cell "*" : assignDataAndName cs name

    buildVarNode :: String -> String -> String -> VarNode
    buildVarNode (tblName) (cell) (varName) = Vari (tblName) (cell) (varName)
    
    -- selfRef :: [[VarNode]] -> [[VarNode]]
    -- selfRef [] = []
    -- selfRef (row:rs) = refRow row ++ selfRef rs

    -- refRow :: [VarNode] -> [VarNode]
    -- refRow [] = []
    -- refRow (cell:cs) = refCell cell cs 
    
    -- refCell :: VarNode -> [VarNode] -> VarNode
    -- refCell _ [] = []
    -- refCell (Vari loc dat name) (cell:cs) | dat = --MORE THAN ONE OTHER CELLS DAT????


    crossMulti :: [[[VarNode]]] -> [[VarNode]]
    crossMulti [] = []
    crossMulti [tableA] = tableA
    crossMulti [tableA, tableB] = crossTwo tableA tableB
    crossMulti (t0:t1:ts) = crossMulti (crossTwo (t0) (t1) : ts)

    --Cross two tables.
    crossTwo :: [[VarNode]] -> [[VarNode]] -> [[VarNode]]
    crossTwo _ [] = []
    crossTwo [] _ = []
    crossTwo (rowA:as) (rowB:bs) = pairRowToTable (rowA) (rowB:bs) ++ crossTwo as (rowB:bs)
    
    --Cross a row to an entire table.
    pairRowToTable :: [VarNode] -> [[VarNode]] -> [[VarNode]]
    pairRowToTable _ [] = []
    pairRowToTable rowA (rowB:bs) = [pairRow (rowA) (rowB)] ++ pairRowToTable (rowA) (bs)
    
    pairRow :: [VarNode] -> [VarNode] -> [VarNode]
    pairRow [] _ = []
    pairRow _ [] = []
    pairRow rowA rowB = rowA ++ rowB

    -- --Asked for function here JT: -e

    getUniqueState :: OpTree -> Bool -> [VarNode]
    getUniqueState (ConjunctionNode (oTA) (oTB)) False = getUniqueState oTA (False) ++ getUniqueState oTB (False)
    getUniqueState (RelationNode (string) (varTree)) False = varTreeToList(varTree)--rmDupVars (varTree) 
    getUniqueState (EquateNode (oTA) (oTB)) False = getUniqueState oTA (False) ++ getUniqueState oTB (False)
    getUniqueState (BoolNode (bool)) False = []
    getUniqueState (VarOp (varTree)) False = (varTreeToList(varTree)) -- REMOVE DUPLICATES???!!!!???!!!
    getUniqueState (EmptyOT (emptyTree)) False = []
    getUniqueState (ExistVar (varTree) (oTA)) False = getUniqueState (oTA) (True)

    getUniqueState (ConjunctionNode (oTA) (oTB)) True = getUniqueState oTA (True) ++ getUniqueState oTB (True)
    getUniqueState (RelationNode (string) (varTree)) True = varTreeToList (varTree)
    getUniqueState (EquateNode (oTA) (oTB)) True = getUniqueState oTA (True) ++ getUniqueState oTB (True)
    getUniqueState (BoolNode (bool)) True = []
    getUniqueState (VarOp (varTree)) True = varTreeToList (varTree)
    getUniqueState (EmptyOT (emptyTree)) True = []
    getUniqueState (ExistVar (varTree) (oTA)) True = getUniqueState (oTA) (True)
{-
    rmDupVars :: VarTree -> [VarNode]
    rmDupVars vTree = nub (varTreeToList(vTree))
-}
    varTreeToList :: VarTree -> [VarNode]
    varTreeToList (CommaNode (varNode) (restTree)) = [varNode] ++ varTreeToList restTree
    varTreeToList (SingleNode (varNode)) = [varNode]
    varTreeToList (EmptyVT (emptyTree)) = [] 

    doesListExistInOpTree :: [VarNode] -> OpTree -> Bool
    doesListExistInOpTree (x:xs) oTree = ( doesExistInOpTree (x) (oTree) ) && ( doesListExistInOpTree (xs) (oTree) )
    doesListExistInOpTree [] oTree = True
    
    doesExistInOpTree :: VarNode -> OpTree -> Bool
    doesExistInOpTree (node) (ConjunctionNode (opTree) (opTreeX)) = doesExistInOpTree node opTree || doesExistInOpTree node opTreeX
    doesExistInOpTree (node) (RelationNode (string) (varTree)) = doesExistInVarTree node varTree
    doesExistInOpTree (node) (EquateNode (opTree) (opTreeX)) =  doesExistInOpTree node opTree || doesExistInOpTree node opTreeX
    doesExistInOpTree (node) (VarOp (varTree)) = doesExistInVarTree node varTree
    doesExistInOpTree (node) (ExistVar (vTree) (oTree)) = doesExistInOpTree node oTree --(EXIS REFORMAT) possibly not right depends on purpose of this function
   --I feel like above line should actually ignore this and call checkExistential on the oTree? Maybe?

    doesExistInVarTree :: VarNode -> VarTree -> Bool
    doesExistInVarTree (node) (CommaNode (varNode) (varTree)) = equateNodesDatAndName node varNode || doesExistInVarTree node varTree
    doesExistInVarTree (node) (SingleNode (varNode)) = equateNodesDatAndName node varNode
    doesExistInVarTree (node) (EmptyVT (emptyTree)) = False
    
    getOrderOfVars :: ParseTree -> [VarNode]
    getOrderOfVars (Marker (list) (opTree)) = list
    getOrderOfVars (EmptyPT (emptyTree)) = []
    
    filterTrue :: [(Bool, [VarNode])] -> [[VarNode]]
    filterTrue ((bool, vars):xs) | length xs == 0 = [[]]
    filterTrue ((bool, vars):xs) | bool == True = vars : filterTrue xs
    filterTrue ((bool, vars):xs) | bool == False = filterTrue xs
    
    extractData :: VarNode -> String
    extractData (Vari (loc) (dat) (name)) = dat
    
    extractName :: VarNode -> String
    extractName (Vari (loc) (dat) (name)) = name
    
    -- takes tabletoString and changes all VarNodes to all the dat within using extractData.
    extractTableData :: [[VarNode]] -> [String]
    extractTableData [] = []
    extractTableData (x:xs) = [extractRowData(x)] ++ extractTableData (xs)

    -- varN -> String
    extractRowData :: [VarNode] -> String
    extractRowData [] = ""
    extractRowData (x:xs) = extractData(x) ++ extractRowData (xs)
    
    -- Input list of list of cells
    -- Output a list of rows (cells combined using below function)
    tableToString :: [[String]] -> [String]
    --tableToString [] = []
    tableToString [a] = [] : rowToString a : []
    tableToString [a,b] = [] : rowToString a : rowToString b : []
    tableToString (x:xs) = [] : rowToString x : tableToString xs
    
    {-==============================================================================-}
    {-=============================== MAIN EVALUATION ==============================-}
    {-==============================================================================-}

    readyOutput :: [[String]] -> [String]
    readyOutput [] = []
    readyOutput (row:rs) = sort(rowToString row : readyOutput rs)

    extractOutput :: [[VarNode]] -> [[String]]
    extractOutput [] = []
    extractOutput (row:rs) = extractOutput' row : extractOutput rs  

    extractOutput':: [VarNode] -> [String]
    extractOutput' [] = []
    extractOutput' (v:vs) = extractData v : extractOutput' vs 

    orderTable :: [VarNode] -> [[VarNode]] -> [[VarNode]]
    orderTable _ [] = []
    orderTable order (row:rs) = orderRow order row : orderTable order rs

    --              ORDER OF VARS   ROW IN     ROW OUT, REARRANGED COLUMNS
    orderRow :: [VarNode] -> [VarNode] -> [VarNode] -- Outputs list of rows (i.e. one table)
    orderRow [] _ = []
    orderRow (o:os) list = orderRow' o list ++ orderRow os list

    orderRow' :: VarNode -> [VarNode] -> [VarNode]
    orderRow' v []     = []
    orderRow' v (w:ws) | equateNodesName v w == True = w : orderRow' v ws
    orderRow' v (w:ws) | equateNodesName v w == False = orderRow' v ws

    rowToString :: [String] -> String
    rowToString [] = ""
    rowToString [a] = a
    rowToString [a,b] = a ++ "," ++ b
    rowToString (x:xs) = x ++ "," ++ rowToString xs 
{- Pre- checkExis update
    executeQuery :: [[VarNode]] -> ParseTree -> [[VarNode]] 
    executeQuery [] _ = []
    executeQuery (row:remainingRows) (Marker ordVars oTree)      | (evaluateParseTree (Marker ordVars assignedTree) ) == True   = [assignedRow] ++ executeQuery (remainingRows) (Marker ordVars oTree)
                                                                 | (evaluateParseTree (Marker ordVars assignedTree) ) == False  = executeQuery (remainingRows) (Marker ordVars oTree)
                                                                 where   assignedRow = getTreeState(assignedTree) -- : executeQuery (remainingRows) (pTree)
                                                                         assignedTree = popTree (sanitiseOpTree(oTree)) (row)
-}

    executeQuery :: [[VarNode]] -> [[VarNode]] -> ParseTree -> [[VarNode]] -- PopTable -> ExisTable -> pTree -> OutputRows
    executeQuery [] _ _ = []
    executeQuery (row:remainingRows) (exisTable) (Marker ordVars oTree)         | (evaluateParseTree (Marker ordVars assignedTree) (exisTable) ) == True   = [assignedRow] ++ executeQuery (remainingRows) (exisTable) (Marker ordVars oTree)
                                                                                | (evaluateParseTree (Marker ordVars assignedTree) (exisTable) ) == False  = executeQuery (remainingRows) (exisTable) (Marker ordVars oTree)
                                                                                where   assignedRow = getTreeState(assignedTree) -- : executeQuery (remainingRows) (pTree)
                                                                                        assignedTree = popTree (sanitiseOpTree(oTree)) (row)

    --New ePT: (EXIS REFORMAT)
    evaluateParseTree :: ParseTree -> [[VarNode]] -> Bool -- pTree -> ExisTable -> Boolean output
    evaluateParseTree (Marker ordVars assignedTree) (exisTable) = checkRepeats(filterRepeats(groupRepeats(getTreeState(assignedTree)))) && (evaluate (assignedTree) (exisTable))


    evaluateTree :: OpTree -> [[VarNode]] -> Bool -- oTree -> ExisTable -> Boolean output
    evaluateTree (assignedTree) (exisTable) = checkRepeats(filterRepeats(groupRepeats(getTreeState(assignedTree)))) && (evaluate (assignedTree) (exisTable))


    --Are all nodes in list . NB ************* Not sure of ">", ">=" ie what combination *******************
    areRepeats :: [VarNode] -> Int -> Bool
    areRepeats [] _ = False --Is this ever called?? -> Nope
    areRepeats ( (Vari (loc) (dat) (name)) : xs) ind    | ind < length totalList =  checkAllDataSame (totalList) (dat)-- && (areRepeats (totalList) (ind+1))
                                                        | ind >= length totalList = checkAllDataSame (totalList) (dat)
                                                        where   repeats = getRepeats (totalList) (1)
                                                               -- matches = matchNodeFromName repeats name   
                                                                totalList = ( (Vari (loc) (dat) (name)) : xs)

    --Rewritten areRepeats
    checkRepeats :: [[VarNode]] -> Bool
    checkRepeats [] = True
    checkRepeats (x:xs) = isAllDataSame x && checkRepeats xs


    filterRepeats :: [[VarNode]] -> [[VarNode]]
    filterRepeats [] = []
    filterRepeats (x:xs)    | length x > 1 = [x] ++ filterRepeats xs
                            | length x == 1 = filterRepeats xs

    isAllDataSame :: [VarNode] -> Bool --New higher order for ease of use (fixes checkrepeats comp error)
    isAllDataSame [] = checkAllDataSame [] "irrelevant"
    isAllDataSame ((Vari (loc) (dat) (name)):xs) = checkAllDataSame ((Vari (loc) (dat) (name)):xs) dat

    checkAllDataSame :: [VarNode] -> String -> Bool
    checkAllDataSame [] _ = True
    checkAllDataSame ((Vari (loc) (datA) (name)):xs) dat = ( dat == datA ) && ( checkAllDataSame xs dat)
    
    getRepeats :: [VarNode] -> Int ->  [VarNode] --return repeated namez
    getRepeats [] _ = []
    getRepeats (x:xs) ind   | (ind <= length(x:xs)) && (countInstancesInVarList (fromJust(getNthVNode (ind) (x:xs) )) (x:xs) > 1) = [x] ++ getRepeats (x:xs) (ind+1)
                            | (ind <= length(x:xs)) && (countInstancesInVarList (fromJust(getNthVNode (ind) (x:xs) )) (x:xs) == 1) = getRepeats (x:xs) (ind+1)
                            | otherwise = [] -- should never be called
  
    --Current attempt, deleted other auxilary functions
    groupRepeats :: [VarNode] -> [[VarNode]]
    groupRepeats (v:vs) = groupWith (extractName) (v:vs) 

    equalityTest :: VarNode -> VarNode -> Bool
    equalityTest (Vari (lA) (dA) (nA)) (Vari (lB) (dB) (nB)) | nA == nB = True
                                                             | nA /= nB = False

    countInstancesInVarList :: VarNode -> [VarNode] -> Int
    countInstancesInVarList vN [] = 0
    countInstancesInVarList vN (x:xs)   | (equateName vN x) == True = 1 + countInstancesInVarList (vN) (xs)
                                        | (equateName vN x) == False = 0 + countInstancesInVarList (vN) (xs)
    
    equateName :: VarNode -> VarNode -> Bool
    equateName (Vari (loc) (dat) (name)) (Vari (locB) (datB) (nameB)) = ( (dat == datB) && (name == nameB) )
 
    evaluate :: OpTree -> [[VarNode]] -> Bool --evaluate opTree exisTable output
    evaluate (EquateNode (l) (r)) (exTable)   =  ( checkEquality (EquateNode (l) (r))) 
    evaluate (RelationNode (loc) (varTr)) (exTable)  = checkRelation (RelationNode (loc) (varTr))
    evaluate (ConjunctionNode (l) (r)) (exTable)  = checkConjunction (ConjunctionNode (l) (r)) (exTable)
    evaluate (ExistVar vTree oTree) (exTable) = checkExistential (ExistVar vTree oTree) (exTable)
    evaluate (VarOp v) (exTable) = True

    checkConjunction :: OpTree -> [[VarNode]] -> Bool
    checkConjunction  (ConjunctionNode (l) (r) ) (exTable)  = (evaluate (l) (exTable)) && (evaluate (r) (exTable))
    


    checkRelation :: OpTree -> Bool
    checkRelation (RelationNode (tbl) (vList)) = isTreePopulated(vList)
    
    -- *** TODO ** IMPORTANT: Implement a rule ensuring the children of an equality is 2 var nodes. Do we need to do this in our grammar/tree? See next commenr
    checkEquality :: OpTree -> Bool
    --checkEquality (EquateNodes (VarOp (SingleNode(v))) (restN) ) = evaluate (VarOp (SingleNode(v))) && checkEquality(restN)
    checkEquality (EquateNode (l) (r)) = equateNodes left right
        where   left = convertOpToVarNode (l)
                right = convertOpToVarNode (r)

    {-==============================================================================-}
    {-=========================== TREE & NODE OPERATIONS ===========================-}
    {-==============================================================================-}

    -- highestOrder :: ParseTree -> ParseTree
    -- highestOrder (Marker (vars) (oTree)) = secondOrder oTree (Marker (vars) (oTree)) 

    -- secondOrder :: OpTree -> ParseTree -> OpTree
    -- secondOrder opTree pTree =   renameTree opTree renamedTblNms
    --                         where   renamedTblNms = renameTblName tblNms
    --                                 tblNms = extractTableNames pTree
    -- -- extract table names
    -- gather dup table names
    -- create new table names (depending on exis or nonexis maybe?)
    -- parse list of table names with int flag and go through all OpTree first S leave it the same, second S when int flag is f
    -- renameTree :: OpTree -> [String] -> OpTree
    -- renameTree  (ConjunctionNode (opTree) (opTreeX)) = 
    -- renameTree  (RelationNode (string) (varTree)) =
    -- renameTree  (EquateNode (opTree) (opTreeX)) = 
    -- renameTree  (BoolNode (bool)) = (BoolNode bool)
    -- renameTree  (VarOp (varTree)) = 
    -- renameTree  (EmptyOT (emptyTree)) = (EmptyOT emptyTree)
    -- renameTree  (ExistVar (vTree) (oTree)) = 

    getOTree :: ParseTree -> OpTree
    getOTree (Marker (vars) (oTree)) = oTree

    getPTreeState :: ParseTree -> [VarNode]
    getPTreeState (Marker (vars) (oTree)) = getTreeState (oTree)
    
    getTreeState :: OpTree -> [VarNode]
    getTreeState (ConjunctionNode (opTree) (opTreeX)) = getTreeState(opTree) ++ getTreeState (opTreeX)
    getTreeState (RelationNode (string) (varTree)) = getTreeState(varToOpTree(varTree))
    getTreeState (EquateNode (opTree) (opTreeX)) = getTreeState (opTree) ++ getTreeState (opTreeX)
    getTreeState (BoolNode (bool)) = []
    getTreeState (VarOp (varTree)) = traverseDFVar(varTree)
    getTreeState (EmptyOT (emptyTree)) = []
    getTreeState (ExistVar (vTree) (oTree)) = getTreeState(varToOpTree(vTree)) ++ getTreeState (oTree) --Possibly not what we need (EXIS REFORMAT)

    getTreeStateNoEx :: OpTree -> [VarNode]
    getTreeStateNoEx (ConjunctionNode (opTree) (opTreeX)) = getTreeStateNoEx(opTree) ++ getTreeStateNoEx (opTreeX)
    getTreeStateNoEx (RelationNode (string) (varTree)) = getTreeStateNoEx(varToOpTree(varTree))
    getTreeStateNoEx (EquateNode (opTree) (opTreeX)) = getTreeStateNoEx (opTree) ++ getTreeStateNoEx (opTreeX)
    getTreeStateNoEx (BoolNode (bool)) = []
    getTreeStateNoEx (VarOp (varTree)) = traverseDFVar(varTree)
    getTreeStateNoEx (EmptyOT (emptyTree)) = []
    getTreeStateNoEx (ExistVar (vTree) (oTree)) = [] --Possibly not what we need (EXIS REFORMAT)

    -- New checkExis below along with needed aux functions
    checkExistential :: OpTree -> [[VarNode]] -> Bool --oTree -> ExisTable 
    checkExistential _ [] = False
    checkExistential (ExistVar vTree oTree) (ex:exs) = evaluateAssignTree (oTree) (ex) (ex:exs) || checkExistential (ExistVar vTree oTree) (exs)  

    evaluateAssignTree :: OpTree -> [VarNode] -> [[VarNode]] -> Bool
    evaluateAssignTree oTree rList exTable = evaluateTree (popTreeEX (sanitiseOpTree(oTree)) (rList)) (exTable)

    -- checkExisTInOpT :: VarTree -> OpTree -> Bool
    -- checkExisTInOpT (SingleNode (vNode)) oTree = checkExisInOpTree vNode oTree
    -- checkExisTInOpT (CommaNode (vNode) (rem Tree)) oTree = (checkExisInOpTree (vNode) (oTree)) && (checkExisTInOpT (remTree) (oTree))
    
    
    -- checkExisInOpTree :: VarNode -> OpTree -> Bool
    -- checkExisInOpTree vNode oTree = checkExisInOpTreeList (vNode) (extractAssignedNodes(getUniqueState (oTree) (False)))

    -- checkExisInOpTreeList :: VarNode -> [VarNode] -> Bool
    -- checkExisInOpTreeList (Vari loc dat name) rList = existsExactNode (rList) (loc) (name)

-- ::::::::::::::::::::::::::::::::::::::::::::::populateTree attempt 4:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
{-
General order of execution:
pre pass check          : checkBounds rule applied + existential Scope rule potentially??
                        : assign eqNode vNodes locations with assignEqLocation
1st pass of population  : populates varnodes encapsulated within a relation eg R(x1,z2)
2nd pass of population  : populated varnodes outside of relation eg x1 = x2, or (z1,z2)E.(..) data and location


--Consider case         : x1,x2 |- (R{x1,x2} ^ x1 = x2)
                        : 
--unsolved problem: in case (z1,z2)E.(R{z1,z2} ^ z1 = z2) how do we specify scope of the exis variables -  as a different variable of same name could exist in query

-}
    popTree :: OpTree -> [VarNode] -> OpTree
    popTree oTree rList = popTreeNextPass postFstTree uniqueState
                        where   postFstTree = (popTreeFirstPass (oTree) (rList))
                                uniqueState = (extractAssignedNodes(getUniqueState (postFstTree) (False)))
    
    popTreeFirstPass :: OpTree -> [VarNode] -> OpTree
    popTreeFirstPass (VarOp (vTree)) rList   = (VarOp (vTree))-- Case example: x1  =  x2 ^ ... |||| To be left for 2nd pass
    popTreeFirstPass (ConjunctionNode (querA) (querB)) rList = (ConjunctionNode (popTreeFirstPass querA rList) (popTreeFirstPass querB rList)) 
    popTreeFirstPass (EquateNode (querX) (querY)) rList = (EquateNode (popTreeFirstPass querX rList) (popTreeFirstPass querY rList))
    popTreeFirstPass (RelationNode (tbl) (vTree)) rList = popRelation (RelationNode (tbl) (vTree)) (rList)
    popTreeFirstPass (ExistVar (vTree) (oTree)) rList = (ExistVar (vTree) (oTree)) --(ExistVar (vTree) (popTreeFirstPass oTree rList) )

    popTreeNextPass :: OpTree -> [VarNode] -> OpTree
    popTreeNextPass (VarOp (vTree)) rList   = (VarOp (SingleNode (Vari "***LINE672***" "ERROR CASE" "shouldnt be happening"))) -- Does this case ever occur
    popTreeNextPass (ConjunctionNode (querA) (querB)) rList = (ConjunctionNode (popTreeNextPass querA rList) (popTreeNextPass querB rList)) 
    popTreeNextPass (EquateNode (querX) (querY)) rList = popEquateNode (EquateNode (querX) (querY)) rList--popEquateNode
    popTreeNextPass (RelationNode (tbl) (vTree)) rList = (RelationNode (tbl) (vTree)) --Already populated so left alone
    popTreeNextPass (ExistVar (vTree) (oTree)) rList = (ExistVar (vTree) (oTree)) --popExistNode (ExistVar (vTree) (oTree)) rList

        
    popTreeEX :: OpTree -> [VarNode] -> OpTree
    popTreeEX (VarOp (vTree)) rList   = (VarOp (vTree))
    popTreeEX (ConjunctionNode (querA) (querB)) rList = (ConjunctionNode (popTreeEX querA rList) (popTreeEX querB rList)) 
    popTreeEX (EquateNode (querX) (querY)) rList = (EquateNode (popTreeEX querX rList) (popTreeEX querY rList))
    popTreeEX (RelationNode (tbl) (vTree)) rList = popRelation (RelationNode (tbl) (vTree)) (rList)
    popTreeEX (ExistVar (vTree) (oTree)) rList = (ExistVar (vTree) (popTreeFirstPass oTree rList) )


    locPTree :: ParseTree -> [VarNode] -> ParseTree
    locPTree (Marker (ordVars) (oTree)) rList = Marker ordVars (locTree (oTree) (rList))

    locTree :: OpTree -> [VarNode] -> OpTree
    locTree (ConjunctionNode (querA) (querB)) rList = (ConjunctionNode (locTree (querA) rList) (locTree (querB) rList))
    locTree (EquateNode (querX) (querY)) rList = (EquateNode (assignLocationInOTree querX rList) (assignLocationInOTree querY rList))
    locTree (ExistVar (vTree) (oTree)) rList = locExistNode (ExistVar (vTree) (oTree)) rList
    locTree x _ = x

    locExistNode :: OpTree -> [VarNode] -> OpTree
    locExistNode (ExistVar (vTree) (oTree)) rList =  (ExistVar (assignLocationInTree (vTree) (getUniqueState(oTree) (False)) ) (locTree oTree rList)) 


    popExistNode :: OpTree -> [VarNode] -> OpTree
    popExistNode (ExistVar (vTree) (oTree)) rList = (ExistVar (popBoundVTree (scopeVTree) (rList)) (popTreeNextPass (oTree) (rList)))
                                                    where   scopeList = extractAssignedNodes(getUniqueState (oTree) (False))
                                                            scopeVTree = assignLocationInTree (vTree) (scopeList)

    assignLocationInOTree :: OpTree -> [VarNode] -> OpTree
    assignLocationInOTree (VarOp (vTree)) rList = VarOp (assignLocationInTree (vTree) (rList))

    assignLocationInTree :: VarTree -> [VarNode] -> VarTree
    assignLocationInTree (SingleNode (Vari loc dat name)) rList = (SingleNode (Vari (getNodeLocation (name) (rList)) dat name))
    assignLocationInTree (CommaNode (Vari loc dat name) (remTree)) rList = (CommaNode (Vari (getNodeLocation (name) (rList)) dat name) (assignLocationInTree (remTree) (rList)))
    
    popEquateNode :: OpTree -> [VarNode] -> OpTree
    popEquateNode (EquateNode (VarOp (SingleNode(Vari (lloc) (ldat) (lname)))) (VarOp(SingleNode(Vari (rloc) (rdat) (rname))))) rList = (EquateNode (VarOp(SingleNode(lPopped))) (VarOp(SingleNode(rPopped))))
                                                                                                            where   lPopped = fromJust (extractExactNode rList lloc lname)
                                                                                                                    rPopped = fromJust (extractExactNode rList rloc rname)

    

    --2nd pass only, for outside of a relation
    popBoundOTree :: OpTree -> [VarNode] -> OpTree
    popBoundOTree (VarOp (SingleNode (vNode))) (rList) = (VarOp(popBoundVTree (SingleNode(vNode)) (rList)))
    popBoundOTree (VarOp (CommaNode (vNode) (remTree))) (rList) = (VarOp(popBoundVTree (CommaNode (vNode) (remTree)) rList))

    popBoundVTree :: VarTree -> [VarNode] -> VarTree
    popBoundVTree (SingleNode (vNode)) rList = SingleNode (popBoundVNode (vNode) (rList))
    popBoundVTree (CommaNode (vNode) (remTree)) rList = CommaNode (popBoundVNode (vNode) (rList)) (popBoundVTree remTree rList) 

    popBoundVNode :: VarNode -> [VarNode] -> VarNode --2nd pass only, MAYBE additional check to ensure all values are populated
    popBoundVNode (Vari loc dat name) rList | isNothing (extractExactNode (rList) (loc) (name)) == True = (Vari loc dat name)
                                            | otherwise = fromJust(extractExactNode rList loc name)

    --assignScope :: OpTree -> OpTree

    extractAssignedNodes :: [VarNode] -> [VarNode]
    extractAssignedNodes [] = []
    extractAssignedNodes (x:xs) | isNodeAssigned x == True = [x] ++ extractAssignedNodes xs
                                | isNodeAssigned x == False = extractAssignedNodes xs


    getNodeLocation :: String -> [VarNode] -> String -- name -> rList -> location
    getNodeLocation _ [] = "*" -- Elliott : you can probably error check when this case occurs cos all varN's should have a location after population (need to implement that tho)
    getNodeLocation name (x:xs) | equateNodeToName x name == True = unwrapNodeLocation x
                                | equateNodeToName x name == False = getNodeLocation name xs

    unwrapNodeLocation :: VarNode -> String
    unwrapNodeLocation (Vari loc dat name) = loc 

    equateNodeToName :: VarNode -> String -> Bool
    equateNodeToName (Vari locA datA nameA) nameB = ( nameA) == ( nameB)

    extractExactNode :: [VarNode] -> String -> String -> Maybe VarNode --rList -> loc -> name -> outputNode
    extractExactNode [] _  _ = Data.Maybe.Nothing
    extractExactNode (x:xs) loc name    | matchLocName x loc name == True = Just x
                                        | matchLocName x loc name == False = extractExactNode xs loc name

    existsExactNode :: [VarNode] -> String -> String -> Bool
    existsExactNode [] _ _ = False
    existsExactNode (x:xs) loc name = matchLocName x loc name || existsExactNode xs loc name

    matchLocName :: VarNode -> String -> String -> Bool 
    matchLocName (Vari thisLoc thisDat thisName) loc name = (thisLoc == loc) && (thisName == name)

        
    equateNodes :: VarNode -> VarNode -> Bool
    equateNodes (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))   | datA == datB = True
                                                                            | datA /= datB = False

    equateNodesDatAndName :: VarNode -> VarNode -> Bool
    equateNodesDatAndName (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB)) | datA == datB && nameA == nameB = True
                                                                                    | otherwise = False
    equateNodesName :: VarNode -> VarNode -> Bool
    equateNodesName (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))   | nameA == nameB = True
                                                                                | nameA /= nameB = False
    
    equateNodesLoc :: VarNode -> VarNode -> Bool
    equateNodesLoc (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))    | locA == locB = True
                                                                                | locA /= locB = False

    filterNodesByTable :: [VarNode] -> String -> [VarNode]
    filterNodesByTable [] _ = []
    filterNodesByTable ((Vari loc dat name):xs) tblName     | loc == tblName = [(Vari loc dat name)] ++ filterNodesByTable xs tblName
                                                            | loc /= tblName = filterNodesByTable xs tblName

    getNodeAtNameAndLoc :: [VarNode] -> String -> String -> Maybe VarNode
    getNodeAtNameAndLoc [] _ _ = Data.Maybe.Nothing
    getNodeAtNameAndLoc ((Vari loc dat name):xs) thisLoc thisName   | loc == thisLoc && name == thisName = Just (Vari loc dat name)
                                                                    | otherwise = getNodeAtNameAndLoc xs thisLoc thisName

    getNodeAtName :: [VarNode] -> String -> Maybe VarNode
    getNodeAtName [] _ = Data.Maybe.Nothing
    getNodeAtName ((Vari loc dat name):xs) thisName | name == thisName = Just (Vari loc dat name)
                                                    | name /= thisName = getNodeAtName xs thisName

    popRelation :: OpTree -> [VarNode] -> OpTree --RelatioNode case only
    popRelation (RelationNode (tbl) (vTree)) rList = (RelationNode (tbl) (populateVarTree (vTree) (reverse (extractOutput'((filterNodesByTable (rList) (tbl))))) 0 ) )
        --(RelationNode (tbl) (populateVarTree (vTree) (extractOutput'(rList)) 0 ) )
        --(RelationNode (tbl) (popBoundVTree vTree rList) )

    populateVarTree :: VarTree -> [String] -> Int -> VarTree
    populateVarTree (EmptyVT e) _ _ = (EmptyVT e)
    populateVarTree (SingleNode (Vari (loc) (dat) (name))) (x:xs) ind | isNodePopulated (Vari (loc) (dat) (name)) == False = (SingleNode (Vari (loc) (generateNextVarData (x:xs) (ind)) (name) ))
    populateVarTree (SingleNode (Vari (loc) (dat) (name))) (x:xs) ind | isNodePopulated (Vari (loc) (dat) (name)) == True = (SingleNode (Vari (loc) (dat) (name)))
    populateVarTree ( CommaNode (Vari (loc) (dat) (name)) (remTree) ) (x:xs) ind  | isNodePopulated (Vari (loc) (dat) (name)) == False = ( CommaNode (Vari (loc) (generateNextVarData (x:xs) (ind)) (name)) ( populateVarTree (remTree) (x:xs) (ind+1) ) )
    populateVarTree ( CommaNode (Vari (loc) (dat) (name)) (remTree) ) (x:xs) ind  | isNodePopulated (Vari (loc) (dat) (name)) == True = (CommaNode (Vari (loc) (dat) (name)) (remTree)) --add recursive call here
    populateVarTree e _ _ = e

    --REMEMBER: NON BOUND VARIABLES, eg (z1,z2 (E. (R(z1,z2)))), CAN BE DUPLICATED ELSEWHERE IN QUERY AND SHOULDNT BE REMOVED FROM THE LIST, AS THEYRE UNIQUE
    --Separate function populating vTree in ExistVar AFTER first round of population, so the relavant table names can be easily extracted:
    --postPopTreePass :: OpTree -> [VarNode] -> OpTree

    existsTreeInRelation :: VarTree -> Bool
    existsTreeInRelation (SingleNode (Vari loc dat name))  = existsInRelation (Vari loc dat name)    
    existsTreeInRelation (CommaNode (Vari loc dat name) (rTree) ) = existsInRelation (Vari loc dat name) && existsTreeInRelation rTree                                

    existsInRelation :: VarNode -> Bool
    existsInRelation (Vari loc dat name) = loc /= "*"

    countNodes :: OpTree -> Int
    countNodes (ConjunctionNode (querA) (querB)) = countNodes (querA) + countNodes (querB)
    countNodes (EquateNode (querA) (querB))      = countNodes (querA) + countNodes (querB)
    countNodes (RelationNode (lbl) (vTree))      = countNodesV (vTree)
    countNodes (VarOp (vTree))                   = countNodesV (vTree)

    countNodesV :: VarTree -> Int
    countNodesV (SingleNode (vNode)) = 1
    countNodesV (CommaNode (vNode) (remTree)) = 1 + countNodesV (remTree)
   
    sanitiseOpTree :: OpTree -> OpTree
    sanitiseOpTree (RelationNode (tblName) (vTree)) = (RelationNode (tblName) (sanitiseVarTree(vTree)))
    sanitiseOpTree (VarOp (vT)) = (VarOp (sanitiseVarTree(vT)))
    sanitiseOpTree (ConjunctionNode (querA) (querB)) = (ConjunctionNode (sanitiseOpTree (querA)) (sanitiseOpTree(querB)) )
    sanitiseOpTree (EquateNode (querX) (querY)) = (EquateNode (sanitiseOpTree (querX)) (sanitiseOpTree (querY)) )
    sanitiseOpTree (ExistVar (vTree) (oTree)) = (ExistVar (sanitiseVarTree (vTree)) (sanitiseOpTree (oTree)))
    
    sanitiseVarTree :: VarTree -> VarTree
    sanitiseVarTree (SingleNode ( Vari (loc) (dat) (name) ) ) = (  SingleNode (  Vari (loc) ("*") (name)  )  )
    sanitiseVarTree (CommaNode ( Vari (loc) (dat) (name) ) (remTree) ) = (CommaNode ( Vari (loc) ("*") (name) ) (sanitiseVarTree remTree) )


    generateNextVarData :: [String] -> Int -> String
    generateNextVarData (x:xs) ind  | ind <= length(x:xs) && isJust(itemOf (ind+1) (x:xs)) = fromJust(itemOf (ind+1) (x:xs))  --( getNthElement (ind+1) (x:xs) ) --possibly unsafe, maybe use getNth
                                    | ind == length(x:xs) && isJust(itemOf (ind) (x:xs)) = fromJust(itemOf (ind) (x:xs))
                                    | ind > length(x:xs) = "THIS IS AN ERROR AND SHOULD NOT BE HERE (genNextVar)"
                                    | otherwise = "THIS REALLY REALLY SHOULDNT BE HERE : Last case scenario (getNextVar)"
    
    itemOf :: Int -> [a] -> Maybe a; x `itemOf` xs = let xslen = length xs in if ((abs x) > xslen) then Data.Maybe.Nothing else Just (xs !! (x `mod` xslen))

    isTreePopulated :: VarTree -> Bool
    isTreePopulated (SingleNode vNode) = isNodePopulated vNode
    isTreePopulated (CommaNode vNode remTree) = isNodePopulated vNode && isTreePopulated  remTree 
    
    isNodePopulated :: VarNode -> Bool
    isNodePopulated (Vari (loc) (dat) (name))       | dat == "*"  = False-- Represents unassiged null value 
                                                    | otherwise = True
    
    isTreeAssigned :: VarTree -> Bool
    isTreeAssigned (SingleNode vNode) = isNodeAssigned vNode
    isTreeAssigned (CommaNode vNode remTree) = isNodeAssigned vNode && isTreeAssigned remTree 
    
    isNodeAssigned :: VarNode -> Bool
    isNodeAssigned (Vari (loc) (dat) (name))    | loc == "*"  = False-- Represents unassiged null value 
                                                | otherwise = True
    queryToVariables :: Query -> Variables
    queryToVariables (V (Comma varA varB))  = (Comma varA varB)
    queryToVariables (V (VarSingle string)) = (VarSingle string)
    
    varToOpTree :: VarTree -> OpTree
    varToOpTree (CommaNode varN varT) = (VarOp (CommaNode varN varT))
    varToOpTree (SingleNode varN) = (VarOp (SingleNode varN))
    varToOpTree (EmptyVT emptyT) = (VarOp (EmptyVT emptyT))

    charToString :: Char -> String
    charToString = (:[])


    -- Removes multiple instances of table names. Only need to read the file once. [Removed : Eq String => on type sig]
    extractDups :: [String] -> [String]
    extractDups = rdHelper []
        where rdHelper seen [] = seen
              rdHelper seen (x:xs)
                  | x `elem` seen = rdHelper seen xs
                  | otherwise = rdHelper (seen ++ [x]) xs

    extractPTableNames :: ParseTree -> [String]
    extractPTableNames (Marker (vars) (oTree)) = extractOTableNames(getOpRelationNodesOut(oTree))
    --extractPTableNames (MarkerNested (vars) (eTree)) = extractETableNames(eTree) (EXIS REFORMAT) 
    
    extractOTableNames :: [OpTree] -> [String] -- takes output from liftRelationNodesOut, possibly needs to be reverse
    extractOTableNames [] = []
    extractOTableNames ( (RelationNode (tbl) (vTree)) :xs) = (tbl) : extractOTableNames xs
    extractOTableNames ((ExistVar vTree oTree):xs) = extractOTableNames (oTree:xs)
    --Elliott: Pls add cases for all other oTrees, otherwise it wont traverse correctly

    assignVarTreeLoc :: VarTree -> String -> VarTree
    assignVarTreeLoc (SingleNode (Vari (loc) (dat) (name))) x = (SingleNode (Vari (x) (dat) (name)))
    assignVarTreeLoc (CommaNode (Vari (loc) (dat) (name)) (remTree)) x = (CommaNode (Vari (x) (dat) (name)) (assignVarTreeLoc (remTree) (x)))
    
    assignAssignment :: OpTree -> String -> OpTree
    assignAssignment (RelationNode (ass) (vTree) ) newAss = (RelationNode (newAss) (vTree))
    
    assignVarNodeVal :: VarNode -> String -> VarNode
    assignVarNodeVal (Vari (loc) (dat) (name)) newDat = (Vari (loc) (newDat) (name))
    
    assignRelation :: OpTree -> String -> OpTree --Will only assign if not signed before. If loc already exists then....what??
    assignRelation (RelationNode (tbl) (vTree)) relName | isTreeAssigned (vTree) == False = RelationNode (tbl) (assignVarTreeLoc (vTree) (tbl))
                                                        | otherwise = (RelationNode (tbl) (vTree)) --check node equality here

    liftRelationNodesOut :: ParseTree -> [OpTree] --Creates list of single node OpTree's, 
    liftRelationNodesOut (Marker vList oTree) = getOpRelationNodesOut(oTree)

    getOpRelationNodesOut :: OpTree -> [OpTree] --Relation nodess are never subtrees of "="
    getOpRelationNodesOut (RelationNode (tbl) (vTree)) = [(RelationNode (tbl) (vTree))]
    getOpRelationNodesOut (ConjunctionNode (querA) (querB)) = getOpRelationNodesOut(querA) ++ getOpRelationNodesOut(querB)
    getOpRelationNodesOut (ExistVar (vTree) (oTree)) = getOpRelationNodesOut(oTree)
    getOpRelationNodesOut _ = []                  
    {-==============================================================================-}
    {-=============================== TREE TRAVERSAL ===============================-}
    {-==============================================================================-}
    
    traverseDFVar :: VarTree -> [VarNode] --Int represents ORDER (NB: this is why I decided to add VarNode)
    traverseDFVar (SingleNode (node) )  = [(treeToNode (SingleNode (node)))]  ++ []
    traverseDFVar (CommaNode (nextVar) (remainingTree)) = [nextVar] ++ traverseDFVar remainingTree
    
    {-========================== HANDLING FOR TREES ================================-}
    
    treeToNode :: VarTree -> VarNode
    treeToNode (SingleNode (Vari (loc) (dat) (name))) = Vari (loc) (dat) (name)
    treeToNode (CommaNode (node) (remainingTree)) = error "Variable tree contains multiple nodes. Cannot convert to single node."--Unsure of error notation or if this will work but throw an error here (***TODO***)
    
    --toIndexedList :: (VarTree) -> [(Int, VarNode)]
    --toIndexedList lst = zip [1..] traverseDFVar(lst)
    
    -- Blindly assumes OpTree contains a VarTree containing only one VarNode.
    convertOpToVarNode :: OpTree -> VarNode
    convertOpToVarNode (VarOp (vTree)) = treeToNode (vTree)
