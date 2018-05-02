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


    --Begin testing existential cases
    --Make assignTree functions smarter: Differentiate when varName & tableN are different, eg in cases of R(...) ^ R (...), or cases like A(...) ^ B (...) ^ A(...)
    --3 page user manual
    --Informative error messages/type system
    --Rework Exis to allow (x1,x2,x3)E.(Query) - ALREADY DONE
    --PR6: Account for empty input files
    --PR10: Potential issue: Scope of var assignments when checking pairs (checkRepeats/groupRepeats)
    --Add functionality to define empty input file and cause function to add this null value to the xProd table for evaluation
    -- Deal with empty tree cases for pTrees and oTrees
    --possible getRelationData function? Would return all current assignments from a given table as a row
    --Remove ambiguity with exis cases (as in pr6) so we get the correct semantics and convert to a form our interpreter can read (exis on left)
    -- WE MIGHT NEED TO MOVE EXIS BACK INTO QUERY :( :( :( ...
    -- ... SEE PR6: Our interpreter pTree structure must account for an exis being anywhere in a query, but we could convert all to left maybe?
    -- Evaluate exis
    --getRelationState :: 

    --EXIS REFORMAT LEFT TO DO:
    --executeQuery, populateTree (smart), checkExistetial, evalutateExis
    --checkBound
   
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
        | LSubNode (OpTree) (OpTree) -- JT CAN WE REMOVE THESE?
        | RSubNode (OpTree) (OpTree)
        | BoolNode (Bool)
        | VarOp (VarTree)
        | EmptyOT (EmptyTree)
        | ExistVar (VarTree) (OpTree)
        deriving Show

    -- (EXIS REFORMAT) : Is EmptyPT ever actually needed?
    -- All expressions available within language.
    -- PARSE TREE
    data ParseTree = Marker ([VarNode]) (OpTree)
        -- | MarkerNested ([VarNode]) (ExistTree)
      --  | MarkerExisExtended ([VarNode]) (ExistTree) (OpTree) --Added to account for PR6 issues UPDT: probs wont work
        | EmptyPT (EmptyTree)
        deriving Show
      --  (1,10,3)E.( ( (1,2)E.Q(x1,x2) ^ (x1 = x2) ) ^ (x3=foo) )
    
    data EmptyTree = Nothing
        deriving Show
    
    {-==============================================================================-}
    {-===================================== MAIN ===================================-}
    {-==============================================================================-}
   -- main :: IO()
    main = do 
        a <- getArgs
        b <- readFile (head a)
        let herbLang = head (splitOn "\n" b)
        let alex = alexScanTokens (herbLang)
        let happy = parseCalc (alex)
        let pTree = buildParseTree (happy)
        let lhsVar = getOrderOfVars (pTree)
        let tableNames = extractTableNames (pTree)
        allContents <- extractContents readContents tableNames
        let allTables = fmap buildTable allContents

        -- For future implementation where contentA-N and allTables is built dynamically.
        let bigTable = crossMulti(allTables)
        let answer = executeQuery (bigTable) (pTree)
        let output = readyOutput ( extractOutput (orderTable (lhsVar) (answer)))
        putStr("_____________________")
        mapM_ putStrLn output      

                                         
    {-==============================================================================-}
    {-============================== SYNTAX CHECKER ================================-}
    {-==============================================================================-}

    --checkBounds
    --checkScope
    {-
    checkSyntax :: Exp -> Bool
    checkSyntax (Evaluate (Comma String Variables) (Conjunction Query Query)) = True
    checkSyntax (Evaluate (Comma String Variables) (Relation String Variables)) = True
    checkSyntax (Evaluate (Comma String Variables) (Equality Query Query)) = True
    checkSyntax (Evaluate (Comma String Variables) (Bool Bool)) = True
    checkSyntax (Evaluate (Comma String Variables) (V Variables)) = True
    checkSyntax (Evaluate (Comma String Variables) (ExistentialSingle Variables Query)) = True
    checkSyntax (Evaluate (VarSingle String) (Conjunction Query Query)) = True
    checkSyntax (Evaluate (VarSingle String) (Relation String Variables)) = True
    checkSyntax (Evaluate (VarSingle String) (Equality Query Query)) = True
    checkSyntax (Evaluate (VarSingle String) (Bool Bool)) = True
    checkSyntax (Evaluate (VarSingle String) (V Variables)) = True
    checkSyntax (Evaluate (VarSingle String) (ExistentialSingle Variables Query)) = True
    -}

  
    {-==============================================================================-}
    {-=============================== CSV EXTRACTION ===============================-}
    {-==============================================================================-}

    readContents :: String -> IO [[String]]
    readContents filepath = do
        contents <- readFile (filepath ++ ".csv")
        let lines = chunksOf 1 (splitOn "\n" contents)
        return lines

    buildTable :: [[String]] -> [[String]]
    buildTable [] = []
    buildTable ((x:xs):xss) = (splitOn "," x) : buildTable xss 

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
    buildOpTree (Relation tblN varis) = RelationNode (tblN) (assignVarTreeLoc (buildVarTree varis) (tblN) )
    --Below (TODO) add type checker for querA/B, checking its a VarTree
    buildOpTree (Equality querA querB) = (EquateNode (varToOpTree(buildVarTree(queryToVariables(querA)))) (varToOpTree(buildVarTree(queryToVariables(querB)))))
    buildOpTree (V varis) = VarOp (buildVarTree(varis))
    buildOpTree (ExistentialSingle vTree oTree) = (ExistVar (buildVarTree(vars)) (buildOpTree (quer))) --(EXIS REFORMAT)
    buildOpTree _ = EmptyOT (Main.Nothing)
    
    --buildRelationalTree :: String -> VarTree -> OpTree
    --buildRelationalTree tblName vTree = (RelationNode (tblName) (assignVarTreeLoc (vTree) (tblName)))
    



    {-==============================================================================-}
    {-============================== TABLE OPERATIONS ==============================-}
    {-==============================================================================-}

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







    crossNew :: [[[String]]] -> [String] -> [[VarNode]]
    crossNew [] [] = []
    crossNew [tableA] (x:xs) = assignTblNm tableA x
    crossNew [tableA, tableB] (x:y:xs) = assignTblNm tableA x : assignTblNm tableB y
    crossNew (tableA:tableB:ts) (x:y:xs) = crossNew(firstTwoTables : ts)
        where firstTwoTables = crossTwo (assignTblNm tableA x) (assignTblNm tableB y)

    assignTblNm :: [[String]] -> String -> [[VarNode]]
    assignTblNm [] _ = []
    assignTblNm (row:rs) name = assignDataAndName row name : assignTblNm rs name

    assignDataAndName :: [String] -> String -> [VarNode]
    assignDataAndName [] _ = []
    assignDataAndName (cell:cs) name = buildVarNode name cell "*" : assignDataAndName cs name

    buildVarNode :: String -> String -> String -> VarNode
    buildVarNode (tblName) (cell) (varName) = Vari (tblName) (cell) (varName)
    
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
    getUniqueState (RelationNode (string) (varTree)) False = rmDupVars (varTree) (False)
    getUniqueState (EquateNode (oTA) (oTB)) False = getUniqueState oTA (False) ++ getUniqueState oTB (False)
    getUniqueState (BoolNode (bool)) False = []
    getUniqueState (VarOp (varTree)) False = rmDupVars (varTree)
    getUniqueState (EmptyOT (emptyTree)) False = []
    getUniqueState (ExistVar (varTree) (oTA)) False = getUniqueState (oTA) (True)

    getUniqueState (ConjunctionNode (oTA) (oTB)) True = getUniqueState oTA (True) ++ getUniqueState oTB (True)
    getUniqueState (RelationNode (string) (varTree)) True = varTreeToList (varTree)
    getUniqueState (EquateNode (oTA) (oTB)) True = getUniqueState oTA (True) ++ getUniqueState oTB (True)
    getUniqueState (BoolNode (bool)) True = []
    getUniqueState (VarOp (varTree)) True = varTreeToList (varTree)
    getUniqueState (EmptyOT (emptyTree)) True = []
    getUniqueState (ExistVar (varTree) (oTA)) True = getUniqueState (oTA) (True)

    rmDupVars :: VarTree -> [VarNode]
    rmDupVars vTree = nub (varTreeToList(vTree))

    varTreeToList :: VarTree -> [VarNode]
    varTreeToList (CommaNode (varNode) (restTree)) = varNode : varTreeToList restTree
    varTreeToList (SingleNode (varNode)) = [] : varNode
    varTreeToList (EmptyVT (emptyTree)) = [] 

    {-==============================================================================-}
    {-====================== EXIS REFORMAT old TABLE OPERATIONS ====================-}
    {-==============================================================================-}
{-
--ELLIOTT PLEASE CONFRIM THESE IDK WHAT ONES ARE STILL APPLICABLE
-- Pretty much all these are redudant now. -E

    
    --[Table] -> Table
    --[[Row]] -> [Row]
    --[[[String]]] -> [[String]]
    
    --Input multiple tables to cross product all.
    --          ALL TABLES      REMAINING TABLES       OUTPUT
    crossMulti :: [[[String]]] -> [[String]]
    crossMulti [] = []
    crossMulti [tableA] = tableA
    crossMulti [tableA, tableB] = crossTwo tableA tableB
    crossMulti (tableA:tableB:ts) = crossMulti(firstTwo:ts)
        where firstTwo = crossTwo tableA tableB
    
    --Cross two tables.
    crossTwo :: [[String]] -> [[String]] -> [[String]]
    crossTwo _ [] = []
    crossTwo [] _ = []
    crossTwo (rowA:as) (rowB:bs) = pairRowToTable (rowA) (rowB:bs) ++ crossTwo as (rowB:bs)
    
    --Cross a row to an entire table.
    pairRowToTable :: [String] -> [[String]] -> [[String]]
    pairRowToTable _ [] = []
    pairRowToTable rowA (rowB:bs) = [pairRow (rowA) (rowB)] ++ pairRowToTable (rowA) (bs)
    
    pairRow :: [String] -> [String] -> [String]
    pairRow [] _ = []
    pairRow _ [] = []
    pairRow rowA rowB = rowA ++ rowB
    
    -- String is a cell
    -- [String] is a row
    -- [[String]] is a table
    -- [[[String]]] is multiple tables.
    foldRows' :: [[[String]]] -> [[String]]
    foldRows' [] = []
    foldRows' (((x:xs):xss):xsss) = ((x:xs):xss) ++ foldRows' xsss
    
    -- foldRows :: [[[String]]] -> [[String]]
    -- foldRows [] = []
    -- foldRows [table] = foldr (++) [] [table]
    -}
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
    --getOrderOfVars (MarkerNested (list) (existTree)) = list (EXIS REFORMAT)
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
    
    --list of varN -> String
    
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
    readyOutput (row:rs) = rowToString row : readyOutput rs

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

    executeQuery :: [[VarNode]] -> ParseTree -> [[VarNode]]
    executeQuery [] _ = []
    executeQuery (row:remainingRows) (Marker ordVars oTree)      | (evaluateParseTree (assignedTree) ) == True   = [assignedRow] ++ executeQuery (remainingRows) (oTree)
                                                                 | (evaluateParseTree (assignedTree) ) == False  = executeQuery (remainingRows) (pTree)
                                                                 where   assignedRow = assignPTState pTree row -- : executeQuery (remainingRows) (pTree)
                                                                         assignedTree = popTree (sanitiseOpTree(oTree)) (row) (0)

    assignPTState :: ParseTree -> [String] -> [VarNode]
    assignPTState (Marker (vars) (oTree)) strings = getTreeState((popTree (sanitiseOpTree(oTree)) (strings) 0)) : []
   -- assignPTState (MarkerNested (vars) (eTree)) strings = getETreeState(populateExisTree (sanitiseExisTree(eTree)) (strings)) (EXIS REFORMAT)


-- NEW ExQuer: (EXIS REFORMAT)


    -- getNodeFromRow :: 

    --New ePT: (EXIS REFORMAT)
    evaluateParseTree :: ParseTree -> Bool --
    evaluateParseTree thisTree rList = checkRepeats(filterRepeats(groupRepeats(getTreeState(thisTree)))) && (evaluate (thisTree))
                                                            


    {- OLD VERSION (EXIS REFORMAT)
    evaluateParseTree :: ParseTree -> [String] -> Bool --
    evaluateParseTree (Marker ordVars oTree) rList          = checkRepeats(filterRepeats(groupRepeats(getTreeState(thisTree)))) &&  (evaluate (thisTree))
                                                            where thisTree = popTree (sanitiseOpTree(oTree)) (rList) (0)
    --evaluateParseTree (MarkerNested ordVars eTree ) rList   = evaluateExis (eTree) (rList) (EXIS REFORMAT)
    -}

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
    
    --ISNT USED
    -- matchNodeFromName :: [VarNode] -> String -> [VarNode]
    -- matchNodeFromName [] _ = []
    -- matchNodeFromName ( (Vari (loc) (dat) (nameX)) : xs) name   | name == nameX = [(Vari (loc) (dat) (name))] ++ matchNodeFromName xs name
    --                                                             | name /= nameX = matchNodeFromName xs name

    isAllDataSame :: [VarNode] -> Bool --New higher order for ease of use (fixes checkrepeats comp error)
    isAllDataSame [] = checkAllDataSame [] "irrelevant"
    isAllDataSame ((Vari (loc) (dat) (name)):xs) = checkAllDataSame ((Vari (loc) (dat) (name)):xs) dat


    checkAllDataSame :: [VarNode] -> String -> Bool
    checkAllDataSame [] _ = True
    checkAllDataSame ((Vari (loc) (datA) (name)):xs) dat = ( dat == datA ) && ( checkAllDataSame xs dat)
    


--unsure if this is needed at all now
    getRepeats :: [VarNode] -> Int ->  [VarNode] --return repeated namez
    getRepeats [] _ = []
    getRepeats (x:xs) ind   | (ind <= length(x:xs)) && (countInstancesInVarList (fromJust(getNthVNode (ind) (x:xs) )) (x:xs) > 1) = [x] ++ getRepeats (x:xs) (ind+1)
                            | (ind <= length(x:xs)) && (countInstancesInVarList (fromJust(getNthVNode (ind) (x:xs) )) (x:xs) == 1) = getRepeats (x:xs) (ind+1)
                            | otherwise = [] -- should never be called
    
    --Takes a list of varnodes
    -- Checks every varnode to every varnode and sees if the data within varnode matches only when name matches.
    -- True when all varnodes have matching values where data and name
    --Rewritten areRepeats

    -- checkRepeats :: [VarNode] -> Bool
    -- checkRepeats (Vari (loc) (dat) (name):vs) = checkRepeats' (Vari (loc) (dat) (name)) vs && checkRepeats vs

    -- checkRepeats' :: VarNode -> [VarNode] -> Bool
    -- checkRepeats' (Vari (loc) (dat) (name)) (Vari (locL) (datL) (nameL):vs) = equateDataAndName ((Vari (loc) (dat) (name)) (Vari (locL) (datL) (nameL))) && checkRepeats' (Var (loc) (dat) (name)) vs

    --Current attempt, deleted other bs auxilary functions
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
 
 
    -- TODO *** *** *** EXIS REFORMAT CASE
    evaluate :: OpTree -> Bool --evaluate opTree freeVarList
    evaluate (EquateNode (l) (r))  =  ( checkEquality (EquateNode (l) (r))) 
    evaluate (RelationNode (loc) (varTr))  = checkRelation (RelationNode (loc) (varTr))
    evaluate (ConjunctionNode (l) (r))  = checkConjunction (ConjunctionNode (l) (r))
    evaluate (ExistVar vTree oTree) = checkExistential (ExistVar vTree oTree) && evaluate (oTree)
    --evaluate (VarTree ) varRow = 
    --evaluate (VarOp tree) freeVars = assignVars ((traverseDFVar (tree)) freeVars)
    evaluate (VarOp v) = True

    checkConjunction :: OpTree -> Bool
    checkConjunction  (ConjunctionNode (l) (r) ) = (evaluate(l)) && (evaluate(r))
    
    checkExistential :: OpTree -> Bool --does vTree exist in oTree
    checkExistential (ExistVar vTree oTree) = doesListExistInOpTree (traverseDFVar vTree) oTree


    checkRelation :: OpTree -> Bool
    checkRelation (RelationNode (tbl) (vList)) = isTreePopulated(vList)
    
    -- *** TODO ** IMPORTANT: Implement a rule ensuring the children of an equality is 2 var nodes. Do we need to do this in our grammar/tree? See next commenr
    checkEquality :: OpTree -> Bool
    --checkEquality (EquateNodes (VarOp (SingleNode(v))) (restN) ) = evaluate (VarOp (SingleNode(v))) && checkEquality(restN)
    checkEquality (EquateNode (l) (r)) = equateNodes left right
        where   left = convertOpToVarNode (l)
                right = convertOpToVarNode (r)
    
    equateNodes :: VarNode -> VarNode -> Bool
    equateNodes (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))   | datA == datB = True
                                                                            | datA /= datB = False
    
    equateNodesDatAndName :: VarNode -> VarNode -> Bool
    equateNodesDatAndName (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB)) | datA == datB && nameA == nameB = True
                                                                                    | otherwise = False
    equateNodesName :: VarNode -> VarNode -> Bool
    equateNodesName (Vari (locA) (datA) (nameA)) (Vari (locB) (datB) (nameB))   | nameA == nameB = True
                                                                                | nameA /= nameB = False
    
    {-==============================================================================-}
    {-=========================== TREE & NODE OPERATIONS ===========================-}
    {-==============================================================================-}
    getPTreeState :: ParseTree -> [VarNode]
    getPTreeState (Marker (vars) (oTree)) = getTreeState (oTree)
   -- getPTreeState (MarkerNested (vars) (eTree) ) = getETreeState (eTree) (EXIS REFORMAT)
    

    
    
    getTreeState :: OpTree -> [VarNode]
    getTreeState (ConjunctionNode (opTree) (opTreeX)) = getTreeState(opTree) ++ getTreeState (opTreeX)
    getTreeState (RelationNode (string) (varTree)) = getTreeState(varToOpTree(varTree))
    getTreeState (EquateNode (opTree) (opTreeX)) = getTreeState (opTree) ++ getTreeState (opTreeX)
    getTreeState (LSubNode (opTree) (opTreeX)) = getTreeState (opTree) ++ getTreeState (opTreeX)
    getTreeState (RSubNode (opTree) (opTreeX)) = getTreeState (opTree) ++ getTreeState (opTreeX)
    getTreeState (BoolNode (bool)) = []
    getTreeState (VarOp (varTree)) = traverseDFVar(varTree)
    getTreeState (EmptyOT (emptyTree)) = []
    getTreeState (ExistVar (vTree) (oTree)) = getTreeState(varToOpTree(vTree)) ++ getTreeState (oTree) --Possibly not what we need (EXIS REFORMAT)
    

    {-
    --Issues: 1 : When VarOp is met, works fine. However with multiple vTrees spread across conjunctionNode etc, it cant remember the "index" value across diff trees
    --Issues:   : Order of execution means getPopTotal is called BEFORE populateTree so will return 0 and hence fuck up the multi tree issue..i think
    populateTree :: OpTree -> [String] -> Int -> OpTree
    populateTree (VarOp (vTree)) rList ind                      = (VarOp (populateVarTree (vTree) (rList) (ind)))
    populateTree (ConjunctionNode (querA) (querB)) rList ind    = (ConjunctionNode (populateTree (querA) (rList) (ind + (getPopTotal querA querB))) (populateTree (querB) (rList) (ind + (getPopTotal querA querB)) ) )
                                                       -- where   popsC = countPopNodes (querA) + countPopNodes (querB)
    populateTree (EquateNode (querX) (querY)) rList ind         = (EquateNode (populateTree(querX) (rList) (ind + (getPopTotal querX querY))) (populateTree(querY) (rList) (ind + (getPopTotal querX querY))) )
                                                     --   where   popsX = countPopNodes (querX) + countPopNodes (querY)
    populateTree (RelationNode (tbl) (vTree)) rList ind         = populateRelation (RelationNode (tbl) (vTree)) rList ind
    -}
    getPopTotal :: OpTree -> OpTree -> Int
    getPopTotal querP querQ = countPopNodes (querP) + countPopNodes (querQ)



    -- ::::::::::::::::::::::::::::::::::::::::::::::populateTree attempt 2:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
{-
--EXIS REFORMAT TODO: In case of R(...) ^ A(...) ^ R(...), R must assign same vals twice /// ALSO: redo entire thing lol (impl filterNBT)
-- Changed row input from [String] to [VarNode]
    popTree :: OpTree -> [Varnode] -> Int -> OpTree
    popTree (VarOp (vTree)) rList ind                      = popSubTree  (VarOp (vTree)) (rList) (ind)
    popTree (ConjunctionNode (querA) (querB)) rList ind    = (ConjunctionNode (popSubTree (querA) (rList) (ind)) (popTree (querB) (rList) (ind + (countNodes querA)) ) )
    popTree (EquateNode (querX) (querY)) rList ind         = (EquateNode (popSubTree (querX) (rList) (ind)) (popTree(querY) (rList) (ind + (countNodes querX))) )
    popTree (RelationNode (tbl) (vTree)) rList ind         = popSubTree  (RelationNode (tbl) (vTree)) (rList) (ind)
    --popTree (ExistVar (vTree) (oTree)) rList ind           = THINK ABOUT THIS (EXIS REFORMAT)


-- wat do in case at RIGHT subtree at end of tree? ie stopping condition
-- wat do when multiple conjunctions etc?? come back here -- think this will be ok
    popSubTree :: OpTree -> [VarNode] -> Int -> OpTree
    popSubTree (EquateNode (querX) (querY)) rList ind       = popTree  (EquateNode (querX) (querY)) (rList) (ind)
    popSubTree (ConjunctionNode (querA) (querB)) rList ind  = popTree  (ConjunctionNode (querA) (querB)) (rList) (ind)
    popSubTree (VarOp (vTree)) rList ind                    = (VarOp (populateVarTree (vTree) (rList) (ind)))
    popSubTree (RelationNode (tbl) (vTree)) rList ind       = (RelationNode (tbl) (populateVarTree (vTree) (rList) (ind)))
 -}

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
                        where   postFstTree = popTreeFirstPass oTree rList
                                uniqueState = getUniqueState postFstTree
    

    popTreeFirstPass :: OpTree -> [VarNode] -> OpTree
    popTreeFirstPass (VarOp (vTree)) rList   = (VarOp (vTree))-- Case example: x1  =  x2 ^ ... |||| To be left for 2nd pass
    popTreeFirstPass (ConjunctionNode (querA) (querB)) rList = (ConjunctionNode (popTreeFirstPass querA rList) (popTreeFirstPass querB rList)) 
    popTreeFirstPass (EquateNode (querX) (querY)) rList = (EquateNode (popTreeFirstPass querX rList) (popTreeFirstPass querY rList))
    popTreeFirstPass (RelationNode (tbl) (vTree)) rList = popRelation (RelationNode (tbl) (vTree)) (rList)
    popTreeFirstPass (ExistVar (vTree) (oTree)) rList = (ExistVar (vTree) (popTreeFirstPass oTree rList) )


    popTreeNextPass :: OpTree -> [VarNode] -> OpTree
    popTreeNextPass (VarOp (vTree)) rList   = (VarOp (vTree)) -- Does this case ever occur
    popTreeNextPass (ConjunctionNode (querA) (querB)) rList = (ConjunctionNode (popTreeNextPass querA rList) (popTreeNextPass querB rList)) 
    popTreeNextPass (EquateNode (querX) (querY)) rList = (EquateNode (popTreeNextPass querX rList) (popTreeNextPass querY rList)) --popEquateNode
    popTreeNextPass (RelationNode (tbl) (vTree)) rList = (RelationNode (tbl) (vTree)) --Already populated so left alone
    popTreeNextPass (ExistVar (vTree) (oTree)) rList = (ExistVar (vTree) (popTreeNextPass oTree rList) )

    popBoundVTree :: OpTree -> [VarNode] -> OpTree
    popBoundVTree (VarOp(SingleNode (vNode))) rList = VarOp (popBoundVNode (vNode) (rList))
    popBoundVTree (VarOp(CommaNode (vNode) (remTree))) rList = VarOp ( CommaNode (popBoundVNode (vNode) (rList)) (popBoundVTree (remTree) (rList)) )

    popBoundVNode :: VarNode -> [VarNode] -> VarNode --2nd pass only, MAYBE additional check to ensure all values are populated
    popBoundVNode (Vari loc dat name) rList | extractExactNode rList loc name == Main.Nothing = (Vari loc dat name)
                                            | otherwise = extractExactNode rList loc name

    --assignScope :: OpTree -> OpTree



    getNodeLocation :: String -> [VarNode] -> String -- name -> rList -> location
    getNodeLocation _ [] = "*" -- Elliott : you can probably error check when this case occurs cos all varN's should have a location after population (need to implement that tho)
    getNodeLocation name (x:xs) | equateNodeName x name == True = unwrapNodeLocation x
                                | equateNodeName x name == False = getNodeLocation name xs

    unwrapNodeLocation :: VarNode -> String
    unwrapNodeLocation (Vari loc dat name) = loc 

    equateNodeName :: VarNode -> String -> Bool
    equateNodeName (Vari locA datA nameA) nameB = nameA == nameB 

        

    extractExactNode :: [VarNode] -> String -> String -> Maybe VarNode --rList -> loc -> name -> outputNode
    extractExactNode [] _  _ = Main.Nothing
    extractExactNode (x:xs) loc name    | matchLocName x loc name == True = x
                                        | matchLocName x loc name == False = extractExactNode xs loc name

    matchLocName :: VarNode -> String -> String -> Bool 
    matchLocName (Vari thisLoc thisDat thisName) loc name = (thisLoc == loc) && (thisName == name)
{-
    popVarNode :: VarTree -> [VarNode] -> VarTree -- for use in popRelation
    popVarNode (SingleNode (Vari loc dat name)) rList   | isNodeAssigned (Vari loc dat name) = matchLocName (Vari loc dat name)
          (extractOutput' rList)
    --UNCOMMENT N FINISH
    --                                                    | !isNodeAssigned (Vari loc dat name) = --unsure rn 

    popVarTree :: VarTree -> [VarNode] -> VarTree
    popVarTree (SingleNode (vNode)) rList = popVarNode (vNode) (rList)
    popVarTree (CommaNode (vNode) (vTree)) rList = (CommaNode (popVarNode(vNode) (rList)) (popVarTree vTree rList)
-}

    popRelation :: OpTree -> [VarNode] -> OpTree --RelatioNode case only
    popRelation (RelationNode (tbl) (vTree)) rList = populateVarTree (vTree) (extractOutput'(filterNodesByTable rList tbl)) 0

    


    --REMEMBER: NON BOUND VARIABLES, eg (z1,z2 (E. (R(z1,z2)))), CAN BE DUPLICATED ELSEWHERE IN QUERY AND SHOULDNT BE REMOVED FROM THE LIST, AS THEYRE UNIQUE
    --Separate function populating vTree in ExistVar AFTER first round of population, so the relavant table names can be easily extracted:
    --postPopTreePass :: OpTree -> [VarNode] -> OpTree


--THINK ABOUT THIS (EXIS REFORMAT SMART POP)
    --REVOLUTIONARY NEW FUNCTION : (EXIS REFORMAT)
    filterNodesByTable :: [VarNode] -> String -> [VarNode]
    filterNodesByTable [] _ = []
    filterNodesByTable ((Vari loc dat name):xs) tblName     | loc == tblName = [(Vari loc dat name)] ++ filterNodesByTable xs tblName
                                                            | loc /= tblName = filterNodesByTable xs

    getNodeAtNameAndLoc :: [VarNode] -> String -> String -> Maybe VarNode
    getNodeAtNameAndLoc [] _ _ = Main.Nothing
    getNodeAtNameAndLoc ((Vari loc dat name):xs) thisLoc thisName   | loc == thisLoc && name == thisName = (Vari loc dat name)
                                                                    | otherwise = getNodeAtNameAndLoc xs

    getNodeAtName :: [VarNode] -> String -> Maybe VarNode
    getNodeAtName [] _ = Main.Nothing
    getNodeAtName ((Vari loc dat name):xs) thisName | name == thisName = Just (Vari loc dat name)
                                                    | name /= thisName = getNodeAtName xs

    existsTreeInRelation :: VarTree -> Bool
    existsTreeInRelation (SingleNode (Vari loc dat name))  = existsInRelation (Vari loc dat name)    
    existsTreeInRelation (CommaNode (Vari loc dat name) (rTree) ) existsInRelation (Vari loc dat name) && existsTreeInRelation rTree                                

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
   
   
   
    -- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                                        
    --populateParseTree :: ParseTree -> [String] ->
    
   
    
    
    
    sanitiseOpTree :: OpTree -> OpTree
    sanitiseOpTree (RelationNode (tblName) (vTree)) = (RelationNode (tblName) (sanitiseVarTree(vTree)))
    sanitiseOpTree (VarOp (vT)) = (VarOp (sanitiseVarTree(vT)))
    sanitiseOpTree (ConjunctionNode (querA) (querB)) = (ConjunctionNode (sanitiseOpTree (querA)) (sanitiseOpTree(querB)) )
    sanitiseOpTree (EquateNode (querX) (querY)) = (EquateNode (sanitiseOpTree (querX)) (sanitiseOpTree (querY)) )
    sanitiseOpTree (ExistVar (vTree) (oTree)) = (ExistVar (sanitiseVarTree (vTree)) (sanitiseOpTree (oTree)))
    
    sanitiseVarTree :: VarTree -> VarTree
    sanitiseVarTree (SingleNode ( Vari (loc) (dat) (name) ) ) = (  SingleNode (  Vari (loc) ("*") (name)  )  )
    sanitiseVarTree (CommaNode ( Vari (loc) (dat) (name) ) (remTree) ) = (CommaNode ( Vari (loc) ("*") (name) ) (sanitiseVarTree remTree) )
    {-
    countPopulations :: OpTree -> Int -> Int
    countPopulations (RelationNode (tbl) (vTree)) _ | isTreePopulated (vTree) == True = countPopNodesInVT (vTree)
                                                    | isTreePopulated (vTree) == False = 0
    countPopulations (ConjunctionNode (querA) (querB)) = countPopulations(querA) + countPopulations(querB)
    countPopulations (EquateNode (querA) (querB)) = countPopulations(querA) + countPopulations(querB)
    countPopulations (VarOp (vTree))                | isTreePopulated (vTree) == True = countPopNodesInVT (vTree)
                                                    | isTreePopulated (vTree) == False = 0
    -}
    populateRelation :: OpTree -> [String] -> Int -> OpTree
    populateRelation (RelationNode (tblName) (vTree)) rList ind     | isTreePopulated (vTree) == False = (RelationNode (tblName) (populateVarTree (vTree) rList (ind) )) --Somethings gone wrong maybe?
                                                                    | otherwise = (RelationNode (tblName) (vTree))
    populateVarTree :: VarTree -> [String] -> Int -> VarTree
    populateVarTree (SingleNode (Vari (loc) (dat) (name))) (x:xs) ind | isNodePopulated (Vari (loc) (dat) (name)) == False = (SingleNode (Vari (loc) (generateNextVarData (x:xs) (ind)) (name) ))
    populateVarTree (SingleNode (Vari (loc) (dat) (name))) (x:xs) ind | isNodePopulated (Vari (loc) (dat) (name)) == True = (SingleNode (Vari (loc) (dat) (name)))
    populateVarTree ( CommaNode (Vari (loc) (dat) (name)) (remTree) ) (x:xs) ind  | isNodePopulated (Vari (loc) (dat) (name)) == False = ( CommaNode (Vari (loc) (generateNextVarData (x:xs) (ind)) (name)) ( populateVarTree (remTree) (x:xs) (ind+1) ) )
    populateVarTree ( CommaNode (Vari (loc) (dat) (name)) (remTree) ) (x:xs) ind  | isNodePopulated (Vari (loc) (dat) (name)) == True = (CommaNode (Vari (loc) (dat) (name)) (remTree)) --add recursive call here
    
    -- doesNameExistInVList :: String -> [VarNode] -> Bool
    -- doesNameExistInVList _  (x:xs) [] = False
    -- doesNameExistInVList targStr (x:xs) ((Vari (loc) (dat) (name)):ys)  | targStr == name = True
    --                                                                | targStr /= name = doesNameExistInVList targStr ys
    
    --If name already exists, assign variable with same name to the next unassigned 
    --Add syntax error here?
    {- OLD nextVar
    generateNextVarData :: [String] -> Int -> String
    generateNextVarData (x:xs) ind  | ind < length(x:xs) = ( (x:xs)!!(ind + 1) ) --possibly unsafe, maybe use getNth
                                    | ind >= length(x:xs) = "THIS IS AN ERROR AND SHOULD NOT BE HERE"
    -}
    generateNextVarData :: [String] -> Int -> String
    generateNextVarData (x:xs) ind  | ind < length(x:xs) = ( getNthElement (ind+1) (x:xs) ) --possibly unsafe, maybe use getNth
                                    | ind >= length(x:xs) = "THIS IS AN ERROR AND SHOULD NOT BE HERE (genNextVar)"

    -- getDataMatchingName :: String -> [VarNode] -> VarNode
    
    -- doesVarNameExist :: [VarNode] -> String -> Bool
    
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
    
    countPopNodes :: OpTree -> Int
    countPopNodes (ConjunctionNode (opTree) (opTreeX)) = countPopNodes opTree + countPopNodes opTreeX
    countPopNodes (RelationNode (string) (varTree)) = countPopNodesInVT varTree
    countPopNodes (EquateNode (opTree) (opTreeX)) = countPopNodes opTree + countPopNodes opTreeX
    countPopNodes (LSubNode (opTree) (opTreeX)) = countPopNodes opTree + countPopNodes opTreeX
    countPopNodes (RSubNode (opTree) (opTreeX)) = countPopNodes opTree + countPopNodes opTreeX
    countPopNodes (BoolNode (bool)) = 0
    countPopNodes (VarOp (varTree)) = countPopNodesInVT varTree
    countPopNodes (EmptyOT (emptyTree)) = 0
    
    countPopNodesInVT :: VarTree -> Int
    countPopNodesInVT (CommaNode varN varTree) = (if(checkNodePop varN) then 1 else 0) + countPopNodesInVT varTree
    countPopNodesInVT (SingleNode varN) = (if(checkNodePop varN) then 1 else 0)
    countPopNodesInVT (EmptyVT empty) = 0
    
    checkNodePop :: VarNode -> Bool
    checkNodePop (Vari loc dat name) | dat /= "*" = True
    checkNodePop (Vari loc dat name) | dat == "*" = False
    
    countVarNodes :: VarTree -> Int
    countVarNodes (CommaNode varN varTree) = 1 + countVarNodes varTree
    countVarNodes (SingleNode varN) = 1
    countVarNodes (EmptyVT empty) = 0
    
    charToString :: Char -> String
    charToString = (:[])

    extractTableNames :: ParseTree -> [String]
    extractTableNames pTree = extractDups(map (++ ".csv") (map charToString (map head (extractPTableNames pTree))))
    
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
    extractOTableNames ((ExistVar vTree oTree):xs) = extractOTableNames oTree
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
    


    --(EXIS REFORMAT) REDUNDANT NOW?? 
    liftRelationNodesOut :: ParseTree -> [OpTree] --Creates list of single node OpTree's, 
    liftRelationNodesOut (Marker vList oTree) = getOpRelationNodesOut(oTree)
    --liftRelationNodesOut (MarkerNested vList eTree) = getExisRelationNodesOut(eTree)
    --liftRelationNodesOut (MarkerExtended vList eTree oTree) = getExisRelationNodesOut(eTree) ++ getOpRelationNodesOut(oTree)
    

    
    getOpRelationNodesOut :: OpTree -> [OpTree] --Relation nodess are never subtrees of "="
    getOpRelationNodesOut (RelationNode (tbl) (vTree)) = [(RelationNode (tbl) (vTree))]
    getOpRelationNodesOut (ConjunctionNode (querA) (querB)) = getOpRelationNodesOut(querA) ++ getOpRelationNodesOut(querB)
                      
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



    {-==============================================================================-}
    {-=============================== OLD EXIS STUFF ===============================-}
    {-==============================================================================-}
{-
    getExisRelationNodesOut :: ExistTree -> [OpTree]
    getExisRelationNodesOut (ExistVar (vTree) (oTree)) = getOpRelationNodesOut(oTree)
    getExisRelationNodesOut (ExistNest (vTree) (eTree) (oTree)) = getExisRelationNodesOut(eTree) ++ getOpRelationNodesOut(oTree)
    getExisRelationNodesOut (EmptyET empty) = []

    buildExisTree :: Existential -> ExistTree 
    buildExisTree (ExistentialSingle (vars) (quer)) = (ExistVar (buildVarTree(vars)) (buildOpTree (quer)))
    buildExisTree (ExistentialNested (vars) (exisNest) (quer)) = (ExistNest (buildVarTree(vars)) (buildExisTree (exisNest)) (buildOpTree(quer)))

    extractETableNames :: ExistTree -> [String]
    extractETableNames (ExistVar (vTree) (oTree)) = extractOTableNames(getOpRelationNodesOut(oTree))
    extractETableNames (ExistNest (vTree) (eTree) (oTree)) = extractOTableNames(getOpRelationNodesOut(oTree))

    sanitiseExisTree :: ExistTree -> ExistTree
    sanitiseExisTree (ExistVar (vTree) (oTree)) = (ExistVar (sanitiseVarTree(vTree)) (sanitiseOpTree(oTree)))
    saniitseExisTree (ExistNest (vTree) (eTree) (oTree)) = (ExistNest (sanitiseVarTree(vTree)) (sanitiseExisTree(eTree)) (sanitiseOpTree(oTree)) )

    populateExisTree :: ExistTree -> [String] ->  ExistTree
    populateExisTree (ExistVar (vTree) (oTree)) rList = (ExistVar (populateVarTree (vTree) (rList) (0) ) (popTree (oTree) (rList) (0) ))
    populateExisTree (ExistNest (vTree) (eTree) (oTree)) rList = (ExistNest (populateVarTree (vTree) (rList) (0) ) (populateExisTree (eTree) (rList) ) (popTree (oTree) (rList) (0)))

        -- getTreeStateEx :: ExistTree -> [VarNode]
    -- getTreeStateEx ExistVar (VarTree) (OpTree) 
    -- getTreeStateEx ExistNest (VarTree) (ExistTree)
    -- getTreeStateEx EmptyET (EmptyTree)

    getETreeState :: ExistTree -> [VarNode]
    getETreeState (ExistVar (vars) (oTree)) = getTreeState(oTree)
    getETreeState (ExistNest (vars) (eTree) (oTree)) = getTreeState(oTree)

    checkExistential :: ExistTree -> Bool
    checkExistential (ExistVar (vTree) (oTree)) = (doesListExistInOpTree (traverseDFVar(vTree)) (oTree)) 
    checkExistential (ExistNest (vTree) (eTree) (oTree)) = (doesListExistInOpTree (traverseDFVar(vTree)) (oTree)) && checkExistential (eTree)

    evaluateExis :: ExistTree -> [String] -> Bool
    evaluateExis eTree strL = (areRepeats(getETreeState(thisTree)) 0) && (checkExistential(thisTree))
                            where thisTree = populateExisTree (sanitiseExisTree(eTree)) (strL)

                           
    -- Seperate exisitential operator.
    -- EXISITENTIAL TREE ***TODO** add case for nested eTree
    data ExistTree = ExistVar (VarTree) (OpTree) 
        | ExistNest (VarTree) (ExistTree) (OpTree)
        | EmptyET (EmptyTree)
        deriving Show
    -- ***TODO*** ADD NESTED EXISTENTIAL

    popTree :: OpTree -> [String] -> Int -> OpTree
    popTree (VarOp (vTree)) rList ind                      = popSubTree  (VarOp (vTree)) (rList) (ind)
    popTree (ConjunctionNode (querA) (querB)) rList ind    = (ConjunctionNode (popSubTree (querA) (rList) (ind)) (popTree (querB) (rList) (ind + (countNodes querA)) ) )
    popTree (EquateNode (querX) (querY)) rList ind         = (EquateNode (popSubTree (querX) (rList) (ind)) (popTree(querY) (rList) (ind + (countNodes querX))) )
    popTree (RelationNode (tbl) (vTree)) rList ind         = popSubTree  (RelationNode (tbl) (vTree)) (rList) (ind)


        popSubTree :: OpTree -> [String] -> Int -> OpTree
    popSubTree (EquateNode (querX) (querY)) rList ind       = popTree  (EquateNode (querX) (querY)) (rList) (ind)
    popSubTree (ConjunctionNode (querA) (querB)) rList ind  = popTree  (ConjunctionNode (querA) (querB)) (rList) (ind)
    popSubTree (VarOp (vTree)) rList ind                    = (VarOp (populateVarTree (vTree) (rList) (ind)))
    popSubTree (RelationNode (tbl) (vTree)) rList ind       = (RelationNode (tbl) (populateVarTree (vTree) (rList) (ind)))




      {- OLD VERSION (EXIS REFORMAT)
    executeQuery :: [[String]] -> ParseTree -> [[VarNode]] -- Elliott: Changed data type here
    executeQuery [] _ = []
    executeQuery (row:remainingRows) (pTree)    | (evaluateParseTree (pTree) (row)) == True   = [output] ++ executeQuery (remainingRows) (pTree)
                                                | (evaluateParseTree (pTree) (row)) == False  = executeQuery (remainingRows) (pTree)
                                                where   output = assignPTState pTree row -- : executeQuery (remainingRows) (pTree)
                                                       


-}

-}