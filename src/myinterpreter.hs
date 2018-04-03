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
    import Debug
    import qualified Data.ByteString.Char8 as C
    
    
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
        | LSubNode (OpTree) (OpTree)
        | RSubNode (OpTree) (OpTree)
        | BoolNode (Bool)
        | VarOp (VarTree)
        | EmptyOT (EmptyTree)
        deriving Show
    
    -- Seperate exisitential operator.
    -- EXISITENTIAL TREE ***TODO** add case for nested eTree
    data ExistTree = ExistVar (VarTree) (OpTree) 
        | ExistNest (VarTree) (ExistTree) (OpTree)
        | EmptyET (EmptyTree)
        deriving Show
    -- ***TODO*** ADD NESTED EXISTENTIAL
    -- All expressions available within language.
    -- PARSE TREE
    data ParseTree = Marker ([VarNode]) (OpTree)
        | MarkerNested ([VarNode]) (ExistTree)
        | EmptyPT (EmptyTree)
        deriving Show
      --  (1,10,3)E.( ( (1,2)E.Q(x1,x2) ^ (x1 = x2) ) ^ (x3=foo) )
    
    data EmptyTree = Nothing
        deriving Show
    
    {-==============================================================================-}
    {-===================================== MAIN ===================================-}
    {-==============================================================================-}
    --main :: IO()
    main = do 
        a <- getArgs
        b <- readFile (head a)
        let herbLang = head (splitOn "\n" b)
        let alex = alexScanTokens (herbLang)
        let happy = parseCalc (alex)
        let pTree = buildParseTree (happy)
        let lhsVar = getOrderOfVars (pTree)

        contentA <- readContents("A.csv")
        contentB <- readContents("B.csv")
        let allTables = (buildTable(contentA)) : (buildTable(contentB)) : []

        -- For future implementation where contentA-N and allTables is built dynamically.
        let tableNames = extractPTableNames (pTree)

        let bigTable = crossMulti(allTables)
        --mapM_ putStrLn ([show (bigTable)])
        let answer = executeQuery (bigTable) (pTree)
        let output = readyOutput ( extractOutput (orderTable (lhsVar) (answer)))
        putStrLn("No hang")
        --let stringOutput = answer
        mapM_ putStrLn output
        --putStr("Execution complete")
      
    
       -- return putStr("Execution completed!!!!!!!")
       -- return "someting"
    {-==============================================================================-}
    {-================================= BUILDING ===================================-}
    {-==============================================================================-}
    
    buildParseTree :: Exp -> ParseTree
    buildParseTree (Evaluate (vars) (query)) = Marker (traverseDFVar(buildVarTree(vars)))  (buildOpTree(query))
    buildParseTree (Eval vars exis) = MarkerNested (traverseDFVar(buildVarTree(vars))) (buildExisTree(exis))
    --buildParseTree (EvalExisExt vars exis quer) = MarkerExtended (traverseDFVar(buildVarTree(vars))) (buildExisTree(exis)) (buildOpTree(quer))
    
    buildVarTree :: Variables -> VarTree
    buildVarTree (VarSingle strName) = SingleNode (Vari ("*") ("*") (strName))
    buildVarTree (Comma (string) remVars) = CommaNode (Vari ("*") ("*") (string)) (buildVarTree remVars)

    buildExisTree :: Existential -> ExistTree 
    buildExisTree (ExistentialSingle (vars) (quer)) = (ExistVar (buildVarTree(vars)) (buildOpTree (quer)))
    buildExisTree (ExistentialNested (vars) (exisNest) (quer)) = (ExistNest (buildVarTree(vars)) (buildExisTree (exisNest)) (buildOpTree(quer)))
    
    buildOpTree :: Query -> OpTree 
    buildOpTree (Conjunction querA querB) = ConjunctionNode (buildOpTree querA) (buildOpTree querB)
    buildOpTree (Relation tblN varis) = RelationNode (tblN) (assignVarTreeLoc (buildVarTree varis) (tblN) )
    --Below (TODO) add type checker for querA/B, checking its a VarTree
    buildOpTree (Equality querA querB) = (EquateNode (varToOpTree(buildVarTree(queryToVariables(querA)))) (varToOpTree(buildVarTree(queryToVariables(querB)))))
    buildOpTree (V varis) = VarOp (buildVarTree(varis))
    buildOpTree _ = EmptyOT (Main.Nothing)
    
    --buildRelationalTree :: String -> VarTree -> OpTree
    --buildRelationalTree tblName vTree = (RelationNode (tblName) (assignVarTreeLoc (vTree) (tblName)))
    
    {-==============================================================================-}
    {-=============================== CSV EXTRACTION ===============================-}
    {-==============================================================================-}
    
    -- For more than two databases.
    -- readMulti :: String -> IO [[String]]
    -- readMulti x = do
    --        readContents ([(take 1 x)] ++ ".csv")

    readContents :: String -> IO [[String]]
    readContents filepath = do
        contents <- readFile (filepath)
        let lines = chunksOf 1 (splitOn "\n" contents)
        return lines

    buildTable :: [[String]] -> [[String]]
    buildTable [] = []
    buildTable ((x:xs):xss) = (splitOn "," x) : buildTable xss 
    
    {-==============================================================================-}
    {-============================== TABLE OPERATIONS ==============================-}
    {-==============================================================================-}
    
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
    
    doesListExistInOpTree :: [VarNode] -> OpTree -> Bool
    doesListExistInOpTree (x:xs) oTree = ( doesExistInOpTree (x) (oTree) ) && ( doesListExistInOpTree (xs) (oTree) )
    doesListExistInOpTree [] oTree = True
    
    doesExistInOpTree :: VarNode -> OpTree -> Bool
    doesExistInOpTree (node) (ConjunctionNode (opTree) (opTreeX)) = doesExistInOpTree node opTree || doesExistInOpTree node opTreeX
    doesExistInOpTree (node) (RelationNode (string) (varTree)) = doesExistInVarTree node varTree
    doesExistInOpTree (node) (EquateNode (opTree) (opTreeX)) =  doesExistInOpTree node opTree || doesExistInOpTree node opTreeX
    doesExistInOpTree (node) (VarOp (varTree)) = doesExistInVarTree node varTree
    
    doesExistInVarTree :: VarNode -> VarTree -> Bool
    doesExistInVarTree (node) (CommaNode (varNode) (varTree)) = equateNodesDatAndName node varNode || doesExistInVarTree node varTree
    doesExistInVarTree (node) (SingleNode (varNode)) = equateNodesDatAndName node varNode
    doesExistInVarTree (node) (EmptyVT (emptyTree)) = False
    
    getOrderOfVars :: ParseTree -> [VarNode]
    getOrderOfVars (Marker (list) (opTree)) = list
    getOrderOfVars (MarkerNested (list) (existTree)) = list
    getOrderOfVars (EmptyPT (emptyTree)) = []
    
    
    filterTrue :: [(Bool, [VarNode])] -> [[VarNode]]
    filterTrue ((bool, vars):xs) | length xs == 0 = [[]]
    filterTrue ((bool, vars):xs) | bool == True = vars : filterTrue xs
    filterTrue ((bool, vars):xs) | bool == False = filterTrue xs
    
    extractData :: VarNode -> String
    extractData (Vari (loc) (dat) (name)) = dat
    
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
    orderTable [] _ = []
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
    rowToString [a] = a ++ "\n"
    rowToString [a,b] = a ++ "," ++ b
    rowToString (x:xs) = x ++ "," ++ rowToString xs 

    assignPTState :: ParseTree -> [String] -> [VarNode]
    assignPTState (Marker (vars) (oTree)) strings = getTreeState((populateTree (sanitiseOpTree(oTree)) (strings) 0))
    assignPTState (MarkerNested (vars) (eTree)) strings = getETreeState(populateExisTree (sanitiseExisTree(eTree)) (strings))

    executeQuery :: [[String]] -> ParseTree -> [[VarNode]] -- Elliott: Changed data type here
    executeQuery [] _ = []
    executeQuery (row:remainingRows) (pTree)    | (evaluateParseTree (pTree) (row)) == True   = output ++ executeQuery (remainingRows) (pTree)
                                                | (evaluateParseTree (pTree) (row)) == False  = executeQuery (remainingRows) (pTree)
                                                where   output = populatedRow : executeQuery (remainingRows) (pTree)
                                                        populatedRow = assignPTState pTree row



    --Whats this saying elliott idk wat it does. Needed?
    executeQuery''''' :: [[String]] -> ParseTree -> [[String]] -- Elliott: Changed data type here
    executeQuery''''' [] _ = []
    executeQuery''''' (x:xs) (pTree)     | (evaluateParseTree (pTree) (x)) == True = [x] ++ executeQuery''''' (xs) (pTree)
                                    | (evaluateParseTree (pTree) (x)) == False = executeQuery''''' (xs) (pTree)
                                    -- where nodeFromX = 

    -- getNodeFromRow :: 
    
    evaluateParseTree :: ParseTree -> [String] -> Bool
    evaluateParseTree (Marker ordVars oTree) rList          = (areRepeats(getTreeState(thisTree)) 0) && (evaluate (thisTree))
                                                            where thisTree = populateTree (sanitiseOpTree(oTree)) (rList) (0)
    evaluateParseTree (MarkerNested ordVars eTree ) rList   = evaluateExis (eTree) (rList)
    
    evaluateExis :: ExistTree -> [String] -> Bool
    evaluateExis eTree strL = (areRepeats(getETreeState(thisTree)) 0) && (checkExistential(thisTree))
                            where thisTree = populateExisTree (sanitiseExisTree(eTree)) (strL)
    --Are all nodes in list . NB ************* Not sure of ">", ">=" ie what combination *******************
    areRepeats :: [VarNode] -> Int -> Bool
    areRepeats [] _ = True --Is this ever called??
    areRepeats ( (Vari (loc) (dat) (name)) : xs) ind    | length totalList <= ind =  checkAllDataSame (matches) (dat) && (areRepeats (totalList) (ind+1))
                                                        | length totalList > ind = checkAllDataSame (matches) (dat)
                                                        where   repeats = getRepeats (totalList) (ind+1)
                                                                matches = matchNodeFromName repeats name   
                                                                totalList = ( (Vari (loc) (dat) (name)) : xs)
    
    matchNodeFromName :: [VarNode] -> String -> [VarNode]
    matchNodeFromName [] _ = []
    matchNodeFromName ( (Vari (loc) (dat) (nameX)) : xs) name   | name == nameX = [(Vari (loc) (dat) (name))] ++ matchNodeFromName xs name
                                                                | name /= nameX = matchNodeFromName xs name
    
    checkAllDataSame :: [VarNode] -> String -> Bool
    checkAllDataSame [] _= True
    checkAllDataSame ((Vari (loc) (datA) (name)):xs) dat = ( dat == datA ) && ( checkAllDataSame xs dat)
    
    getRepeats :: [VarNode] -> Int ->  [VarNode] --return repeated namez
    getRepeats [] _ = []
    getRepeats (x:xs) ind   | (ind <= length(x:xs)) && (countInstancesInVarList (fromJust(getNthVNode (ind) (x:xs) )) (x:xs) > 1) = [x] ++ getRepeats (x:xs) (ind+1)
                            | (ind <= length(x:xs)) && (countInstancesInVarList (fromJust(getNthVNode (ind) (x:xs) )) (x:xs) == 1) = getRepeats (x:xs) (ind+1)
                            | otherwise = [] -- should never be called
    
    countInstancesInVarList :: VarNode -> [VarNode] -> Int
    countInstancesInVarList vN [] = 0
    countInstancesInVarList vN (x:xs)   | (equateName vN x) == True = 1 + countInstancesInVarList (vN) (xs)
                                        | (equateName vN x) == False = 0 + countInstancesInVarList (vN) (xs)
    
    equateName :: VarNode -> VarNode -> Bool
    equateName (Vari (loc) (dat) (name)) (Vari (locB) (datB) (nameB)) = ( (dat == datB) && (name == nameB) )
    
    evaluate :: OpTree -> Bool --evaluate opTree freeVarList
    evaluate (EquateNode (l) (r))  =  ( checkEquality (EquateNode (l) (r))) 
    evaluate (RelationNode (loc) (varTr))  = checkRelation (RelationNode (loc) (varTr))
    evaluate (ConjunctionNode (l) (r))  = checkConjunction (ConjunctionNode (l) (r))
    --evaluate (VarTree ) varRow = 
    --evaluate (VarOp tree) freeVars = assignVars ((traverseDFVar (tree)) freeVars)
    evaluate (VarOp v)  = True
    
    checkExistential :: ExistTree -> Bool
    checkExistential (ExistVar (vTree) (oTree)) = (doesListExistInOpTree (traverseDFVar(vTree)) (oTree)) 
    checkExistential (ExistNest (vTree) (eTree) (oTree)) = (doesListExistInOpTree (traverseDFVar(vTree)) (oTree)) && checkExistential (eTree)
    
    checkConjunction :: OpTree -> Bool
    checkConjunction  (ConjunctionNode (l) (r) ) = (evaluate(l)) && (evaluate(r))
    
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
    
    
    
    
    -- evaluateE :: ExisitTree -> Bool
    -- evaluateE varTree opTree | (traverseDFVar (varTree)) --TODO
    
    {-==============================================================================-}
    {-=========================== TREE & NODE OPERATIONS ===========================-}
    {-==============================================================================-}
    getPTreeState :: ParseTree -> [VarNode]
    getPTreeState (Marker (vars) (oTree)) = getTreeState (oTree)
    getPTreeState (MarkerNested (vars) (eTree) ) = getETreeState (eTree)
    
    getETreeState :: ExistTree -> [VarNode]
    getETreeState (ExistVar (vars) (oTree)) = getTreeState(oTree)
    getETreeState (ExistNest (vars) (eTree) (oTree)) = getTreeState(oTree)
    
    
    getTreeState :: OpTree -> [VarNode]
    getTreeState (ConjunctionNode (opTree) (opTreeX)) = getTreeState(opTree) ++ getTreeState (opTreeX)
    getTreeState (RelationNode (string) (varTree)) = getTreeState(varToOpTree(varTree))
    getTreeState (EquateNode (opTree) (opTreeX)) = getTreeState (opTree) ++ getTreeState (opTreeX)
    getTreeState (LSubNode (opTree) (opTreeX)) = getTreeState (opTree) ++ getTreeState (opTreeX)
    getTreeState (RSubNode (opTree) (opTreeX)) = getTreeState (opTree) ++ getTreeState (opTreeX)
    getTreeState (BoolNode (bool)) = []
    getTreeState (VarOp (varTree)) = traverseDFVar(varTree)
    getTreeState (EmptyOT (emptyTree)) = []
    
    -- getTreeStateEx :: ExistTree -> [VarNode]
    -- getTreeStateEx ExistVar (VarTree) (OpTree) 
    -- getTreeStateEx ExistNest (VarTree) (ExistTree)
    -- getTreeStateEx EmptyET (EmptyTree)
    
    
    populateTree :: OpTree -> [String] -> Int -> OpTree
    populateTree (VarOp (vTree)) rList ind                      = (VarOp (populateVarTree (vTree) (rList) (ind)))
    populateTree (ConjunctionNode (querA) (querB)) rList ind    = (ConjunctionNode (populateTree (querA) (rList) (ind+popsC)) (populateTree (querB) (rList) (popsC+ind) ) )
                                                        where   popsC = countPopNodes (querA) + countPopNodes (querB)
    populateTree (EquateNode (querX) (querY)) rList ind         = (EquateNode (populateTree(querX) (rList) (popsX+ind)) (populateTree(querY) (rList) (popsX+ind)) )
                                                        where   popsX = countPopNodes (querX) + countPopNodes (querY)
    populateTree (RelationNode (tbl) (vTree)) rList ind         = populateRelation (RelationNode (tbl) (vTree)) rList ind
                                                        
    --populateParseTree :: ParseTree -> [String] ->
    
    populateExisTree :: ExistTree -> [String] -> ExistTree
    populateExisTree (ExistVar (vTree) (oTree)) rList = (ExistVar (populateVarTree (vTree) (rList) (0) ) (populateTree (oTree) (rList) (0) ))
    populateExisTree (ExistNest (vTree) (eTree) (oTree)) rList = (ExistNest (populateVarTree (vTree) (rList) (0) ) (populateExisTree (eTree) (rList) ) (populateTree (oTree) (rList) (0)))
    
    sanitiseExisTree :: ExistTree -> ExistTree
    sanitiseExisTree (ExistVar (vTree) (oTree)) = (ExistVar (sanitiseVarTree(vTree)) (sanitiseOpTree(oTree)))
    saniitseExisTree (ExistNest (vTree) (eTree) (oTree)) = (ExistNest (sanitiseVarTree(vTree)) (sanitiseExisTree(eTree)) (sanitiseOpTree(oTree)) )
    
    sanitiseOpTree :: OpTree -> OpTree
    sanitiseOpTree (RelationNode (tblName) (vTree)) = (RelationNode (tblName) (sanitiseVarTree(vTree)))
    sanitiseOpTree (VarOp (vT)) = (VarOp (sanitiseVarTree(vT)))
    sanitiseOpTree op = op
    
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
    populateVarTree ( CommaNode (Vari (loc) (dat) (name)) (remTree) ) (x:xs) ind  | isNodePopulated (Vari (loc) (dat) (name)) == True = (CommaNode (Vari (loc) (dat) (name)) (remTree))
    
    -- doesNameExistInVList :: String -> [VarNode] -> Bool
    -- doesNameExistInVList _  (x:xs) [] = False
    -- doesNameExistInVList targStr (x:xs) ((Vari (loc) (dat) (name)):ys)  | targStr == name = True
    --                                                                | targStr /= name = doesNameExistInVList targStr ys
    
    --If name already exists, assign variable with same name to the next unassigned 
    --Add syntax error here?
    generateNextVarData :: [String] -> Int -> String
    generateNextVarData (x:xs) ind  | ind < length(x:xs) = ( (x:xs)!!(ind + 1) ) --possibly unsafe
                                    | ind >= length(x:xs) = "THIS IS AN ERROR AND SHOULD NOT BE HERE"
    
    -- getDataMatchingName :: String -> [VarNode] -> VarNode
    
    -- doesVarNameExist :: [VarNode] -> String -> Bool
    
    isTreePopulated :: VarTree -> Bool
    isTreePopulated (SingleNode vNode) = isNodePopulated vNode
    isTreePopulated (CommaNode vNode remTree) = isNodePopulated vNode && isTreeAssigned remTree 
    
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
    
    
    extractPTableNames :: ParseTree -> [String]
    extractPTableNames (Marker (vars) (oTree)) = extractTableNames(getOpRelationNodesOut(oTree))
    extractPTableNames (MarkerNested (vars) (eTree)) = extractETableNames(eTree)
    
    extractETableNames :: ExistTree -> [String]
    extractETableNames (ExistVar (vTree) (oTree)) = extractTableNames(getOpRelationNodesOut(oTree))
    extractETableNames (ExistNest (vTree) (eTree) (oTree)) = extractTableNames(getOpRelationNodesOut(oTree))
    
    extractTableNames :: [OpTree] -> [String] -- takes output from liftRelationNodesOut, possibly needs to be reverse
    extractTableNames [] = []
    extractTableNames ( (RelationNode (tbl) (vTree)) :xs) = (tbl) : extractTableNames xs
    
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
    liftRelationNodesOut (MarkerNested vList eTree) = getExisRelationNodesOut(eTree)
    --liftRelationNodesOut (MarkerExtended vList eTree oTree) = getExisRelationNodesOut(eTree) ++ getOpRelationNodesOut(oTree)
    
    getExisRelationNodesOut :: ExistTree -> [OpTree]
    getExisRelationNodesOut (ExistVar (vTree) (oTree)) = getOpRelationNodesOut(oTree)
    getExisRelationNodesOut (ExistNest (vTree) (eTree) (oTree)) = getExisRelationNodesOut(eTree) ++ getOpRelationNodesOut(oTree)
    getExisRelationNodesOut (EmptyET empty) = []
    
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