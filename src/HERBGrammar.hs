{-# OPTIONS_GHC -w #-}
module HERBGrammar where 
import HERBTokens
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

action_0 (11) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 _ = happyFail

action_1 (11) = happyShift action_3
action_1 (6) = happyGoto action_2
action_1 _ = happyFail

action_2 (20) = happyShift action_8
action_2 (21) = happyShift action_7
action_2 _ = happyFail

action_3 _ = happyReduce_7

action_4 (22) = happyAccept
action_4 _ = happyFail

action_5 (20) = happyShift action_6
action_5 (21) = happyShift action_7
action_5 _ = happyFail

action_6 (9) = happyShift action_10
action_6 (10) = happyShift action_11
action_6 (12) = happyShift action_12
action_6 (13) = happyShift action_15
action_6 (5) = happyGoto action_14
action_6 (7) = happyGoto action_9
action_6 _ = happyFail

action_7 (11) = happyShift action_3
action_7 (6) = happyGoto action_13
action_7 _ = happyFail

action_8 (9) = happyShift action_10
action_8 (10) = happyShift action_11
action_8 (12) = happyShift action_12
action_8 (7) = happyGoto action_9
action_8 _ = happyFail

action_9 (16) = happyShift action_19
action_9 (17) = happyShift action_20
action_9 (18) = happyShift action_21
action_9 (19) = happyShift action_22
action_9 _ = happyReduce_1

action_10 _ = happyReduce_13

action_11 _ = happyReduce_14

action_12 (11) = happyShift action_3
action_12 (6) = happyGoto action_18
action_12 _ = happyFail

action_13 _ = happyReduce_6

action_14 (9) = happyShift action_10
action_14 (10) = happyShift action_11
action_14 (12) = happyShift action_12
action_14 (7) = happyGoto action_17
action_14 _ = happyReduce_2

action_15 (11) = happyShift action_3
action_15 (6) = happyGoto action_16
action_15 _ = happyFail

action_16 (14) = happyShift action_28
action_16 (21) = happyShift action_7
action_16 _ = happyFail

action_17 (16) = happyShift action_19
action_17 (17) = happyShift action_20
action_17 (18) = happyShift action_21
action_17 (19) = happyShift action_22
action_17 _ = happyReduce_3

action_18 (15) = happyShift action_27
action_18 (21) = happyShift action_7
action_18 _ = happyFail

action_19 (9) = happyShift action_10
action_19 (10) = happyShift action_11
action_19 (12) = happyShift action_12
action_19 (7) = happyGoto action_26
action_19 _ = happyFail

action_20 (9) = happyShift action_10
action_20 (10) = happyShift action_11
action_20 (12) = happyShift action_12
action_20 (7) = happyGoto action_25
action_20 _ = happyFail

action_21 (9) = happyShift action_10
action_21 (10) = happyShift action_11
action_21 (12) = happyShift action_12
action_21 (7) = happyGoto action_24
action_21 _ = happyFail

action_22 (9) = happyShift action_10
action_22 (10) = happyShift action_11
action_22 (12) = happyShift action_12
action_22 (7) = happyGoto action_23
action_22 _ = happyFail

action_23 (17) = happyShift action_20
action_23 _ = happyReduce_12

action_24 (17) = happyShift action_20
action_24 _ = happyReduce_11

action_25 (17) = happyShift action_20
action_25 _ = happyReduce_10

action_26 (17) = happyShift action_20
action_26 _ = happyReduce_8

action_27 _ = happyReduce_9

action_28 (8) = happyShift action_29
action_28 _ = happyFail

action_29 (13) = happyShift action_30
action_29 _ = happyFail

action_30 (9) = happyShift action_10
action_30 (10) = happyShift action_11
action_30 (12) = happyShift action_12
action_30 (13) = happyShift action_15
action_30 (5) = happyGoto action_31
action_30 (7) = happyGoto action_32
action_30 _ = happyFail

action_31 (14) = happyShift action_34
action_31 _ = happyFail

action_32 (14) = happyShift action_33
action_32 (16) = happyShift action_19
action_32 (17) = happyShift action_20
action_32 (18) = happyShift action_21
action_32 (19) = happyShift action_22
action_32 _ = happyFail

action_33 _ = happyReduce_4

action_34 (13) = happyShift action_35
action_34 _ = happyFail

action_35 (9) = happyShift action_10
action_35 (10) = happyShift action_11
action_35 (12) = happyShift action_12
action_35 (7) = happyGoto action_36
action_35 _ = happyFail

action_36 (14) = happyShift action_37
action_36 (16) = happyShift action_19
action_36 (17) = happyShift action_20
action_36 (18) = happyShift action_21
action_36 (19) = happyShift action_22
action_36 _ = happyFail

action_37 _ = happyReduce_5

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (Evaluate happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (Eval happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (EvalExisExt happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 7 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExistentialSingle happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 10 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExistentialNested happy_var_2 happy_var_6 happy_var_9
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Comma happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn6
		 (VarSingle happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Conjunction happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokenRelation _ happy_var_1))
	 =  HappyAbsSyn7
		 (Relation happy_var_1 happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Equality happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (LSub happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (RSub happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn7
		 (Bool True
	)

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn7
		 (Bool False
	)

happyNewToken action sts stk [] =
	action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenExistential _ happy_dollar_dollar -> cont 8;
	TokenBool _ -> cont 9;
	TokenBool _ -> cont 10;
	TokenVar _ happy_dollar_dollar -> cont 11;
	TokenRelation _ happy_dollar_dollar -> cont 12;
	TokenLParen _ -> cont 13;
	TokenRParen _ -> cont 14;
	TokenRCurly _ -> cont 15;
	TokenConjunction _ happy_dollar_dollar -> cont 16;
	TokenEquality _ happy_dollar_dollar -> cont 17;
	TokenLSubset _ happy_dollar_dollar -> cont 18;
	TokenRSubset _ happy_dollar_dollar -> cont 19;
	TokenEntailment _ happy_dollar_dollar -> cont 20;
	TokenComma _ -> cont 21;
	_ -> happyError' (tk:tks)
	}

happyError_ 22 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseCalc tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError token = error "Parse error on Token: " ++ read tokenPosn token

data Exp = Evaluate Variables Query
    | Eval Variables Existential
    | EvalExisExt Variables Existential Query
    deriving Show

data Variables = Comma Variables Variables
    | VarSingle String
    deriving Show

data Query = Conjunction Query Query
    | Relation String Variables
    | Equality Query Query
    | LSub Query Query
    | RSub Query Query
    | Bool Bool
    | V Variables
    deriving Show

data Existential = ExistentialSingle Variables Query
    | ExistentialNested Variables Existential Query
    deriving Show
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc8056_0\\ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
