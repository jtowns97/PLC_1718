myinterpreter: HERBTokens.hs HERBGrammar.hs myinterpreter.hs
	 ghc myinterpreter.hs
HERBTokens.hs: HERBTokens.x
		 alex HERBTokens.x
HERBGrammar.hs: HERBGrammar.y
		 happy HERBGrammar.y