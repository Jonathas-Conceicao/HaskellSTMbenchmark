module Parser where

import Soup( PlotItem(..), Mode(..) )
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as TP
import Text.ParserCombinators.Parsec.Language( emptyDef )


----------------------------------
-- External interface
----------------------------------

parseProgram :: String -> String -> [Decl]
parseProgram filename cts
  = case parse pProgram filename cts of
	Left err -> error (show err)
	Right ds -> ds

----------------------------------
-- Data types
----------------------------------

data Decl 
  = NewDecl String PValue PType		-- new ch@10.0:chan
  | ValDecl String PValue		-- val x = 3.4
  | LetDecl String [Pattern] PProc	-- let x(a,b) = p
  | Plot [PlotItem String String]
  | Run Int PProc
  | Sample Double Int
  deriving( Show )

isValDecl, isPlotDecl, isRunDecl :: Decl -> Bool
isValDecl (ValDecl {}) = True
isValDecl (NewDecl {}) = True
isValDecl (LetDecl {}) = True
isValDecl other = False

isPlotDecl (Plot _) = True
isPlotDecl other = False

isRunDecl (Run _ _) = True
isRunDecl other = False

data PProc
  = PPar [PProc]		-- Empty list is (), the empty process
  | PCall String [PValue]
  | PDo [(Action,PProc)]	  
  | PDecl Decl PProc	 	-- Actually only New is allowed at the moment
  deriving( Show )

data Action
  = InAct String [Pattern] 
  | OutAct String [PValue]
  | DelayAct PValue
  deriving( Show )

-- For now...
data PValue 
  = IdVal String | LitVal Double 
  deriving( Show )

type Pattern = String
type PType   = ()

	

----------------------------------
-- Parsing processes
----------------------------------

pProgram :: CharParser st [Decl]
pProgram = do { whiteSpace; dss <- many pDecl; eof; return (concat dss) }

pDecl :: CharParser st [Decl]
pDecl = choice [ pTypeDecl, pValDecl, pNewDecl, pLet, pDirective, pRun ]

pRun :: CharParser st [Decl]
pRun = do { reserved "run"; n <- pRunIter; p <- pProc; return [Run n p] }

pRunIter = choice [ do { n <- integer; reserved "of"; return n }, 
		    return 1 ]

pDirective :: CharParser st [Decl]
pDirective = do { reserved "directive"; choice [pPlot, pSample] }

pPlot :: CharParser st [Decl]
pPlot = do { reserved "plot"; is <- sepBy1 pPlotItem (symbol ";")
	   ; return [Plot is] }

pPlotItem :: CharParser st (PlotItem String String)
pPlotItem = choice [ do { symbol "?"; id <- identifier; s <- pAs ("?"++id); return (PlotChan id InMode s) },
		     do { symbol "!"; id <- identifier; s <- pAs ("!"++id); return (PlotChan id OutMode s) },
		     do { fn <- identifier; symbol "("; symbol ")"; s <- pAs fn; return (PlotProc fn s) } ]

pAs :: String -> CharParser st String
pAs s = choice [do { reserved "as"; stringLiteral },
		return s]

pSample :: CharParser st [Decl]
pSample = do { reserved "sample"; time <- float 
	     ; count <- choice [integer, return 100] 
	     ; return [Sample time count] }

pTypeDecl :: CharParser st [Decl]
	-- We just discard type declarations
pTypeDecl = do	{ reserved "type"; identifier; symbol "="; pType; return [] }

pValDecl :: CharParser st [Decl]
pValDecl = do	{ reserved "val"; id <- identifier
		; symbol "="; val <- pValue
		; return [ValDecl id val] }

pNewDecl :: CharParser st [Decl]
pNewDecl = do	{ reserved "new"; id <- identifier
		; rate <- choice [do {symbol "@"; pValue},
				  return (LitVal (1/0))]	-- Infinite rate yuk
		; symbol ":"; ty <- pType
		; return [NewDecl id rate ty] }

pLet :: CharParser st [Decl]
pLet = do { reserved "let"
	  ; d <- pBind
	  ; ds <- many (reserved "and" >> pBind)
	  ; return (d : ds) }

pBind :: CharParser st Decl
pBind = do { id <- identifier
	   ; args <- parens pPats
	   ; symbol "="
	   ; rhs <- pParProc
	   ; return (LetDecl id args rhs) }

pParProc, pProc :: CharParser st PProc
pParProc = do { ps <- sepBy pProc (symbol "|"); 
	      ; case ps of 
		  [p] -> return p
		  ps  -> return (PPar ps) }
pProc 
  = choice [
	do { id <- identifier; args <- parens pValues; return (PCall id args) },
	do { act <- pActionProcess; return (PDo [act]) },
	do { reserved "do"; acts <- sepBy pActionProcess (reserved "or"); return (PDo acts) },
	try $ parens pParProc,
	parens (do { ds <- many1 pDecl; p <- pParProc
		   ; return (foldr PDecl p (concat ds)) })
    ]

pActionProcess :: CharParser st (Action,PProc)
pActionProcess
  = do { a <- pAction
       ; p <- choice [ symbol ";" >> pParProc, return (PPar []) ]
       ; return (a,p) } 

pAction :: CharParser st Action
pAction = choice [ do { symbol "!"; ch <- identifier; vals <- optParend pValues; return (OutAct ch vals) },
		   do { symbol "?"; ch <- identifier; pats <- optParend pPats; return (InAct ch pats) },
		   do { reserved "delay"; symbol "@"; rate <- pValue; return (DelayAct rate) } ]

pPats :: CharParser st [String]
-- A list of comma-separated patterns (currently identifiers)
pPats = sepBy pPat (symbol ",")

pPat :: CharParser st String
pPat = do { n <- identifier; choice [ symbol ":" >> pType, return () ]
	  ; return n }

pValues :: CharParser st [PValue]
-- A list of comma-separated values
pValues = sepBy pValue (symbol ",")

pValue :: CharParser st PValue
pValue = choice [ do { id <- identifier; return (IdVal id) },
		  do { n  <- float; return (LitVal n) } ]

pType :: CharParser st PType
pType = do { identifier; choice [do { parens (sepBy pType (symbol ",")); return () },
				      return ()] }

optParend :: CharParser st [a] -> CharParser st [a]
-- Either (a,b,c), or just empty
optParend p = choice [ parens p, return [] ]

----------------------------------
-- Lexical analysis
----------------------------------

lexer :: TP.TokenParser st 
lexer = TP.makeTokenParser (emptyDef {
		TP.commentStart = "(*",
		TP.commentEnd   = "*)",
		TP.commentLine   = "--",
		TP.reservedNames = ["directive", "sample", "plot", "as",
				 "new", "type", "val", "run", "let", 
				 "and", "do", "replicate",	
				 "if", "then", "else", "match", "case",
				 "delay", "of" ]
	})

whiteSpace :: CharParser st ()
whiteSpace = TP.whiteSpace lexer

parens     :: CharParser st a -> CharParser st a
parens      = TP.parens lexer

integer    :: CharParser st Int
integer     = do { i <- TP.integer lexer; return (fromIntegral i) }

float    :: CharParser st Double
float     = TP.float lexer

reservedOp :: String -> CharParser st ()
reservedOp  = TP.reservedOp lexer

reserved   :: String -> CharParser st ()
reserved    = TP.reserved lexer

symbol     :: String -> CharParser st String
symbol      = TP.symbol lexer

stringLiteral :: CharParser st String
stringLiteral = TP.stringLiteral lexer

identifier :: CharParser st String
identifier  = TP.identifier lexer 
