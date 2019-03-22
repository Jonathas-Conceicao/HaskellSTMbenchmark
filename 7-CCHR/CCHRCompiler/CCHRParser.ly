> {
> module CCHRParser where
> import Char
> import List
> import CCHRLexer
> }

> %name cCHRParser
> %tokentype { Token }

> %token 
>     data		{ TokenData }
>	rule		{ TokenRule }
>	name		{ TokenName }
>	cons		{ TokenCons }
>	';'		{ TokenSemi }
>	var		{ TokenVar $$ }
>	const		{ TokenConst $$ }
>	'('		{ TokenLBrack }
>	')'		{ TokenRBrack }
>	haskell	{ TokenHaskell $$ }
>	','		{ TokenComma }
>	'='		{ TokenEq }
>	'|'		{ TokenBar }
>	"<==>"	{ TokenSimp }
>	"==>"		{ TokenProp }
>	'{'		{ TokenMLBrack }
>	'}'		{ TokenMRBrack}
>     true        { TokenTrue }
>     false       { TokenFalse }
>	'['		{ TokenLList }
>	']'		{ TokenRList }
>	"(("		{ TokenSLBrack }
>	"))"		{ TokenSRBrack }
>	",,"		{ TokenSComma }
>	':'		{ TokenCollen }
>	'@'		{ TokenAt }
>     "->"		{ TokenArrow }
>     '_'         { TokenWild }
>	"Err"		{ TokenError }		

> %%

> Exprs :: { [Expr] }
> Exprs : Expr ';'	 { [$1] }
>       | Expr ';' Exprs { $1:$3 }
> 
> Expr : name var					    { Name $2 }
>      | name const				    { Name $2 }
>      | '{' haskell '}'			    { Code $2 }
>      | cons const '=' Constr		    { Cons $2 $4 }
>      | data Type '=' Dat			    { Data (parseType $2) $4 }
>      | rule Patterns "<==>" const '|' Body  { CHR (SimpRule $2 (Guard $4) $6) }
>      | rule Patterns "<==>" Body		    { CHR (SimpRule $2 NoGuard $4) }
>      | rule Patterns "==>" const '|' Body   { CHR (PropRule $2 (Guard $4) $6) }
>      | rule Patterns "==>" Body             { CHR (PropRule $2 NoGuard $4) }
>
> Dat : const OTypes		{ [TypeSig $1 $2] }
>     | const OTypes '|' Dat  { (TypeSig $1 $2):$4 }
>
> OTypes : 			{ [] }
>	   | Type		{ $1 }
>
> Constr : const						{ [TypeSig $1 []] }
>        | const "((" Types "))"			{ [TypeSig $1 $3] }
>        | const '|' Constr				{ (TypeSig $1 []):$3 }
>        | const "((" Types "))" '|' Constr	{ (TypeSig $1 $3):$6 }
>
> Types : Type			  { [(parseType $1)] }
>       | Type ",," Types	  { (parseType $1):$3 }
>
> Type : var			{ [VarType $1] }
>      | '[' Type ']'   	{ [ListType (parseType $2)] }
>      | '(' TupleTypes ')'	{ [TupleType $2] }
>      | '(' Type ')'		{ [(parseType $2)] }
>	 | ArrayTypes		{ $1 }
>      | Type "->" Type		{ [FuncType (parseType $1) (parseType $3)] }
>
> ArrayTypes : const 			{ [ConstType $1] }
>            | ArrayTypes Type	{ $1 ++ $2 }
>
> TupleTypes : Type ',' TupleTypesP	{ (parseType $1):$3 }
>
> TupleTypesP : Type				{ [(parseType $1)] }
>             | Type ',' TupleTypesP	{ (parseType $1):$3 }
>
> Patterns : Pattern			{ [$1] }
>          | Pattern ',' Patterns	{ $1:$3 }
>
> Pattern : const					{ Pattern $1 [] }
>         | const "((" "))"               { Pattern $1 [] }
>         | const "((" VarTerms "))"	{ Pattern $1 $3 }
>
> VarTerms : VarTerm 			{ [(parsePat $1)] }
>          | VarTerm ",," VarTerms	{ (parsePat $1):$3 }
>
> VarTerm : var					{ [Var $1] }
>         | VarTerm ':' VarTerm		{ [ListPat1 (parsePat $1) (parsePat $3)] }
>         | '[' CommaTerms ']'		{ [ListPat2 $2] }
>         | '[' VarTerm ']'               { [ListPat2 [(parsePat $2)]] }
>         | '[' ']'				{ [ListPat2 []] }
>         | '(' CommaTerms ')'		{ [TuplePat $2] }
>         | ArrayTerms 				{ $1 }
>         | var '@' VarTerm			{ [AtPat $1 (parsePat $3)] }
>         | '(' VarTerm ')'			{ [BrackPat (parsePat $2)] }
>         | '_'					{ [WildCardPat] }
>
> CommaTerms : VarTerm ',' CommaTermsP	{ (parsePat $1):$3 }
>
> CommaTermsP : VarTerm				{ [(parsePat $1)] }
>             | VarTerm ',' CommaTermsP	{ (parsePat $1):$3 }
>
> ArrayTerms : const			{ [Const $1] }
>            | ArrayTerms VarTerm	{ $1 ++ $2 }
>
> Body : const	{ Body $1 }
>      | true	{ NoBody }
>      | false	{ FalseBody }

> {

>
> happyError :: [Token] -> a
> happyError _ = error ("Parse error\n")
>
> type HSyn  = String
> type VarId = String
>
> data TypeSig = TypeSig String [Type]
> data Type = ListType Type | TupleType [Type] | ConstrType String [Type] | ConstType String | VarType String
>           | FuncType Type Type | UnknownType
> data TermPat = Var HSyn | Const HSyn | ListPat1 TermPat TermPat | ListPat2 [TermPat] | TuplePat [TermPat] 
>              | ConstrPat String [TermPat] | AtPat String TermPat | BrackPat TermPat | WildCardPat
> data Pattern = Pattern HSyn [TermPat] 
> data CHRRule = SimpRule [Pattern] Guard Body | PropRule [Pattern] Guard Body
> data Guard   = Guard HSyn | NoGuard
> data Body    = Body HSyn | NoBody | FalseBody
> data Expr    = Name String | CHR CHRRule | Code HSyn | Cons String [TypeSig] | Data Type [TypeSig]
>
> parseType :: [Type] -> Type
> parseType [ConstType c] = ConstType c
> parseType ((ConstType c):ts) = ConstrType c ts
> parseType [t] = t
> parseType _ = error "Error in parseType"                 
>
> parsePat :: [TermPat] -> TermPat
> parsePat [Const s] = Const s
> parsePat ((Const s):ps) = ConstrPat s ps
> parsePat [p] = p
> parsePat _ = error "Error in parsePat"
>
> instance Eq Type where
>    (ListType t1) == (ListType t2)   = t1 == t2
>    (TupleType t1) == (TupleType t2) = t1 == t2
>    (ConstrType s1 t1) == (ConstrType s2 t2) = (s1 == s2) && (t1 == t2)
>    (ConstType s1) == (ConstType s2) = s1 == s2
>    (VarType s1) == (VarType s2) = s1 == s2
>    (FuncType t1 t1') == (FuncType t2 t2') = (t1 == t2) && (t1' == t2')
>    _ == _ = False
>
> instance Eq TypeSig where
>    (TypeSig s1 t1) == (TypeSig s2 t2) = (s1 == s2) && (t1 == t2)
>
> instance Show TypeSig where
>    show (TypeSig s ts) = "TypeSig(" ++ s ++ "," ++ (show ts) ++ ")"
>
> instance Show Type where
>    show (ListType t)   = "List(" ++ (show t) ++ ")"
>    show (TupleType ts) = "Tuple(" ++ (show ts) ++ ")"
>    show (ConstrType s ts) = "Constr(" ++ s ++ "," ++ (show ts) ++ ")"
>    show (ConstType s)  = "Const(" ++ s ++ ")"
>    show (VarType s) = "Var(" ++ s ++ ")"
>    show (FuncType t1 t2) = "Arrow(" ++ (show t1) ++ "," ++ (show t2) ++ ")"
>    show UnknownType = "Unknown()"
>
> instance Show TermPat where
>    show (Var x)   = "Var(" ++ x ++ ")"
>    show (Const x) = "Const(" ++ x ++ ")"
>    show (ListPat1 t1 t2) = "List1(" ++ (show t1) ++ "," ++ (show t2) ++ ")"
>    show (ListPat2 ts) = "List2(" ++ (show ts) ++ ")"
>    show (TuplePat ts) = "Tuple(" ++ (show ts) ++ ")"
>    show (ConstrPat s ts) = "Constr(" ++ s ++ "," ++ (show ts) ++ ")"
>    show (AtPat s t) = "At(" ++ s ++ "," ++ (show t) ++ ")"
>    show (BrackPat t) = "Brack(" ++ (show t) ++ ")"
>    show WildCardPat = "WildCard"
>
> instance Show Pattern where
>    show (Pattern s ts) = "Pattern(" ++ s ++ ","++ (show ts) ++ ")" 
>
> instance Show CHRRule where
>    show (SimpRule cons g b) = "SimpRule(" ++ (show cons) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")"
>    show (PropRule cons g b) = "PropRule(" ++ (show cons) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")"
>
> instance Show Guard where
>    show (Guard g) = "Guard(" ++ (show g) ++ ")"
>    show (NoGuard) = "Guard(True)"
>
> instance Show Body where
>    show (Body b)  = "Body(" ++ (show b) ++ ")"
>    show NoBody    = "Body(True)"
>    show FalseBody = "Body(False)"
>
> instance Show Expr where
>    show (Name s) = "Name(" ++ s ++ ")"
>    show (CHR c)  = "CHR(" ++ (show c) ++ ")"
>    show (Code s) = "Code(" ++ (show s) ++ ")"
>    show (Cons s cs) = "Cons(" ++ s ++ "," ++ (show cs) ++ ")"
>    show (Data s cs) = "Data(" ++ (show s) ++ "," ++ (show cs) ++ ")"
>
> runParser :: String -> IO [Expr] 
> runParser s = do let ts = lexerTop s
>                  return (hCHRParser ts)
>

> }
