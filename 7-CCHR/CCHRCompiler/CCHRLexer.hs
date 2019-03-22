--------------------------------------------------------------------------------
--
-- Copyright (C) 2006 
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation; either version 2 of the License, or (at your option)
-- any later version. This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--
--------------------------------------------------------------------------------

module CCHRLexer where

import Char
import List

data Token = TokenData
           | TokenRule 
           | TokenName
           | TokenCons
           | TokenSemi
           | TokenVar String
           | TokenConst String
           | TokenLBrack
           | TokenRBrack
           | TokenHaskell String
           | TokenComma
           | TokenEq
           | TokenBar
           | TokenSimp
           | TokenProp
           | TokenMLBrack
           | TokenMRBrack
           | TokenTrue
           | TokenFalse
           | TokenLList
           | TokenRList
           | TokenSLBrack
           | TokenSRBrack
           | TokenSComma
           | TokenCollen
           | TokenAt
           | TokenArrow
           | TokenWild
           | TokenError

instance Show Token where
   show TokenData = "Data"
   show TokenRule = "Rule"
   show TokenName = "Name"
   show TokenCons = "Cons"
   show TokenSemi = "Semi"
   show (TokenVar s) = "Var " ++ s
   show (TokenConst s) = "Const " ++ s
   show TokenLBrack = "LBrack"
   show TokenRBrack = "RBrack"
   show (TokenHaskell s) = "Hask " ++ s
   show TokenComma = "Comma"
   show TokenEq = "Eq"
   show TokenBar = "Bar"
   show TokenSimp = "Simp"
   show TokenProp = "Prop"
   show TokenMLBrack = "MLBrack"
   show TokenMRBrack = "MRBrack"
   show TokenTrue = "True"
   show TokenFalse = "False"
   show TokenLList = "LList"
   show TokenRList = "RList"
   show TokenSLBrack = "SLBrack"
   show TokenSRBrack = "SRBrack"
   show TokenSComma = "SComma"
   show TokenCollen = "Collen"
   show TokenAt = "At"
   show TokenArrow = "Arrow"
   show TokenWild  = "Wild"
   show TokenError = "Error"

partitionS :: String -> String -> (String,String)
partitionS rs (';':cs)  = (rs,cs)
partitionS rs ('\n':cs) = (rs,cs)
partitionS rs (r:cs)   = partitionS (rs ++ [r]) cs
partitionS rs [] = (rs,[])

lexerTop :: String -> [Token]
lexerTop [] = []
lexerTop ('\n':cs) = lexerTop cs
lexerTop (';':cs)  = lexerTop cs
lexerTop (' ':cs) = lexerTop cs
lexerTop ('d':'a':'t':'a':cs) =
     let (cns,cs') = partitionS [] cs
     in (TokenData:(lexerCons cns)) ++ [TokenSemi] ++ (lexerTop cs')
lexerTop ('r':'u':'l':'e':cs) = 
     let (rs,cs') = partitionS [] cs
     in (TokenRule:(lexerRule rs)) ++ [TokenSemi] ++ (lexerTop cs')
lexerTop ('n':'a':'m':'e':cs) = 
     let (ns,cs') = partitionS [] cs
     in (TokenName:(lexerName ns)) ++ [TokenSemi] ++ (lexerTop cs')
lexerTop ('c':'o':'n':'s':'t':'r':'a':'i':'n':'t':cs) =
     let (cns,cs') = partitionS [] cs
     in (TokenCons:(lexerCons cns)) ++ [TokenSemi] ++ (lexerTop cs')
lexerTop ('{':cs) = let (h,rest) = span cond cs
                    in TokenMLBrack:(TokenHaskell h):(lexerTop rest)
                    where
                       cond c = not ('}' == c)
lexerTop ('}':cs) = TokenMRBrack:TokenSemi:(lexerTop cs) 
lexerTop _ = [TokenError]    

lexerRule :: String -> [Token]
lexerRule [] = []
lexerRule (c:cs)
        | isSpace c = lexerRule cs
        | isAlpha c = let (t,cs') = lexerId (c:cs)
                      in t:(lexerRule cs')
lexerRule ('<':'=':'=':'>':cs) = TokenSimp:(lexerBody cs)
lexerRule ('=':'=':'>':cs) = TokenProp:(lexerBody cs)
lexerRule ('(':cs) = let (tArgs,cs') = lexerArgs [] cs
                     in (TokenSLBrack:tArgs) ++ (lexerRule cs')
lexerRule (')':cs) = TokenSRBrack:(lexerRule cs)
lexerRule (',':cs) = TokenComma:(lexerRule cs)
lexerRule _ = [TokenError]

lexerBody :: String -> [Token]
lexerBody [] = []
lexerBody (' ':cs) = lexerBody cs
lexerBody ('T':'r':'u':'e':cs) = TokenTrue:(lexerBody cs)
lexerBody ('F':'a':'l':'s':'e':cs) = TokenFalse:(lexerBody cs) 
lexerBody ('|':cs) = TokenBar:(lexerBody cs)
lexerBody cs = let (s,cs') = span cond cs
               in (TokenConst s):(lexerBody cs')
               where
                  cond c = not ('|' == c)

lexerArgs :: [Token] -> String -> ([Token],String)
lexerArgs ts [] = (ts,[])
lexerArgs ts (')':cs) = (ts,')':cs)
lexerArgs ts (',':cs) = lexerArgs (ts ++ [TokenSComma]) cs
lexerArgs ts cs = let (arg,rest) = partitionArgs 0 [] cs
                      ts' = lexerArg arg
                  in (lexerArgs (ts ++ ts') rest)

partitionArgs :: Int -> String -> String -> (String,String)
partitionArgs 0 arg (')':cs) = (arg,')':cs)
partitionArgs 0 arg (',':cs) = (arg,',':cs)
partitionArgs i arg ('(':cs) = partitionArgs (i+1) (arg ++ ['(']) cs
partitionArgs i arg (')':cs) = partitionArgs (i-1) (arg ++ [')']) cs
partitionArgs i arg (c:cs)   = partitionArgs i (arg ++ [c]) cs
partitionArgs _ arg [] = (arg,[])

lexerArg :: String -> [Token]
lexerArg (c:cs) | isAlpha c = let (t,rest) = lexerId (c:cs)
                              in t:(lexerArg rest)
lexerArg (c:cs) | isDigit c = let (t,rest) = lexerDigits (c:cs)
                              in t:(lexerArg rest)
lexerArg (c:cs) | isSpace c = lexerArg cs
lexerArg ('-':'>':cs) = TokenArrow:(lexerArg cs)
lexerArg ('(':cs) = TokenLBrack:(lexerArg cs)
lexerArg (')':cs) = TokenRBrack:(lexerArg cs)
lexerArg (',':cs) = TokenComma:(lexerArg cs)
lexerArg (':':cs) = TokenCollen:(lexerArg cs)
lexerArg ('[':cs) = TokenLList:(lexerArg cs)
lexerArg (']':cs) = TokenRList:(lexerArg cs)
lexerArg ('@':cs) = TokenAt:(lexerArg cs)
lexerArg ('_':cs) = TokenWild:(lexerArg cs)
lexerArg [] = []
lexerArg _  = [TokenError]

lexerName :: String -> [Token]
lexerName (c:cs)
        | isSpace c = lexerName cs
        | isAlpha c = let (t,_) = lexerId (c:cs)
                      in [t]
lexerName _ = []

lexerCons :: String -> [Token]
lexerCons [] = []
lexerCons (' ':cs) = lexerCons cs
lexerCons ('=':cs) = TokenEq:(lexerCons cs)
lexerCons ('|':cs) = TokenBar:(lexerCons cs)
lexerCons ('(':cs) = let (tArgs,cs') = lexerArgs [] cs
                     in (TokenSLBrack:tArgs) ++ (lexerCons cs')
lexerCons (')':cs) = TokenSRBrack:(lexerCons cs)
lexerCons (c:cs) | isAlpha c = let (cons,rest) = lexerId (c:cs)
                               in cons:(lexerCons rest)
lexerCons _ = [TokenError]

lexerId :: String -> (Token,String)
lexerId cs = let (c:cons,rest) = span alphaOrDigit cs
                 c' = toUpper c
             in case (c==c') of
                   True  -> (TokenConst (c:cons),rest)
                   False -> (TokenVar (c:cons),rest)
             where
                alphaOrDigit c = isAlpha c || isDigit c
 
lexerDigits :: String -> (Token,String)
lexerDigits cs = let (d,rest) = span cond cs
                 in (TokenConst d,rest)
                 where
                    cond c = (isDigit c) || ('.' == c)

f = "name Gcd ; constraint MyCons = A(String,[Int]) | B | C(Bool String,Int) ;"

g = "rule C((Don x,z):xs),A(x),C() <==> (Don x,z) == y | D(),F(z:ds,(show d) ++ fd) ;"

h = "constraint Crap = Crap(Cell Int Int,Bool)"

{-
main :: IO ()
main = do sd <- readFile "test.chr"
          putStr (show (stream sd))

stream ('\n':cs) = stream cs
stream (c:cs) = [c]:(stream cs)
stream [] = []
-}
