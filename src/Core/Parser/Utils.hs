module Core.Parser.Utils where

import Core.Types
import Data.Char (isDigit, isLetter)

digit :: Char -> Bool
digit = isDigit

letter :: Char -> Bool
letter = isLetter

-- Strange, exercise 1.11 made us change Token
-- to something incompatible with the code in 1.6.2. We'll stick with
-- String until the book makes it clear whether we actually want to
-- use our changed Token, the default one, or maybe parametrise
-- Parser with another type variable.
type ParserToken = String

type Parser a = [ParserToken] -> [(a, [ParserToken])]

pLit :: [Char] -> Parser [Char]
pLit s [] = []
pLit s (tok:toks)
  | s == tok = [(s, toks)]
  | otherwise = []

pVar :: Parser [Char]
pVar [] = []
pVar (tok:toks)
  | letter (head tok) = [(tok, toks)]
  | otherwise = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                                                    , (v2, toks2) <- p2 toks1
                                                    ]
