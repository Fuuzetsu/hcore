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
pLit s = pSat (== s)

pVar :: Parser [Char]
pVar = pSat (\x -> letter (head x) && x `notElem` keywords)

-- Exercise 1.17
keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                                                    , (v2, toks2) <- p2 toks1
                                                    ]

-- Exercise 1.12
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
  [ (v1 v2, toks2) | (v1, toks1) <- pThen combine p1 p2 toks
                   , (v2, toks2) <- p3 toks1
                   ]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b
          -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks =
  [ (v1 v2, toks2) | (v1, toks1) <- pThen3 combine p1 p2 p3 toks
                   , (v2, toks2) <- p4 toks1
                   ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

-- Exercise 1.13
pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

-- Exercise 1.14
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [ (f t, toks1) | (t, toks1) <- p toks ]

-- Exercise 1.15
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = p1 `j` (pThen k1 p2 (ps p1 p2) `pAlt` pEmpty [])
  where
    j = pThen (:)
    ps = pOneOrMoreWithSep
    k1 x y = y

-- Exercise 1.16
pSat :: ([Char] -> Bool) -> Parser [Char]
pSat f [] = []
pSat f (tok:toks)
  | f tok = [(tok, toks)]
  | otherwise = []

-- Exercise 1.18
pNum :: Parser NumberI
pNum = pApply (pSat $ all digit) read
