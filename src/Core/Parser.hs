module Core.Parser where

import Core.Parser.Utils
import Core.Types
import Core.Utils

lexCore :: [Char] -> NumberI -> [Token]
lexCore [] l = []
lexCore ('|':'|':cs) l = lexCore cs l -- Exercise 1.9
lexCore (c:c':cs) l
  | [c, c'] `elem` twoCharOps = (l, [c, c']) : lexCore cs l -- Exercise 1.10
lexCore ('\n':cs) l = lexCore cs (l + 1)
lexCore (c:cs) l
  | isWhiteSpace c = lexCore cs l
  | digit c = let numTok = (l, c : takeWhile digit cs)
                  rest = dropWhile digit cs
              in numTok : lexCore rest l
  | letter c = let varTok = (l, c : takeWhile isIdChar cs)
                   rest = dropWhile isIdChar cs
               in varTok : lexCore rest l
lexCore (c:cs) l = (l, [c]) : lexCore cs l

-- Exercise 1.10
twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

isIdChar :: Char -> Bool
isIdChar c = letter c || digit c || c == '_'

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

syntax :: [Token] -> CoreProgram
syntax = undefined

-- We could unsafePerformIO if the IO monad turns out to
-- be a problem: in Miranda we can just read the file without
-- reflecting it in the type.
parse :: [Char] -> IO CoreProgram
parse fname = syntax `fmap` flip lexCore 0 `fmap` readFile fname
