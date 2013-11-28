module Core.Parser where

import Core.Parser.Utils
import Core.Types
import Core.Utils

-- We comment out the result of Exercise 1.10
-- lexCore :: [Char] -> NumberI -> [Token]
-- lexCore [] l = []
-- lexCore ('|':'|':cs) l = lexCore cs l -- Exercise 1.9
-- lexCore (c:c':cs) l
--   | [c, c'] `elem` twoCharOps = (l, [c, c']) : lexCore cs l -- Exercise 1.10
-- lexCore ('\n':cs) l = lexCore cs (l + 1)
-- lexCore (c:cs) l
--   | isWhiteSpace c = lexCore cs l
--   | digit c = let numTok = (l, c : takeWhile digit cs)
--                   rest = dropWhile digit cs
--               in numTok : lexCore rest l
--   | letter c = let varTok = (l, c : takeWhile isIdChar cs)
--                    rest = dropWhile isIdChar cs
--                in varTok : lexCore rest l
-- lexCore (c:cs) l = (l, [c]) : lexCore cs l

recursive :: Bool
recursive = True

nonRecursive :: Bool
nonRecursive = False

lexCore :: [Char] -> [Token]
lexCore [] = []
lexCore ('|':'|':cs) = lexCore cs -- Exercise 1.9
lexCore (c:c':cs)
  | [c, c'] `elem` twoCharOps = [c, c'] : lexCore cs -- Exercise 1.10
lexCore ('\n':cs) = lexCore cs
lexCore (c:cs)
  | isWhiteSpace c = lexCore cs
  | digit c = let numTok = c : takeWhile digit cs
                  rest = dropWhile digit cs
              in numTok : lexCore rest
  | letter c = let varTok = c : takeWhile isIdChar cs
                   rest = dropWhile isIdChar cs
               in varTok : lexCore rest
lexCore (c:cs) = [c] : lexCore cs


-- Exercise 1.10
twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

isIdChar :: Char -> Bool
isIdChar c = letter c || digit c || c == '_'

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []) : otherParses) = prog
    takeFirstParse other = error "Syntax error"


-- We could unsafePerformIO if the IO monad turns out to
-- be a problem: in Miranda we can just read the file without
-- reflecting it in the type.
parse :: [Char] -> IO CoreProgram
parse fname = syntax `fmap` lexCore `fmap` readFile fname

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

-- Exercise 1.20
mkSc :: Name -> [Name] -> String -> CoreExpr -> CoreScDefn
mkSc name vars sep expr = (name, vars, expr)

-- Exercise 1.21
pExpr :: Parser CoreExpr
pExpr = pThen4 (mkLet nonRecursive) (pLit "let") pDefns (pLit "in") pExpr
        `pAlt`
        pThen4 (mkLet nonRecursive) (pLit "letrec") pDefns (pLit  "in") pExpr
        `pAlt`
        pThen4 mkCase (pLit "case") pExpr (pLit "of") pAlts
        `pAlt`
        pThen4 mkLam (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
        `pAlt` pAExpr

pAExpr = pApply pVar EVar `pAlt` pApply pNum ENum
         `pAlt` pConstr `pAlt` paren pExpr
  where
    paren p = pThen3 (\_ expr _ -> expr) (pLit "(") p (pLit ")")

pConstr :: Parser CoreExpr
pConstr = pThen3 mkConstr (pLit "Pack{") numPair (pLit "}")
  where
    mkConstr _ (n, arity) _ = EConstr n arity
    numPair = pThen3 (\x _ y -> (x, y)) pNum (pLit ",") pNum

mkLet :: IsRec -> a -> [(Name, CoreExpr)] -> b -> CoreExpr -> CoreExpr
mkLet r _ defns _ expr = ELet r defns expr

mkCase :: String -> CoreExpr -> String -> [Alter Name] -> CoreExpr
mkCase _ expr _ alts = ECase expr alts

mkLam :: String -> [Name] -> String -> CoreExpr -> CoreExpr
mkLam _ vars _ expr = ELam vars expr

pAlts :: Parser [(Number, [Name], CoreExpr)]
pAlts = pOneOrMoreWithSep pAlt (pLit ";")
  where
    pAlt :: Parser (Alter Name)
    pAlt = pThen4 mkAlt bNum (pZeroOrMore pVar) (pLit "->") pExpr

    bNum :: Parser Number
    bNum = pThen3 (\_ n _ -> n) (pLit "<") pNum (pLit ">")

    mkAlt :: Number -> [Name] -> String -> CoreExpr -> Alter Name
    mkAlt n vars _ expr = (n, vars, expr)

pDefns :: Parser [(Name, CoreExpr)]
pDefns = pOneOrMoreWithSep pDefn (pLit ";")

pDefn = pThen3 (\v _ expr -> (v, expr)) pVar (pLit "=") pExpr
