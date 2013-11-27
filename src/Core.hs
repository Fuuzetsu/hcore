module Core where

import Core.Utils

data Expr a = EVar Name
            | ENum Number
            | EConstr Number Number
            | EAp (Expr a) (Expr a)
            | ELet IsRec [(a, Expr a)] (Expr a)
            | ECase (Expr a) [Alter a]
            | ELam [a] (Expr a)

type CoreExpr = Expr Name
type Name = String
type IsRec = Bool

recursive = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

type Alter a = (Number, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program String

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn String


preludeDefs :: CoreProgram
preludeDefs = [ ("I", ["x"], EVar "x")
              , ("K", ["x", "y"], EVar "x")
              , ("K1", ["x", "y"], EVar "y")
              , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                           (EAp (EVar "g") (EVar "x")))
              , ("compose", ["f", "g", "x"], EAp (EVar "f")
                                                 (EAp (EVar "g") (EVar "x")))
              , ("twice" , ["f"], EAp (EAp (EVar "compose") (EVar "f"))
                                      (EVar "f"))
              ]

pprint :: CoreProgram -> [Char]
pprint prog = iDisplay (pprProgram prog)

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr (shownum n)
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = pprExpr e1 `iAppend` iStr " " `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr) =
  iConcat [ iStr keyword, iNewline
          , iStr " ", iIndent (pprDefns defns), iNewline
          , iStr "in ", pprExpr expr
          ]
  where
    keyword
      | not isrec = "let"
      | isrec = "letrec"
pprExpr (ELam vs expr) =
  iStr "\\ " `iAppend` iInterleave (iStr " ") (map iStr vs)
  `iAppend` iStr "." `iAppend` pprExpr expr
pprExpr (ECase expr alts) =
  iStr "case " `iAppend` pprExpr expr `iAppend` iStr " of"
  `iAppend` iNewline `iAppend` iInterleave sep (map pprAlt alts)
  where
    sep = iConcat [ iStr ";", iNewline ]
    pprAlt (n, vs, expr) =
      iStr "<" `iAppend` iStr (shownum n) `iAppend` iStr "> "
      `iAppend` iInterleave (iStr " ") (map iStr vs) `iAppend` iStr " -> "
      `iAppend` iIndent (pprExpr expr)

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [ iStr name, iStr " = "
                               , iIndent (pprExpr expr)
                               ]

-- Exercise 1.2
iConcat :: [Iseq] -> Iseq
iConcat [] = iNil
iConcat [x] = x
iConcat (x:xs) = iAppend x (iConcat xs)

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep [] = iNil
iInterleave sep [x] = x
iInterleave sep (x:xs) = x `iAppend` sep `iAppend` iInterleave sep xs

-- Exercise 1.3
pprAExpr :: CoreExpr -> Iseq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave iNewline (map pprSc prog)
  where
    pprSc (name, [], expr) = pprDefn (name, expr)
    pprSc (name, vs, expr) = iConcat [ iStr name, iStr " "
                                     , iInterleave (iStr " ") (map iStr vs)
                                     , iStr " = " , iIndent (pprExpr expr)
                                     ]

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
  where
    e2s = e2 : e2s

iNil :: Iseq
iNil = INil

iStr :: [Char] -> Iseq
iStr s = IStr s

-- Exercise 1.5
iAppend :: Iseq -> Iseq -> Iseq
iAppend INil s2 = s2
iAppend s1 INil = s1
iAppend s1 s2 = IAppend s1 s2

iNewline :: Iseq
iNewline = IStr "\n"

iIndent :: Iseq -> Iseq
iIndent i = i

iDisplay :: Iseq -> [Char]
iDisplay s = flatten [s]

flatten :: [IseqRep] -> [Char]
flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ flatten seqs
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)

type Iseq = IseqRep

data IseqRep = INil
             | IStr [Char]
             | IAppend IseqRep IseqRep
