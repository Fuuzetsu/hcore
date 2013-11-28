module Core.Pretty where

import Core.Types
import Core.Utils


pprint :: CoreProgram -> [Char]
pprint prog = iDisplay (pprProgram prog)


pprInfixExpr :: Name -> Number -> CoreExpr -> CoreExpr -> Number -> Iseq
pprInfixExpr op inf e1 e2 prec
  | inf < prec = iConcat [ iStr "(", pprExpr e1 inf, iStr pOp
                       , pprExpr e2 inf, iStr ")"]
  | otherwise = iConcat [pprExpr e1 inf, iStr pOp, pprExpr e2 inf]
  where
    pOp = " " ++ op ++ " "

pprExpr :: CoreExpr -> Number -> Iseq
pprExpr (ENum n) prec = iStr (shownum n)
pprExpr (EVar v) prec = iStr v
pprExpr (EAp (EAp (EVar "*") e1) e2) prec = pprInfixExpr "*" 5 e1 e2 prec
pprExpr (EAp (EAp (EVar "/") e1) e2) prec = pprInfixExpr "/" 5 e1 e2 prec
pprExpr (EAp (EAp (EVar "+") e1) e2) prec = pprInfixExpr "+" 4 e1 e2 prec
pprExpr (EAp (EAp (EVar "-") e1) e2) prec = pprInfixExpr "-" 4 e1 e2 prec
pprExpr (EAp (EAp (EVar "==") e1) e2) prec = pprInfixExpr "==" 3 e1 e2 prec
pprExpr (EAp (EAp (EVar "~=") e1) e2) prec = pprInfixExpr "~=" 3 e1 e2 prec
pprExpr (EAp (EAp (EVar ">") e1) e2) prec = pprInfixExpr ">" 3 e1 e2 prec
pprExpr (EAp (EAp (EVar ">=") e1) e2) prec = pprInfixExpr ">=" 3 e1 e2 prec
pprExpr (EAp (EAp (EVar "<") e1) e2) prec = pprInfixExpr "<" 3 e1 e2 prec
pprExpr (EAp (EAp (EVar "<=") e1) e2) prec = pprInfixExpr "<=" 3 e1 e2 prec
pprExpr (EAp (EAp (EVar "&") e1) e2) prec = pprInfixExpr "&" 2 e1 e2 prec
pprExpr (EAp (EAp (EVar "|") e1) e2) prec = pprInfixExpr "|" 1 e1 e2 prec
pprExpr (EAp e1 e2) prec = iConcat [pprExpr e1 6, iStr " ", pprExpr e2 6]
pprExpr (ELet isrec defns expr) prec =
  iConcat [ iStr keyword , iNewline
          , iStr "  ", iIndent (pprDefns defns), iNewline
          , iStr "in ", pprExpr expr prec
          ]
  where
    keyword
      | not isrec = "let"
      | isrec = "letrec"
pprExpr (ELam vs expr) prec = iConcat [ iStr "\\ "
                                      , iInterleave (iStr " ") (map iStr vs)
                                      ,  iStr "." ,  pprExpr expr prec
                                      ]
pprExpr (ECase expr alts) prec = iConcat [ iStr "case " , pprExpr expr prec
                                         , iStr " of", iNewline
                                         , iInterleave sep (map pprAlt alts)
                                         ]
  where
    sep = iConcat [ iStr ";", iNewline ]
    pprAlt (n, vs, expr) =
      iConcat [ iStr "<" , iStr (shownum n) , iStr "> "
              , iInterleave (iStr " ") (map iStr vs) , iStr " -> "
              , iIndent (pprExpr expr prec)
              ]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [ iStr name, iStr " = "
                               , iIndent (pprExpr expr 6)
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

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave iNewline (map pprSc prog)
  where
    pprSc (name, [], expr) = pprDefn (name, expr)
    pprSc (name, vs, expr) = iConcat [ iStr name, iStr " "
                                     , iInterleave (iStr " ") (map iStr vs)
                                     , iStr " = " , iIndent (pprExpr expr 6)
                                     ]

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
  where
    e2s = e2 : e2s

iNil :: Iseq
iNil = INil

-- Exercise 1.7
iStr :: [Char] -> Iseq
iStr "" = iNil
iStr (s:ss) = foldll f (IStr [s]) ss
  where
    f :: IseqRep -> Char -> IseqRep
    f x '\n' = x `iAppend` iNewline
    f x c = x `iAppend` IStr [c]

-- Exercise 1.5
iAppend :: Iseq -> Iseq -> Iseq
iAppend INil s2 = s2
iAppend s1 INil = s1
iAppend s1 s2 = IAppend s1 s2

iNewline :: Iseq
iNewline = INewline

iIndent :: Iseq -> Iseq
iIndent s = IIndent s

iDisplay :: Iseq -> [Char]
iDisplay s = flatten 0 [(s, 0)]

iNum :: (Num a, Show a) => a -> Iseq
iNum n = iStr (show n)

iFWNum :: (Num a, Show a) => Number -> a -> Iseq
iFWNum width n = iStr $ spaces (width - fromIntegral (length digits)) ++ digits
  where
    digits = show n
    spaces x = replicate (fromIntegral x) ' '

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map layItem (zip [1 ..] seqs))
  where
    layItem (n, s) = iConcat [ iFWNum 4 n, iStr ") ", iIndent s, iNewline ]

flatten :: Number -> [(IseqRep, Number)] -> [Char]
flatten col [] = ""
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((INewline, indent) : seqs) =
  "\n" ++ spaces indent ++ flatten indent seqs
  where
    spaces n = replicate (fromIntegral n) ' '
flatten col ((IIndent s, indent) : seqs) = flatten col ((s, col) : seqs)
flatten col ((IStr s, indent) : seqs) = s ++ flatten (col + l s) seqs
  where
    l = fromIntegral . length
flatten col ((IAppend seq1 seq2, indent) : seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)
