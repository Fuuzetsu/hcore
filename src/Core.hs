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
pprint = undefined

pprExpr :: CoreExpr -> [Char]
pprExpr (ENum n) = shownum n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2

pprAExpr :: CoreExpr -> [Char]
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = "(" ++ pprExpr e ++ ")"

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
  where
    e2s = e2 : e2s
