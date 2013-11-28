module Core.Types where

type Number = Integer

data Expr a = EVar Name
            | ENum Number
            | EConstr Number Number
            | EAp (Expr a) (Expr a)
            | ELet IsRec [(a, Expr a)] (Expr a)
            | ECase (Expr a) [Alter a]
            | ELam [a] (Expr a)
              deriving Show

type CoreExpr = Expr Name
type Name = String
type IsRec = Bool


type Alter a = (Number, [a], Expr a)
type CoreAlt = Alter Name

type Program a = [ScDefn a]
type CoreProgram = Program String

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn String

type Iseq = IseqRep

data IseqRep = INil
             | IStr [Char]
             | IAppend IseqRep IseqRep
             | IIndent IseqRep
             | INewline
               deriving Show

type Token = [Char]
