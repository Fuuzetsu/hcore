module Core.Types where

-- We don't have Miranda's abstype so we use a regular
-- data type and provide some functions on it
type Number = Double

-- We cheat and just use integers in places where we would anyway
-- instead of trying to emulate Miranda's ‘num’ with Doubles.
type NumberI = Integer

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

-- Exercise 1.11
type Token = (NumberI, [Char])
