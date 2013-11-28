module Core where

import Core.Parser
import Core.Pretty
import Core.Types
import Core.Utils

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

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

-- Exercise 1.6
letDef :: CoreProgram
letDef = [ ("testDef", ["x", "y"], ELet True [ ("foo", EAp (EVar "addOne")
                                                           (EVar "x"))
                                             , ("bar", EAp (EVar "addOne")
                                                           (EVar "y"))
                                             ]
                                        (EAp (EVar "foo")
                                             (EAp (EAp (EVar "+") (EVar "bar"))
                                                  (EVar "foo"))))
         ]

-- Exercise 1.21 test
sampleProgram :: CoreProgram
sampleProgram = syntax . lexCore $
  unlines [ "f = 3;", "g x y = let z = x in z;"
          , "h x = case (let y = x in y) of"
          , "        <1> -> 2;"
          , "        <2> -> 5"
          ]
