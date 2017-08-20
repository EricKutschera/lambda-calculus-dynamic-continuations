module Tests where

import Main (
              Expr(..)
            , IntOp(..)
            , Val(..)
            , Env(..)
            , Continuation(..)
            , ContTrail(..)
            , evaluate
            , freeVariablesExp
            , emptyEnv
            , emptyKTrail
            , ppExpr
            )

import Data.Set (Set)
import qualified Data.Set as Set

allTests :: [(String, [Bool])]
allTests = [
             ("evalBasic", evalBasic)
           , ("lambdaApplication", lambdaApplication)
           , ("freeVars", freeVars)
           , ("alphaRenaming", alphaRenaming)
           , ("integerArithmetic", integerArithmetic)
           , ("integerFunctions", integerFunctions)
           , ("booleanFunctions", booleanFunctions)
           , ("factorialCalls", factorialCalls)
           , ("basicPrompt", basicPrompt)
           , ("basicControl", basicControl)
           , ("basicControlAndPrompt", basicControlAndPrompt)
           , ("controlAndPrompt", controlAndPrompt)
           ]

evalBasic :: [Bool]
evalBasic =
  [
    -- x
    -- == Exception var lookup failed: x
    (evaluate (VarExp ("x")))
     == (ExnVal "var lookup failed: x")
    -- \x.x
    -- == \x.x
  , (evaluate (LambdaExp "x" (VarExp "x")))
     == (Closure "x" (VarExp "x") emptyEnv)
    -- Exception s
    -- == Exception s
  , (evaluate (ExnExp "exception"))
     == (ExnVal "exception")
  ]

-- From lambda-calculus.pdf from lecture 2017-06-15
lambdaApplication :: [Bool]
lambdaApplication =
  [
    -- \y.((\x.x)y)
    -- == \y.y
    (evaluate (LambdaExp "y" (AppExp (LambdaExp "x" (VarExp "x")) (VarExp "y"))))
     == (Closure "y" (VarExp "y") emptyEnv)
    -- \x.\y.((\z.x)y)
    -- ==  \x.\y.x
  , (evaluate (LambdaExp "x" (LambdaExp "y" (AppExp (LambdaExp "z" (VarExp "x")) (VarExp "y")))))
     == (Closure "x" (LambdaExp "y" (VarExp "x")) emptyEnv)
    -- \a.\b.\y.((\z.azbz)y)
    -- == \a.\b.\y.(ayby)
  , (evaluate (LambdaExp "a" (LambdaExp "b" (LambdaExp "y" (AppExp (LambdaExp "z" (AppExp (AppExp (AppExp (VarExp "a") (VarExp "z")) (VarExp "b")) (VarExp "z"))) (VarExp "y"))))))
     == (Closure "a" (LambdaExp "b" (LambdaExp "y" (AppExp (AppExp (AppExp (VarExp "a") (VarExp "y")) (VarExp "b")) (VarExp "y")))) emptyEnv)
    -- \y.((\x.(\z.z)x)y)
    -- == \y.y
  , (evaluate (LambdaExp "y" (AppExp (LambdaExp "x" (AppExp (LambdaExp "z" (VarExp "z")) (VarExp "x"))) (VarExp "y"))))
     == (Closure "y" (VarExp "y") emptyEnv)
    -- \a.\b.\y.((\x.x(\z.ax)(\x.bx))y)
    -- == \a.\b.\y.(y(\z.ay)(\x.bx))
  , (evaluate (LambdaExp "a" (LambdaExp "b" (LambdaExp "y" (AppExp (LambdaExp "x" (AppExp (AppExp (VarExp "x") (LambdaExp "z" (AppExp (VarExp "a") (VarExp "x")))) (LambdaExp "x" (AppExp (VarExp "b") (VarExp "x"))))) (VarExp "y"))))))
     == (Closure "a" (LambdaExp "b" (LambdaExp "y" (AppExp (AppExp (VarExp "y") (LambdaExp "z" (AppExp (VarExp "a") (VarExp "y")))) (LambdaExp "x" (AppExp (VarExp "b") (VarExp "x")))))) emptyEnv)
    -- \b.\y.((\x.(\z.zx)(\x.bx))y)
    -- == \b.\y.(by)
  , (evaluate (LambdaExp "b" (LambdaExp "y" (AppExp (LambdaExp "x" (AppExp (LambdaExp "z" (AppExp (VarExp "z") (VarExp "x"))) (LambdaExp "x" (AppExp (VarExp "b") (VarExp "x"))))) (VarExp "y")))))
     == (Closure "b" (LambdaExp "y" (AppExp (VarExp "b") (VarExp "y"))) emptyEnv)
    -- \y.((\x.xx)y)
    -- == \y.(yy)
  , (evaluate (LambdaExp "y" (AppExp (LambdaExp "x" (AppExp (VarExp "x") (VarExp "x"))) (VarExp "y"))))
     == (Closure "y" (AppExp (VarExp "y") (VarExp ("y"))) emptyEnv)
    -- \a.\y.((\x.axxa)y)
    -- == \a.\y.(ayya)
  , (evaluate (LambdaExp "a" (LambdaExp "y" (AppExp (LambdaExp "x" (AppExp (AppExp (AppExp (VarExp "a") (VarExp "x")) (VarExp "x")) (VarExp "a"))) (VarExp "y")))))
     == (Closure "a" (LambdaExp "y" (AppExp (AppExp (AppExp (VarExp "a") (VarExp "y")) (VarExp "y")) (VarExp "a"))) emptyEnv)
    -- \q.\y.((\x.(\z.zx)q)y)
    -- == \q.\y.(qy)
  , (evaluate (LambdaExp "q" (LambdaExp "y" (AppExp (LambdaExp "x" (AppExp (LambdaExp "z" (AppExp (VarExp "z") (VarExp "x"))) (VarExp "q"))) (VarExp "y")))))
     == (Closure "q" (LambdaExp "y" (AppExp (VarExp "q") (VarExp "y"))) emptyEnv)
    -- \b.\y.((\x.x((\z.zx)(\x.bx)))y)
    -- == \b.\y.(y(by))
  , (evaluate (LambdaExp "b" (LambdaExp "y" (AppExp (LambdaExp "x" (AppExp (VarExp "x") (AppExp (LambdaExp "z" (AppExp (VarExp "z") (VarExp "x"))) (LambdaExp "x" (AppExp (VarExp "b") (VarExp "x")))))) (VarExp "y")))))
     == (Closure "b" (LambdaExp "y" (AppExp (VarExp "y") (AppExp (VarExp "b") (VarExp "y")))) emptyEnv)
    -- (\a.a)(\b.b)(\c.cc)(\d.d)
    -- == \d.d
  , (evaluate (AppExp (AppExp (AppExp (LambdaExp "a" (VarExp "a")) (LambdaExp "b" (VarExp "b"))) ((LambdaExp "c" (AppExp (VarExp "c") (VarExp "c"))))) (LambdaExp "d" (VarExp "d"))))
     == (Closure "d" (VarExp "d") emptyEnv)
  ]

freeVars :: [Bool]
freeVars =
  [
    -- (\x.(\y.yx))y
    -- == {y}
    (freeVariablesExp (AppExp (LambdaExp "x" (LambdaExp "y" (AppExp (VarExp "y") (VarExp "x")))) (VarExp "y")))
     == Set.singleton "y"
    -- (\f.(\x.fx))(\y.(\x.y))
    -- == {}
  , (freeVariablesExp (AppExp (LambdaExp "f" (LambdaExp "x" (AppExp (VarExp "f") (VarExp "x")))) (LambdaExp "y" (LambdaExp "x" (VarExp "y")))))
     == Set.empty
    -- (\a.b)(\b.a)
    -- {a, b}
  , (freeVariablesExp (AppExp (LambdaExp "a" (VarExp "b")) (LambdaExp "b" (VarExp "a"))))
     == (Set.fromList ["a", "b"])
  ]

-- From lambda-calculus.pdf from lecture 2017-06-15
alphaRenaming :: [Bool]
alphaRenaming =
  [
    -- \y.((\x.(\y.yx))y)
    -- == \y.(\y'.y'y)
    (evaluate (LambdaExp "y" (AppExp (LambdaExp "x" (LambdaExp "y" (AppExp (VarExp "y") (VarExp "x")))) (VarExp "y"))))
     == (Closure "y" (LambdaExp "y'" (AppExp (VarExp "y'") (VarExp "y"))) emptyEnv)
    -- (\x.(\y.\x.y)x)
    -- == (\x.(\x'.x))
  , (evaluate (LambdaExp "x" (AppExp (LambdaExp "y" (LambdaExp "x" (VarExp "y"))) (VarExp "x"))))
     == (Closure "x" (LambdaExp "x'" (VarExp "x")) emptyEnv)
    -- (\f.(\x.fx))(\y.(\x.y))
    -- == (\x.(\x'.x))
  , (evaluate (AppExp (LambdaExp "f" (LambdaExp "x" (AppExp (VarExp "f") (VarExp "x")))) (LambdaExp "y" (LambdaExp "x" (VarExp "y")))))
     == (Closure "x" (LambdaExp "x'" (VarExp "x")) emptyEnv)
  ]

integerArithmetic :: [Bool]
integerArithmetic =
  [
    -- 123
    -- == 123
    (evaluate (IntExp 123))
     == (IntVal 123)
    -- 3 + 2
    -- == 5
  , (evaluate (IntOpExp Plus (IntExp 3) (IntExp 2)))
     == (IntVal 5)
    -- ((5 * 4) / 3) - (2 + 1)
    -- == 3
  , (evaluate (IntOpExp Minus (IntOpExp Divide (IntOpExp Times (IntExp 5) (IntExp 4)) (IntExp 3)) (IntOpExp Plus (IntExp 2) (IntExp 1))))
     == (IntVal 3)
    -- \x.(x + 2)
    -- == \x.(x + 2)
  , (evaluate (LambdaExp "x" (IntOpExp Plus (VarExp "x") (IntExp 2))))
     == (Closure "x" (IntOpExp Plus (VarExp "x") (IntExp 2)) emptyEnv)
    -- \a.\b.(((5 * a) / b) - (2 + 1))
    -- == \a.\b.(((5 * a) / b) - 3)
  , (evaluate (LambdaExp "a" (LambdaExp "b" (IntOpExp Minus (IntOpExp Divide (IntOpExp Times (IntExp 5) (VarExp "a")) (VarExp "b")) (IntOpExp Plus (IntExp 2) (IntExp 1))))))
     == (Closure "a" (LambdaExp "b" (IntOpExp Minus (IntOpExp Divide (IntOpExp Times (IntExp 5) (VarExp "a")) (VarExp "b")) (IntExp 3))) emptyEnv)
  ]

integerFunctions :: [Bool]
integerFunctions =
  [
    -- def inc(x): x + 1; => (\x.x+1); inc(inc 0)
    -- == 2
    (evaluate (AppExp (LambdaExp "x" (IntOpExp Plus (VarExp "x") (IntExp 1))) (AppExp (LambdaExp "x" (IntOpExp Plus (VarExp "x") (IntExp 1))) (IntExp 0))))
     == (IntVal 2)
    -- def square(x): x*x; => (\x.x*x); square 11
    -- == 121
  , (evaluate (AppExp (LambdaExp "x" (IntOpExp Times (VarExp "x") (VarExp "x"))) (IntExp 11)))
      == (IntVal 121)
  ]

booleanFunctions :: [Bool]
booleanFunctions =
  [
    -- (1 == 1) 1 0
    -- == 1
    (evaluate (AppExp (AppExp (IntOpExp Equal (IntExp 1) (IntExp 1)) (IntExp 1)) (IntExp 0)))
     == (IntVal 1)
    -- (1 == 2) 1 0
    -- == 0
  , (evaluate (AppExp (AppExp (IntOpExp Equal (IntExp 1) (IntExp 2)) (IntExp 1)) (IntExp 0)))
     == (IntVal 0)
    -- (2 <= 3) 1 0
    -- == 1
  , (evaluate (AppExp (AppExp (IntOpExp LessEq (IntExp 2) (IntExp 3)) (IntExp 1)) (IntExp 0)))
     == (IntVal 1)
    -- (3 <= 3) 1 0
    -- == 1
  , (evaluate (AppExp (AppExp (IntOpExp LessEq (IntExp 3) (IntExp 3)) (IntExp 1)) (IntExp 0)))
     == (IntVal 1)
    -- (4 <= 3) 1 0
    -- == 0
  , (evaluate (AppExp (AppExp (IntOpExp LessEq (IntExp 4) (IntExp 3)) (IntExp 1)) (IntExp 0)))
     == (IntVal 0)
  ]

-- Factorial is intended to be a comprehensive test of the lambda calculus
-- with integers. It includes recursion and conditionals
factorialCalls :: [Bool]
factorialCalls = map (\(x, r) -> (evaluate (AppExp factorial x)) == r)
  [
    (IntExp 0, IntVal 1)        -- fact(0) = 1
  , (IntExp 1, IntVal 1)        -- fact(1) = 1
  , (IntExp 2, IntVal 2)        -- fact(2) = 2
  , (IntExp 3, IntVal 6)        -- fact(3) = 6
  , (IntExp 4, IntVal 24)       -- fact(4) = 24
  , (IntExp 5, IntVal 120)      -- fact(5) = 120
  , (IntExp 10, IntVal 3628800) -- fact(10) = 3628800
  ]
factorial :: Expr
{-
def factorial(x):
  if x <= 1:
    return 1
  return x * factorial(x - 1)
-}
factorial = AppExp factorialCore factorialCore

factorialCore :: Expr
factorialCore =
  LambdaExp "f"
  (LambdaExp "x" (lazyIfLam (IntOpExp LessEq (VarExp "x") (IntExp 1))
                  (IntExp 1)
                  (IntOpExp Times (VarExp "x")
                   (AppExp
                    (AppExp (VarExp "f") (VarExp "f"))
                    (IntOpExp Minus (VarExp "x") (IntExp 1))))))
  where lazyIfLam cond tBranch fBranch =
          let closureT = LambdaExp "" tBranch
              closureF = LambdaExp "" fBranch
              chosenBranch = AppExp (AppExp cond closureT) (closureF)
          in AppExp chosenBranch (IntExp 0)

basicPrompt :: [Bool]
basicPrompt =
  [
    -- #0
    -- == 0
    (evaluate (PromptExp (IntExp 0)))
     == (IntVal 0)
    -- ##7
    -- == 7
  , (evaluate (PromptExp (PromptExp (IntExp 7))))
     == (IntVal 7)
    -- (#123) / (#7)
    -- == 17
  , (evaluate (IntOpExp Divide (PromptExp (IntExp 123)) (PromptExp (IntExp 7))))
     == (IntVal 17)
    -- #\x.1
    -- == \x.1
  , (evaluate (PromptExp (LambdaExp "x" (IntExp 1))))
     == (Closure "x" (IntExp 1) emptyEnv)
    -- #((\x.#x * 4)8)
    -- == 32
  , (evaluate (PromptExp (AppExp (LambdaExp "x" (IntOpExp Times (PromptExp (VarExp "x")) (IntExp 4))) (IntExp 8))))
     == (IntVal 32)
  ]

basicControl :: [Bool]
basicControl =
  [
    -- Fx.1
    -- == 1
    (evaluate (ControlExp "x" (IntExp 1)))
     == (IntVal 1)
    -- (\x.0)(Fx.1)
    -- == 1
  , (evaluate (AppExp (LambdaExp "x" (IntExp 0)) (ControlExp "x" (IntExp 1))))
     == (IntVal 1)
    -- (Fx.1)(\x.0)
    -- == 1
  , (evaluate (AppExp (ControlExp "x" (IntExp 1)) (LambdaExp "x" (IntExp 0))))
     == (IntVal 1)
    -- (\x.0)(Fx.(x)1)
    -- == 0
  , (evaluate (AppExp (LambdaExp "x" (IntExp 0)) (ControlExp "x" (AppExp (VarExp "x") (IntExp 1)))))
     == (IntVal 0)
    -- (Fx.x(\y.y))(\x.0)
    -- == \x.0
  , (evaluate (AppExp (ControlExp "x" (AppExp (VarExp "x") (LambdaExp "y" (VarExp "y")))) (LambdaExp "x" (IntExp 0))))
     == (Closure "x" (IntExp 0) emptyEnv)
  ]

basicControlAndPrompt :: [Bool]
basicControlAndPrompt =
  [
    -- (\x.0)((\y.1)(Fz.2))
    -- == 2
    (evaluate (AppExp (LambdaExp "x" (IntExp 0)) (AppExp (LambdaExp "y" (IntExp 1)) (ControlExp "z" (IntExp 2)))))
     == (IntVal 2)
    -- (\x.0)#((\y.1)(Fz.2))
    -- == 0
  , (evaluate (AppExp (LambdaExp "x" (IntExp 0)) (PromptExp (AppExp (LambdaExp "y" (IntExp 1)) (ControlExp "z" (IntExp 2))))))
     == (IntVal 0)
    -- ((Fy.(\z.1))(\x.0))(2)
    -- == \z.1 env:{"y":=(Arg(\x.0, env:{}, Arg(2, env:{}, End)), kTrail:[])}
  , (evaluate (AppExp (AppExp (ControlExp "y" (LambdaExp "z" (IntExp 1))) (LambdaExp "x" (IntExp 0))) (IntExp 2)))
     == (Closure "z" (IntExp 1) (Env [("y", CapturedCont (Arg (LambdaExp "x" (IntExp 0)) emptyEnv (Arg (IntExp 2) emptyEnv End)) emptyKTrail)]))
    -- (#((Fy.(\z.1))(\x.0)))(2)
    -- == 1
  , (evaluate (AppExp (PromptExp (AppExp (ControlExp "y" (LambdaExp "z" (IntExp 1))) (LambdaExp "x" (IntExp 0)))) (IntExp 2)))
     == (IntVal 1)
  ]

-- (\y.#((\f.\x.(factorial body))(Fk.((k)k)y)))
controlFactorial :: Expr
controlFactorial = LambdaExp "y" (PromptExp (AppExp factorialCore (ControlExp "k" (AppExp (AppExp (VarExp "k") (VarExp "k")) (VarExp "y")))))

controlAndPrompt :: [Bool]
controlAndPrompt =
  [
    -- (\a.a + 1)(Fb.(\c.3 * c)(b 7))
    -- == 24
    (evaluate (AppExp (LambdaExp "a" (IntOpExp Plus (VarExp "a") (IntExp 1))) (ControlExp "b" (AppExp (LambdaExp "c" (IntOpExp Times (IntExp 3) (VarExp "c"))) (AppExp (VarExp "b") (IntExp 7))))))
     == (IntVal 24)
    -- (\a.(Fd.(\e.0)))(Fb.(\c.3 * c)(b 7))
    -- == \e.0 env:{"a":=7, "d":=(End, kTrail:[Fun(\c.3 * c env:{b:=(Fun(\a.(Fd.(\e.0)) env:{}, End), kTrail:[])}, End)])}
  , (evaluate (AppExp (LambdaExp "a" (ControlExp "d" (LambdaExp "e" (IntExp 0)))) (ControlExp "b" (AppExp (LambdaExp "c" (IntOpExp Times (IntExp 3) (VarExp "c"))) (AppExp (VarExp "b") (IntExp 7))))))
     == (Closure "e" (IntExp 0) (Env [("d", CapturedCont End (ContTrail [Fun (Closure "c" (IntOpExp Times (IntExp 3) (VarExp "c")) (Env [("b",CapturedCont (Fun (Closure "a" (ControlExp "d" (LambdaExp "e" (IntExp 0))) (Env [])) End) (ContTrail []))])) End])), ("a",IntVal 7)]))
    -- (\a.#((\b.b*2)(Fc.(c(c a)))))(Fd.(\e.e*3)(d 5))
    -- 60
  , (evaluate (AppExp (LambdaExp "a" (PromptExp (AppExp (LambdaExp "b" (IntOpExp Times (VarExp "b") (IntExp 2))) (ControlExp "c" (AppExp (VarExp "c") (AppExp (VarExp "c") (VarExp "a"))))))) (ControlExp "d" (AppExp (LambdaExp "e" (IntOpExp Times (VarExp "e") (IntExp 3))) (AppExp (VarExp "d") (IntExp 5))))))
     == (IntVal 60)
    -- (\a.(\b.b*2)(Fc.(c(c a))))(Fd.(\e.e*3)(d 5))
    -- 180
  , (evaluate (AppExp (LambdaExp "a" (AppExp (LambdaExp "b" (IntOpExp Times (VarExp "b") (IntExp 2))) (ControlExp "c" (AppExp (VarExp "c") (AppExp (VarExp "c") (VarExp "a")))))) (ControlExp "d" (AppExp (LambdaExp "e" (IntOpExp Times (VarExp "e") (IntExp 3))) (AppExp (VarExp "d") (IntExp 5))))))
     == (IntVal 180)
    -- fact(5)
    -- 120
  , (evaluate (AppExp controlFactorial (IntExp 5)))
     == (IntVal 120)
    -- fact(fact(3))
    -- 720
  , (evaluate (AppExp controlFactorial (AppExp controlFactorial (IntExp 3))))
     == (IntVal 720)
  ]

-- The following expressions for the factorial function are intended
-- to illustrate the ability of control and prompt to write more
-- concise functions.
factorialString :: String
factorialString = ppExpr factorial
-- ((\f.(\x.((((x <= 1))((\.1)))((\.(x * ((f)(f))((x - 1))))))(0))))((\f.(\x.((((x <= 1))((\.1)))((\.(x * ((f)(f))((x - 1))))))(0))))

controlFactorialString :: String
controlFactorialString = ppExpr controlFactorial
-- (\y.((\f.(\x.((((x <= 1))((\.1)))((\.(x * ((f)(f))((x - 1))))))(0))))(Fk.((k)(k))(y)))
