# lambda-calculus-dynamic-continuations

For an overview of the project, please refer to the writeup (`writeup/cs_421_4th_hour_project_writeup_erick2.pdf`)

## Code Organization

The source code is contained in `cs421-project-erick2` which is a `stack` project. `cs421-project-erick2.cabal`, `LICENSE`, `Setup.hs`, and `stack.yaml` are used to build the project and can be ignored. `src/Main.hs` provides the full implementation, `test/Spec.hs` is the test runner which prints the results of the tests defined in `test/Tests.hs`.

## Usage

In order to see the code in action:
1. `cd cs421-project-erick2`
2. `stack init`
3. `stack test`
4. `stack ghci`
5. Enter commands similar to those given in the "Demo Script" below

## Demo Script

`let printE = putStrLn . ppExpr`

### Simple Lambda Calculus
\y.((\x.x)y)
== \y.y
```
let e = (LambdaExp "y" (AppExp (LambdaExp "x" (VarExp "x")) (VarExp "y")))
printE e
evaluate e
```
\q.\y.((\x.(\z.zx)q)y)
== \q.\y.(qy)
```
let e = (LambdaExp "q" (LambdaExp "y" (AppExp (LambdaExp "x" (AppExp (LambdaExp "z" (AppExp (VarExp "z") (VarExp "x"))) (VarExp "q"))) (VarExp "y"))))
printE e
evaluate e
```
(\a.a)(\b.b)(\c.cc)(\d.d)
== \d.d
```
let e = (AppExp (AppExp (AppExp (LambdaExp "a" (VarExp "a")) (LambdaExp "b" (VarExp "b"))) ((LambdaExp "c" (AppExp (VarExp "c") (VarExp "c"))))) (LambdaExp "d" (VarExp "d")))
printE e
evaluate e
```

### Alpha Renaming
\y.((\x.(\y.yx))y)
== \y.(\y'.y'y)
```
let e = (LambdaExp "y" (AppExp (LambdaExp "x" (LambdaExp "y" (AppExp (VarExp "y") (VarExp "x")))) (VarExp "y")))
printE e
evaluate e
```

### Integer Functions
def square(x): x\*x; => (\x.x\*x); square 11
== 121
```
let e = (AppExp (LambdaExp "x" (IntOpExp Times (VarExp "x") (VarExp "x"))) (IntExp 11))
printE e
evaluate e
```

### Boolean Functions
(2 <= 3) 1 0
== 1
```
let e = (AppExp (AppExp (IntOpExp LessEq (IntExp 2) (IntExp 3)) (IntExp 1)) (IntExp 0))
printE e
evaluate e
```
(1 == 2) 1 0
== 0
```
let e = (AppExp (AppExp (IntOpExp Equal (IntExp 1) (IntExp 2)) (IntExp 1)) (IntExp 0))
printE e
evaluate e
```

### Prompt
(#123) / (#7)
== 17
```
let e = (IntOpExp Divide (PromptExp (IntExp 123)) (PromptExp (IntExp 7)))
printE e
evaluate e
```

### Control
(\x.0)((\y.1)(Fz.2))
== 2
```
let e = (AppExp (LambdaExp "x" (IntExp 0)) (AppExp (LambdaExp "y" (IntExp 1)) (ControlExp "z" (IntExp 2))))
printE e
evaluate e
```
(\x.0)#((\y.1)(Fz.2))
== 0
```
let e = (AppExp (LambdaExp "x" (IntExp 0)) (PromptExp (AppExp (LambdaExp "y" (IntExp 1)) (ControlExp "z" (IntExp 2)))))
printE e
evaluate e
```

### Complex Control and Prompt
(\a.(Fd.(\e.0)))(Fb.(\c.3 \* c)(b 7))
== \e.0 env:{"a":=7,
     "d":=(End, kTrail:[
        Fun(\c.3 \* c
          env:{b:=(
            Fun(\a.(Fd.(\e.0)) env:{}, End), kTrail:[])}, End)])}
```
let e = (AppExp (LambdaExp "a" (ControlExp "d" (LambdaExp "e" (IntExp 0)))) (ControlExp "b" (AppExp (LambdaExp "c" (IntOpExp Times (IntExp 3) (VarExp "c"))) (AppExp (VarExp "b") (IntExp 7)))))
printE e
evaluate e
```
(\a.#((\b.b\*2)(Fc.(c(c a)))))(Fd.(\e.e\*3)(d 5))
== 60
```
let e = (AppExp (LambdaExp "a" (PromptExp (AppExp (LambdaExp "b" (IntOpExp Times (VarExp "b") (IntExp 2))) (ControlExp "c" (AppExp (VarExp "c") (AppExp (VarExp "c") (VarExp "a"))))))) (ControlExp "d" (AppExp (LambdaExp "e" (IntOpExp Times (VarExp "e") (IntExp 3))) (AppExp (VarExp "d") (IntExp 5)))))
printE e
evaluate e
```

### factorial
def factorial(x):
  if x <= 1:
    return 1
  return x \* factorial(x - 1)
```
let factorial = AppExp (LambdaExp "f" (LambdaExp "x" (AppExp (AppExp (AppExp (IntOpExp LessEq (VarExp "x") (IntExp 1)) (LambdaExp "" (IntExp 1))) (LambdaExp "" (IntOpExp Times (VarExp "x") (AppExp (AppExp (VarExp "f") (VarExp "f")) (IntOpExp Minus (VarExp "x") (IntExp 1)))))) (IntExp 0)))) (LambdaExp "f" (LambdaExp "x" (AppExp (AppExp (AppExp (IntOpExp LessEq (VarExp "x") (IntExp 1)) (LambdaExp "" (IntExp 1))) (LambdaExp "" (IntOpExp Times (VarExp "x") (AppExp (AppExp (VarExp "f") (VarExp "f")) (IntOpExp Minus (VarExp "x") (IntExp 1)))))) (IntExp 0))))
printE factorial
evaluate $ AppExp factorial (IntExp 3)
evaluate $ AppExp factorial (IntExp 5)
```
```
let controlFactorial = LambdaExp "y" (PromptExp (AppExp (LambdaExp "f" (LambdaExp "x" (AppExp (AppExp (AppExp (IntOpExp LessEq (VarExp "x") (IntExp 1)) (LambdaExp "" (IntExp 1))) (LambdaExp "" (IntOpExp Times (VarExp "x") (AppExp (AppExp (VarExp "f") (VarExp "f")) (IntOpExp Minus (VarExp "x") (IntExp 1)))))) (IntExp 0)))) (ControlExp "k" (AppExp (AppExp (VarExp "k") (VarExp "k")) (VarExp "y")))))
printE factorial
printE controlFactorial
evaluate $ AppExp controlFactorial (IntExp 3)
evaluate $ AppExp controlFactorial (IntExp 5)
```