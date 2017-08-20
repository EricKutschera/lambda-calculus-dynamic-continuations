module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Printf (printf)

data Expr = VarExp String
          | LambdaExp String Expr
          | AppExp Expr Expr
          | IntExp Int
          | IntOpExp IntOp Expr Expr
          | PromptExp Expr
          | ControlExp String Expr
          | ExnExp String
  deriving (Show, Eq)

data IntOp = Plus | Minus | Times | Divide
           | Less | Greater | LessEq | GreaterEq | Equal | NotEq
  deriving Eq

instance Show IntOp where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show Less = "<"
  show Greater = ">"
  show LessEq = "<="
  show GreaterEq = ">="
  show Equal = "=="
  show NotEq = "/="

ppExpr :: Expr -> String
ppExpr (VarExp s) = s
ppExpr (LambdaExp p b) = printf "(\\%s.%s)" p $ ppExpr b
ppExpr (AppExp a b) = printf "(%s)(%s)" (ppExpr a) (ppExpr b)
ppExpr (IntExp i) = show i
ppExpr (IntOpExp op lExp rExp) =
  printf "(%s %s %s)" (ppExpr lExp) (show op) (ppExpr rExp)
ppExpr (PromptExp e) = printf "#(%s)" (ppExpr e)
ppExpr (ControlExp p b) = printf "F%s.%s" p (ppExpr b)
ppExpr exn@(ExnExp _) = show exn

data Val = CapturedCont Continuation ContTrail
         | Closure String Expr Env
         | IntVal Int
         | ExnVal String
  deriving (Show, Eq)

-- An Environment maps variable names to values
newtype Env = Env [(String, Val)]
  deriving (Show, Eq)

emptyEnv :: Env
emptyEnv = Env []

data Continuation = End
                  | Arg Expr Env Continuation
                  | Fun Val Continuation
  deriving (Show, Eq)

-- ContTrail holds the continuations which have been queued as a
-- result of the application of a captured continuation. Control
-- allows continuations to be used as variables and applied before the
-- "current" continuation. This can happen many times so the
-- "then-current" continuations are stored in a list.
newtype ContTrail = ContTrail [Continuation]
  deriving (Show, Eq)

emptyKTrail :: ContTrail
emptyKTrail = ContTrail []

-- MetaConts holds (Continuation, ContTrail) pairs which have been
-- delimited by Prompt expressions. Prompt moves the current
-- Continuation and ContTrail into the MetaConts. This can happen many
-- times resulting in a list of "contexts"
newtype MetaConts = MetaConts [(Continuation, ContTrail)]
  deriving (Show, Eq)

emptyMetaKs :: MetaConts
emptyMetaKs = MetaConts []

-- Perform binary operations over ints
performBinIntOp :: IntOp -> Int -> Int -> Expr
performBinIntOp Plus = performArithOp (+)
performBinIntOp Minus = performArithOp (-)
performBinIntOp Times = performArithOp (*)
performBinIntOp Divide = performArithOp div
performBinIntOp Less = performCompOp (<)
performBinIntOp Greater = performCompOp (>)
performBinIntOp LessEq = performCompOp (<=)
performBinIntOp GreaterEq = performCompOp (>=)
performBinIntOp Equal = performCompOp (==)
performBinIntOp NotEq = performCompOp (/=)

performArithOp :: (Int -> Int -> Int) -> Int -> Int -> Expr
performArithOp f a b = IntExp $ f a b

-- Conditionals are implemented using the Church encoding for booleans
performCompOp :: (Int -> Int -> Bool) -> Int -> Int -> Expr
performCompOp f a b
  | f a b = churchTrue
  | otherwise = churchFalse
  where churchTrue = LambdaExp "x" (LambdaExp "y" (VarExp "x"))
        churchFalse = LambdaExp "x" (LambdaExp "y" (VarExp "y"))

-- Determine the set of free variables in an Expr
freeVariablesExp :: Expr -> Set String
freeVariablesExp (VarExp s) = Set.singleton s
freeVariablesExp (LambdaExp p b) = Set.delete p $ freeVariablesExp b
freeVariablesExp (AppExp e1 e2) =
  Set.unions [freeVariablesExp e1, freeVariablesExp e2]
freeVariablesExp (IntExp _) = Set.empty
freeVariablesExp (IntOpExp _ lExp rExp) =
  Set.unions [freeVariablesExp lExp, freeVariablesExp rExp]
freeVariablesExp (PromptExp e) = freeVariablesExp e
freeVariablesExp (ControlExp p b) = Set.delete p $ freeVariablesExp b
freeVariablesExp (ExnExp _) = Set.empty

-- Add prime suffixes to a variable name until it does not occur in the
-- provided set of variable names
makeUnique :: String -> Set String -> String
makeUnique initial others
  | initial `Set.member` others = makeUnique (initial ++ "'") others
  | otherwise = initial

-- If the expression being substituted contains a free variable that
-- gets a new binding from a lambda then change that lambda binding to
-- a fresh variable that does not occur among the free variables in
-- the lambda body.
alphaRename :: String -> Expr -> Expr -> Expr
alphaRename name e1 e2 =
  case e2 of
    (LambdaExp p b) ->
      let (newP, newB) = maybeReplace p b
      in LambdaExp newP newB
    (ControlExp p b) ->
      let (newP, newB) = maybeReplace p b
      in ControlExp newP newB
    _ -> ExnExp $ printf
         "a LambdaExp or ControlExp is required: alphaRename(%s, %s, %s)"
         name (show e1) (show e2)
  where maybeReplace p b =
          let freeExp = freeVariablesExp e1
              freeBody = freeVariablesExp b
              fresh = makeUnique p $ Set.insert p freeBody
              withFresh = replaceVarWithExpr p (VarExp fresh) b
          in if p `Set.member` freeExp
             then (fresh, replaceVarWithExpr name e1 withFresh)
             else (p, replaceVarWithExpr name e1 b)

-- Substitute an expression for all occurrences of the variable
-- name. This represents replacing the parameter name of a lambda
-- abstraction with the supplied argument.
replaceVarWithExpr :: String -> Expr -> Expr -> Expr
replaceVarWithExpr name expr ve@(VarExp v)
  | name == v = expr
  | otherwise = ve
replaceVarWithExpr name expr le@(LambdaExp p _)
  | name == p = le
  | otherwise = alphaRename name expr le
replaceVarWithExpr name expr (AppExp e1 e2) = AppExp r1 r2
  where r1 = replaceVarWithExpr name expr e1
        r2 = replaceVarWithExpr name expr e2
replaceVarWithExpr _ _ ie@(IntExp _) = ie
replaceVarWithExpr name expr (IntOpExp op lExp rExp) = IntOpExp op lR rR
  where lR = replaceVarWithExpr name expr lExp
        rR = replaceVarWithExpr name expr rExp
replaceVarWithExpr name expr (PromptExp e) =
  PromptExp $ replaceVarWithExpr name expr e
replaceVarWithExpr name expr ce@(ControlExp p _)
  | name == p = ce
  | otherwise = alphaRename name expr ce
replaceVarWithExpr _ _ exn@(ExnExp _) = exn

-- Evaluate an expression in a fresh evaluator
evaluate :: Expr -> Val
evaluate e = evalSimplified e emptyEnv End emptyKTrail emptyMetaKs

-- Simplify the expression before evaluating
evalSimplified :: Expr -> Env -> Continuation -> ContTrail -> MetaConts -> Val
evalSimplified = eval . simplify

-- Attempt to simplify an expression by evaluating sub-expressions.
-- This is helpful since the regular evaluator will not evaluate the
-- bodies of un-applied lambdas and those bodies can be reduced.
simplify :: Expr -> Expr
simplify e = if firstPass == secondPass then firstPass else keepGoing
  where firstPass = simplifyOnePass e
        secondPass = simplifyOnePass firstPass
        keepGoing = simplify secondPass

-- Go down the expression tree once, reducing if possible. Do not
-- simplify applications which have control or prompt since those
-- expressions depend on the structure of the expression.
simplifyOnePass :: Expr -> Expr
simplifyOnePass ve@(VarExp _) = ve
simplifyOnePass (LambdaExp param body) = LambdaExp param $ simplifyOnePass body
simplifyOnePass ae@(AppExp _ _)
  | hasControlOrPrompt ae = ae
  where hasControlOrPrompt (VarExp _) = False
        hasControlOrPrompt (LambdaExp _ e) = hasControlOrPrompt e
        hasControlOrPrompt (AppExp a b) = any hasControlOrPrompt [a, b]
        hasControlOrPrompt (IntExp _) = False
        hasControlOrPrompt (IntOpExp _ a b) = any hasControlOrPrompt [a, b]
        hasControlOrPrompt (PromptExp _) = True
        hasControlOrPrompt (ControlExp _ _) = True
        hasControlOrPrompt (ExnExp _) = False
simplifyOnePass (AppExp lExp rExp) =
  case simpL of
    LambdaExp p b -> replaceVarWithExpr p simpR b
    _ -> AppExp simpL simpR
  where simpL = simplifyOnePass lExp
        simpR = simplifyOnePass rExp
simplifyOnePass ie@(IntExp _) = ie
simplifyOnePass (IntOpExp op lExp rExp) =
  case (simpL, simpR) of
    (IntExp l, IntExp r) -> performBinIntOp op l r
    _ -> IntOpExp op simpL simpR
  where simpL = simplifyOnePass lExp
        simpR = simplifyOnePass rExp
simplifyOnePass (PromptExp e) = PromptExp $ simplifyOnePass e
simplifyOnePass (ControlExp param body) = ControlExp param $ simplifyOnePass body
simplifyOnePass ee@(ExnExp _) = ee

-- Evaluate the expression as far as possible given the state of the
-- evaluator. These functions are intended to mirror the operations of
-- the adjusted call-by-value abstract machine shown in Figure 2 of
-- the paper.
-- eval          maps to eval
-- applyCont     maps to cont1
-- applyTrail    maps to trail1
-- applyMetaCont maps to cont2
eval :: Expr -> Env -> Continuation -> ContTrail -> MetaConts -> Val
eval (VarExp v) (Env env) k kTrail metaKs = case lookup v env of
  Just x -> applyCont k x kTrail metaKs
  Nothing -> ExnVal $ "var lookup failed: " ++ v
eval (LambdaExp p b) env k kTrail metaKs =
  applyCont k (Closure p b env) kTrail metaKs
eval (AppExp e1 e2) env k kTrail metaKs =
  eval e1 env (Arg e2 env k) kTrail metaKs
eval (IntExp i) _ k kTrail metaKs = applyCont k (IntVal i) kTrail metaKs
eval (IntOpExp op lExp rExp) env k kTrail metaKs =
  case (lVal, rVal) of
    (IntVal lInt, IntVal rInt) ->
      eval (performBinIntOp op lInt rInt) env k kTrail metaKs
    _ -> ExnVal $ "IntOpExp with non IntVal operand: " ++ (show (op, lVal, rVal))
  where lVal = eval lExp env End emptyKTrail emptyMetaKs
        rVal = eval rExp env End emptyKTrail emptyMetaKs
eval (PromptExp e) env k kTrail (MetaConts metaKs) =
  eval e env End emptyKTrail (MetaConts ((k, kTrail) : metaKs))
eval (ControlExp p b) (Env env) k kTrail metaKs =
  eval b newEnv End emptyKTrail metaKs
  where newEnv = Env $ (p, CapturedCont k kTrail) : env
eval (ExnExp exn) _ _ _ _ = ExnVal exn

-- Apply the current continuation when evaluation  has reached a value
applyCont :: Continuation -> Val -> ContTrail -> MetaConts -> Val
applyCont End v kTrail metaKs = applyTrail kTrail v metaKs
applyCont (Arg argExp env k) v kTrail metaKs =
  eval argExp env (Fun v k) kTrail metaKs
applyCont (Fun (CapturedCont cK (ContTrail cT)) k) v (ContTrail t) metaKs =
  applyCont cK v newTrail metaKs
  where newTrail = ContTrail $ cT ++ (k : t)
applyCont (Fun (Closure paramName body (Env closureEnv)) k) v kTrail metaKs =
  let updatedEnv = Env $ (paramName, v) : closureEnv
  in eval body updatedEnv k kTrail metaKs
applyCont (Fun (IntVal i) _) _ _ _ =
  ExnVal $ "Cannot apply an Int: " ++ (show i) ++ " to an argument"
applyCont (Fun exn@(ExnVal _) _) _ _ _ = exn

-- Apply the correct continuation from the trail of continuations when
-- the current context is completed
applyTrail :: ContTrail -> Val -> MetaConts -> Val
applyTrail (ContTrail []) v metaKs = applyMetaCont metaKs v
applyTrail (ContTrail (c:cs)) v metaKs = applyCont c v (ContTrail cs) metaKs

-- Apply the correct meta-context when both the current continuation
-- is completed and the trail of then-current continuations is empty
applyMetaCont :: MetaConts -> Val -> Val
applyMetaCont (MetaConts []) v = v
applyMetaCont (MetaConts ((c, ct):mcs)) v = applyCont c v ct (MetaConts mcs)

main :: IO ()
main = putStrLn "Main not implemented"
