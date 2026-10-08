module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), VName, subExp, printExp)
import APL.Eval
import APL.Parser
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , withMaxSuccess
  , frequency
  , choose
  )

instance Arbitrary Exp where
  arbitrary = sized $ genExp []

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genVar :: Gen VName
genVar = do
  len <- choose (2, 4)
  v   <- f len
  if v `elem` keywords then
    genVar
  else pure v
  where
    f :: Int -> Gen String
    f 0 = pure []
    f i = (:) <$> choose ('a','z') <*> f (i - 1)

genExp :: [VName] -> Int -> Gen Exp
genExp _ 0 = frequency [(1, CstInt <$> arbitrary), (1, CstBool <$> arbitrary),
                        (1, pure $ CstInt 0)]
genExp vs size =
  frequency
    [ (10, CstInt <$> arbitrary)
    , (10, CstBool <$> arbitrary)
    , (5, Add <$> genExp vs halfSize <*> genExp vs halfSize)
    , (5, Sub <$> genExp vs halfSize <*> genExp vs halfSize)
    , (5, Mul <$> genExp vs halfSize <*> genExp vs halfSize)
    , (5, Div <$> genExp vs halfSize <*> genExp vs halfSize)
    , (5, Pow <$> genExp vs halfSize <*> genExp vs halfSize)
    , (5, Eql <$> genExp vs halfSize <*> genExp vs halfSize)
    , (5, If <$> genExp vs thirdSize <*> genExp vs thirdSize <*> genExp vs thirdSize)
    , (if vs == [] then 0 else 20, Var <$> oneof (fmap pure vs))
    , (1, Var <$> genVar)
    ,
      (20,
      do
        v <- genVar
        let vs' = v : vs
        Let v <$> genExp vs halfSize <*> genExp vs' halfSize)
    , (20,
      do
        v <- genVar
        let vs' = v : vs
        Lambda <$> pure v <*> genExp vs' (size - 1))
    , (10, Apply <$> genExp vs halfSize <*> genExp vs halfSize)
    , (10, TryCatch <$> genExp vs halfSize <*> genExp vs halfSize)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e =
  let e' = f e in
  case parseAPL "" (printExp e') of
    Left _ -> False
    Right e'' ->
      e'' == e'
  where
    f (CstInt n) = CstInt $ abs n
    f (CstBool b) = CstBool b
    f (Add e1 e2) = Add (f e1) (f e2)
    f (Sub e1 e2) = Sub (f e1) (f e2)
    f (Mul e1 e2) = Mul (f e1) (f e2)
    f (Div e1 e2) = Div (f e1) (f e2)
    f (Pow e1 e2) = Pow (f e1) (f e2)
    f (Eql e1 e2) = Eql (f e1) (f e2)
    f (If e1 e2 e3) = If (f e1) (f e2) (f e3)
    f (Var vname) = Var vname
    f (Let vname e1 e2) = Let vname (f e1) (f e2)
    f (Lambda vname e1) = Lambda vname (f e1)
    f (Apply e1 e2) = Apply (f e1) (f e2)
    f (TryCatch e1 e2) = TryCatch (f e1) (f e2)

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e =
  let errors = checkExp e in
  case runEval $ eval e of
    Right _ -> True
    Left err ->
      err `elem` errors

-- The number of tests is part of the specification of this test suite: some of
-- these properties fail only rarely.  Do not reduce it.
properties :: [(String, Property)]
properties =
  [ ("expCoverage", property $ withMaxSuccess 10000 expCoverage)
  , ("parsePrinted", property $ withMaxSuccess 10000 parsePrinted)
  , ("onlyCheckedErrors", property $ withMaxSuccess 10000 onlyCheckedErrors)
  ]
