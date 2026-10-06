module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), VName, subExp)
import APL.Eval
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
genVar =
  choose (2, 4) >>= f
  where
    f :: Int -> Gen String
    f 0 = pure []
    f i = (:) <$> choose ('a','z') <*> f (i - 1)

genExp :: [VName] -> Int -> Gen Exp
genExp _ 0 = frequency [(1, CstInt <$> arbitrary), (1, CstBool <$> arbitrary)]
genExp vs size =
  frequency
    [ (10, CstInt <$> arbitrary)
    , (10, CstBool <$> arbitrary)
    , (10, Add <$> genExp vs halfSize <*> genExp vs halfSize)
    , (10, Sub <$> genExp vs halfSize <*> genExp vs halfSize)
    , (10, Mul <$> genExp vs halfSize <*> genExp vs halfSize)
    , (10, Div <$> genExp vs halfSize <*> genExp vs halfSize)
    , (10, Pow <$> genExp vs halfSize <*> genExp vs halfSize)
    , (10, Eql <$> genExp vs halfSize <*> genExp vs halfSize)
    , (10, If <$> genExp vs thirdSize <*> genExp vs thirdSize <*> genExp vs thirdSize)
    , (if vs == [] then 0 else 8, Var <$> oneof (fmap pure vs))
    , (2, Var <$> genVar)
    ,
      (1,
      do
        v <- genVar
        let vs' = v : vs
        Let v <$> genExp vs halfSize <*> genExp vs' halfSize)
    , (10, Lambda <$> genVar <*> genExp vs (size - 1))
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
parsePrinted _ = undefined

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

-- The number of tests is part of the specification of this test suite: some of
-- these properties fail only rarely.  Do not reduce it.
properties :: [(String, Property)]
properties =
  [ ("expCoverage", property $ withMaxSuccess 10000 expCoverage)
  , ("parsePrinted", property $ withMaxSuccess 10000 parsePrinted)
  , ("onlyCheckedErrors", property $ withMaxSuccess 10000 onlyCheckedErrors)
  ]
