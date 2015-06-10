{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Linear.Grammar where

import Data.List (find)
import Data.String

-- * User-facing API

-- | User-facing abstract syntax tree
data LinAst =
    EVar String
  | ELit Double
  | ECoeff LinAst Double
  | EAdd LinAst LinAst
  deriving (Show, Eq)

-- | Doesn't solve the ridde, but it helps.
instance IsString LinAst where
  fromString = EVar

(.+.) :: LinAst -> LinAst -> LinAst
(.+.) = EAdd

infixr 8 .+.

class Coefficient x y where
  (.*.) :: x -> y -> LinAst

infixr 9 .*.

instance Coefficient LinAst Double where
  (.*.) = ECoeff

instance Coefficient Double LinAst where
  (.*.) = flip ECoeff

-- | Pushes @ECoeff@ down the tree, leaving @EAdd@ at the top level.
-- After using this funciton, all @ECoeff@ constructors @LinAst@ parameter will
-- be @EVar@.
multLin :: LinAst -> LinAst
multLin (EVar n) = EVar n
multLin (ELit x) = ELit x
multLin (ECoeff e x) = case multLin e of
  (ELit y)      -> ELit (y * x)
  (EVar n)      -> ECoeff (EVar n) x
  (ECoeff e' y) -> ECoeff e' (y * x)
  (EAdd e1 e2)  -> EAdd (multLin $ ECoeff e1 x) (multLin $ ECoeff e2 x)
multLin (EAdd e1 e2) = EAdd (multLin e1) (multLin e2)

-- * Linear Expressions

data LinVar = LinVar
  { varName  :: String
  , varCoeff :: Double
  } deriving (Show, Eq)

-- | For sorting tableaus
instance Ord LinVar where
  compare (LinVar x _) (LinVar y _) = compare x y

hasName :: LinVar -> LinVar -> Bool
hasName (LinVar n _) (LinVar m _) = n == m

-- | Linear expressions suited for normal and standard form.
data LinExpr = LinExpr
  { exprCoeffs :: [LinVar]
  , exprConst  :: Double
  } deriving (Show, Eq)

mergeLinExpr :: LinExpr -> LinExpr -> LinExpr
mergeLinExpr (LinExpr vs1 x) (LinExpr vs2 y) = LinExpr (vs1 ++ vs2) (x + y)

-- | Turns @LinAst@ to @LinExpr@ - should be done /after/ @multLin@.
addLin :: LinAst -> LinExpr
addLin = go (LinExpr [] 0)
  where
    go :: LinExpr -> LinAst -> LinExpr
    go (LinExpr vs c) (EVar n)            = LinExpr (LinVar n 1:vs) c
    go (LinExpr vs c) (ELit x)            = LinExpr vs (c + x)
    go (LinExpr vs c) (ECoeff (EVar n) x) = LinExpr (LinVar n x:vs) c
    go le (EAdd e1 e2) = mergeLinExpr (go le e1) (go le e2)

-- | Merged duplicate @LinVar@s in a @LinExpr@. Should be used /after/ @addLin@.
removeDupLin :: LinExpr -> LinExpr
removeDupLin (LinExpr vs c) = LinExpr (foldr go [] vs) c
  where
    go :: LinVar -> [LinVar] -> [LinVar]
    go x [] = [x]
    go (LinVar n x) xs = case find (hasName $ LinVar n x) xs of
      Just (LinVar m y) -> LinVar m (y + x):filter (not . hasName (LinVar n x)) xs
      Nothing           -> xs

makeLinExpr :: LinAst -> LinExpr
makeLinExpr = removeDupLin . addLin . multLin

-- * Linear Inequalities

data Ineq =
    Equ LinExpr LinExpr
  | Lte LinExpr LinExpr
  deriving (Show, Eq)

(.==.) :: LinAst -> LinAst -> Ineq
x .==. y = Equ (makeLinExpr x) (makeLinExpr y)

infixl 7 .==.

(.<=.) :: LinAst -> LinAst -> Ineq
x .<=. y = Lte (makeLinExpr x) (makeLinExpr y)

infixl 7 .<=.

(.=>.) :: LinAst -> LinAst -> Ineq
(.=>.) = flip (.<=.)

infixl 7 .=>.

-- * Standard Form

data IneqStdForm =
    EquStd [LinVar] Double
  | LteStd [LinVar] Double
  | GteStd [LinVar] Double
  deriving (Show, Eq)

-- TODO: Test idempotency
standardize :: Ineq -> Ineq
standardize (Equ (LinExpr xs xc) (LinExpr ys yc))
  | null xs = Equ (LinExpr [] (xc - yc)) (LinExpr ys 0)
  | null ys = Equ (LinExpr xs 0) (LinExpr [] (yc - xc))
  | otherwise = let ys' = map (\y -> y {varCoeff = varCoeff y * (-1)}) ys in
      Equ (removeDupLin $ LinExpr (ys' ++ xs) 0) (LinExpr [] (yc - xc))
standardize (Lte (LinExpr xs xc) (LinExpr ys yc))
  | null xs = Lte (LinExpr [] (xc - yc)) (LinExpr ys 0)
  | null ys = Lte (LinExpr xs 0) (LinExpr [] (yc - xc))
  | otherwise = let ys' = map (\y -> y {varCoeff = varCoeff y * (-1)}) ys in
      Lte (removeDupLin $ LinExpr (ys' ++ xs) 0) (LinExpr [] (yc - xc))

standardForm :: Ineq -> IneqStdForm
standardForm = go . standardize
  where
    go (Equ (LinExpr xs xc) (LinExpr ys yc)) | null xs && yc == 0 = EquStd ys xc
                                             | null ys && xc == 0 = EquStd xs yc
    go (Lte (LinExpr xs xc) (LinExpr ys yc)) | null xs && yc == 0 = LteStd ys xc
                                             | null ys && xc == 0 = GteStd xs yc
    go _ = error "Non-standard Ineq"