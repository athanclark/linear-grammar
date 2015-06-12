{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Linear.Grammar where

import Data.Number.Double.Extended

import Data.Char
import Data.List
import Data.String
import Control.Monad
import Control.Applicative

import Test.QuickCheck


-- * User-facing API

-- | User-facing abstract syntax tree
data LinAst =
    EVar String
  | ELit Double
  | ECoeff LinAst Double
  | EAdd LinAst LinAst
  deriving (Show, Eq)

instance Arbitrary LinAst where
  arbitrary = oneof
    [ EVar <$> (:[]) <$> choose ('A','z')
    , ELit <$> choose (-1000,1000)
    , liftM2 ECoeff arbitrary $ choose (-1000,1000)
    , liftM2 EAdd arbitrary arbitrary
    ]

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

-- | Boolean equality with @e^-6@ precision.
eqLinVar :: LinVar -> LinVar -> Bool
eqLinVar (LinVar n x) (LinVar m y) = n == m && eqDouble x y

eqLinVars :: [LinVar] -> [LinVar] -> Bool
eqLinVars xs ys = length xs == length ys && and (zipWith eqLinVar xs ys)

instance Arbitrary LinVar where
  arbitrary = liftM2 LinVar (arbitrary `suchThat` (\x -> length x < 5
                                                      && not (null x)
                                                      && all isAlpha x))
                            (choose (-1000,1000))

-- | For sorting tableaus
instance Ord LinVar where
  compare (LinVar x _) (LinVar y _) = compare x y

hasName :: String -> LinVar -> Bool
hasName n (LinVar m _) = n == m

mapName :: (String -> String) -> LinVar -> LinVar
mapName f (LinVar n x) = LinVar (f n) x

hasCoeff :: Double -> LinVar -> Bool
hasCoeff x (LinVar _ y) = x == y

mapCoeff :: (Double -> Double) -> LinVar -> LinVar
mapCoeff f (LinVar n x) = LinVar n $ f x

-- | Linear expressions suited for normal and standard form.
data LinExpr = LinExpr
  { exprVars :: [LinVar]
  , exprConst  :: Double
  } deriving (Show, Eq)

-- | Boolean equality with @e^-6@ precision.
eqLinExpr :: LinExpr -> LinExpr -> Bool
eqLinExpr (LinExpr xs x) (LinExpr ys y) = eqDouble x y && eqLinVars xs ys

instance Arbitrary LinExpr where
  arbitrary = liftM2 LinExpr (arbitrary `suchThat` isUniquelyNamed) $ choose (-1000,1000)
    where
      isUniquelyNamed x = let x' = map varName x in
        nub x' == x'

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
    go (LinVar n x) acc = case find (hasName n) acc of
      Just (LinVar m y) -> LinVar m (y + x):filter (not . hasName n) acc
      Nothing           -> LinVar n x:acc

makeLinExpr :: LinAst -> LinExpr
makeLinExpr = removeDupLin . addLin . multLin

-- * Linear Inequalities

data Ineq =
    Equ LinExpr LinExpr
  | Lte LinExpr LinExpr
  deriving (Show, Eq)

instance Arbitrary Ineq where
  arbitrary = oneof
    [ liftM2 Equ arbitrary arbitrary
    , liftM2 Lte arbitrary arbitrary
    ]

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

-- | Boolean equality with @e^-6@ precision.
eqIneqStdForm :: IneqStdForm -> IneqStdForm -> Bool
eqIneqStdForm (EquStd xs x) (EquStd ys y) = eqDouble x y && eqLinVars xs ys
eqIneqStdForm (LteStd xs x) (LteStd ys y) = eqDouble x y && eqLinVars xs ys
eqIneqStdForm (GteStd xs x) (GteStd ys y) = eqDouble x y && eqLinVars xs ys
eqIneqStdForm _ _ = False

instance Arbitrary IneqStdForm where
  arbitrary = oneof
    [ liftM2 EquStd (arbitrary `suchThat` isUniquelyNamed) $ choose (-1000,1000)
    , liftM2 LteStd (arbitrary `suchThat` isUniquelyNamed) $ choose (-1000,1000)
    , liftM2 GteStd (arbitrary `suchThat` isUniquelyNamed) $ choose (-1000,1000)
    ]
    where
      isUniquelyNamed x = let x' = map varName x in
        nub x' == x'

getStdVars :: IneqStdForm -> [LinVar]
getStdVars (EquStd xs _) = xs
getStdVars (LteStd xs _) = xs
getStdVars (GteStd xs _) = xs

mapStdVars :: ([LinVar] -> [LinVar]) -> IneqStdForm -> IneqStdForm
mapStdVars f (EquStd xs xc) = EquStd (f xs) xc
mapStdVars f (LteStd xs xc) = LteStd (f xs) xc
mapStdVars f (GteStd xs xc) = GteStd (f xs) xc

getStdConst :: IneqStdForm -> Double
getStdConst (EquStd _ x) = x
getStdConst (LteStd _ x) = x
getStdConst (GteStd _ x) = x

mapStdConst :: (Double -> Double) -> IneqStdForm -> IneqStdForm
mapStdConst f (EquStd xs xc) = EquStd xs (f xc)
mapStdConst f (LteStd xs xc) = LteStd xs (f xc)
mapStdConst f (GteStd xs xc) = GteStd xs (f xc)

-- | Turns a user-level AST to a structurally standard from inequality.
standardForm :: Ineq -> IneqStdForm
standardForm = go . standardize
  where
    go (Equ (LinExpr xs xc) (LinExpr ys yc)) | null xs && yc == 0 = EquStd ys xc
                                             | null ys && xc == 0 = EquStd xs yc
    go (Lte (LinExpr xs xc) (LinExpr ys yc)) | null xs && yc == 0 = GteStd ys xc -- Ax >= M
                                             | null ys && xc == 0 = LteStd xs yc -- Ax <= M
    go _ = error "Non-standard Ineq"

-- | Standardizes user-level inequalities - to be used before @standardForm@.
standardize :: Ineq -> Ineq
standardize (Equ (LinExpr xs xc) (LinExpr ys yc))
  | null xs   = Equ (LinExpr [] (xc - yc)) (LinExpr ys 0)
  | null ys   = Equ (LinExpr xs 0) (LinExpr [] (yc - xc))
  | otherwise =
      let
        ys' = map (mapCoeff $ (*) (-1)) ys
      in
      Equ (removeDupLin $ LinExpr (ys' ++ xs) 0) (LinExpr [] (yc - xc))
standardize (Lte (LinExpr xs xc) (LinExpr ys yc))
  | null xs   = Lte (LinExpr [] (xc - yc)) (LinExpr ys 0)
  | null ys   = Lte (LinExpr xs 0) (LinExpr [] (yc - xc))
  | otherwise =
      let
        ys' = map (mapCoeff $ (*) (-1)) ys
      in
      Lte (removeDupLin $ LinExpr (ys' ++ xs) 0) (LinExpr [] (yc - xc))
