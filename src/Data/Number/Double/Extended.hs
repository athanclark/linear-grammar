module Data.Number.Double.Extended where


-- | Boolean equality with @e^-6@ precision.
eqDouble :: Double -> Double -> Bool
eqDouble x y = abs (x - y) <= 0.000001
