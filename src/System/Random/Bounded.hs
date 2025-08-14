{-# LANGUAGE ScopedTypeVariables #-}
module System.Random.Bounded where
import System.Random
import System.Random.Stateful (StatefulGen, uniformRM, uniformM)

uniformBounded :: (Enum a, Bounded a, RandomGen g) => g -> (a, g)
uniformBounded oldGen = (x, newGen) where
    (i, newGen) = uniformR
        (fromEnum $ minBound `asTypeOf` x, fromEnum $ maxBound `asTypeOf` x)
        oldGen
    x = toEnum i

uniformBoundedChoice
    :: forall a b c g m.(
        Enum a, Bounded a, Uniform a, 
        Enum b, Bounded b, Uniform b, 
        StatefulGen g m
    )
    => (a -> c)
    -> (b -> c)
    -> g
    -> m c
uniformBoundedChoice f1 f2 g = do
    n <- uniformRM (0, vCount + lCount - 1) g
    if n < vCount then f1 <$> uniformM g else f2 <$> uniformM g
  where
    vCount = fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1
    lCount = fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1