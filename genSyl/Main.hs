{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.Random
import System.Random.Stateful
import Data.Proxy

import Syllable ( isL, Nucleus, Syllable )
import Control.Monad (replicateM)
import Data.List (nub)
import System.IO (hSetEncoding, stdout, utf8, utf16, mkTextEncoding)
import System.IO.CodePage (withCP65001)


pureGen = mkStdGen 628


uniformBounded :: (Enum a, Bounded a, RandomGen g) => g -> (a, g)
uniformBounded oldGen = (x, newGen) where
    (i, newGen) = uniformR
        (fromEnum $ minBound `asTypeOf` x, fromEnum $ maxBound `asTypeOf` x)
        oldGen
    x = toEnum i

testNucProp :: Int -> IO ()
testNucProp n = do
    gen <- initStdGen
    ioGen <- newIOGenM gen
    nucs <- replicateM n (uniformM ioGen :: IO Nucleus)
    let lProp = fromIntegral (length $ filter isL nucs) / fromIntegral n
    let trueProp = 1 / 3
    print $ "proportion more Els than expected: " ++ show (lProp - trueProp)

main3 :: IO ()
main3 = initStdGen >>= newIOGenM >>= uniformM >>= (print :: Syllable -> IO ())

switchToUtf8 = hSetEncoding stdout utf8 --doesn't work. just emits corresponding bytes on the wrong codepage.

main :: IO ()
main =  withCP65001 main3
