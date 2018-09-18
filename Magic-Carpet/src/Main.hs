{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Control.Monad
import Data.Array.IO
import Data.List
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Nvds.Colours.ColourSets
import System.Random


-- colours = pisos ++ sana
colours = junina ++ lisaFrank


main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"


row :: Colour Double
    -> Colour Double
    -> Double
    -> Double
    -> Diagram B
row c1 c2 w h = centerXY . scaleX s . hcat . replicate n $ r1 ||| r2
  where
    r1       = rect w h # lw 0 # fc c1
    r2       = rect w h # lw 0 # fc c2
    rowWidth = 10
    fraction = rowWidth / w
    n        = floor fraction
    s        = rowWidth / (fromIntegral n * w)


d :: IO (Diagram B)
d = do

  shuffledColours <- shuffle colours

  let items = 30

  ourColours <- replicateM items $ do
                        i1 <- randomRIO (0, length shuffledColours - 1)
                        i2 <- randomRIO (0, length shuffledColours - 1)
                        return ( shuffledColours !! i1
                               , shuffledColours !! i2)

  dims <- replicateM items $ do
                        h <- randomRIO (0.1, 3.0)
                        w <- randomRIO (0.1, 3.0)
                        return (w, h)

  let g d ((h, w), (c1, c2)) = d === row c1 c2 h w
      m = foldl' g mempty (zip dims ourColours)

  return m


swapElements_ :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapElements_ arr i j = do a <- readArray arr i
                           b <- readArray arr j
                           writeArray arr i b
                           writeArray arr j a
                           return ()


shuffle :: [a] -> IO [a]
shuffle xs = do let upperBound = length xs
                arr <- (newListArray (1, upperBound) :: [a] -> IO (IOArray Int a)) xs
                mapM_ (shuffleCycle arr) [2..upperBound]
                getElems arr
  where shuffleCycle arr i = do j <- getStdRandom (randomR (1, i))
                                swapElements_ arr i j

