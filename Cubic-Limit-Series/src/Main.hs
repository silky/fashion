{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import Prelude hiding (lines)
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Tilings
import Data.List
import Data.List.Split (chunksOf)
import System.Random (randomRIO)
import Control.Monad
import Data.Random.RVar
import Data.Random
import Nvds.Colours.ColourSets


main :: IO ()
main = mainWith d


squareParts :: [[P2 Double]]
squareParts = 
    [ [ 0 ^& 0, 0 ^& 1 ]
    , [ 0 ^& 1, 1 ^& 1 ]
    , [ 1 ^& 1, 1 ^& 0 ]
    , [ 1 ^& 0, 0 ^& 0 ]
    ]


almostCubeParts :: ([[P2 Double]], [[P2 Double]])
almostCubeParts = (squareParts, (map offset squareParts))
    where
        offset [a, b] = [a + amount, b + amount] 
        amount = 0.5

    
cubeParts :: [[P2 Double]]
cubeParts = as ++ bs ++ zipWith g as bs
    where
        g [a, _] [c, _] = [a, c]
        (as, bs) = almostCubeParts


cubeWith :: Colour Double -> [Int] -> Diagram B
cubeWith c indicies = 
    sublines 
        # map fromVertices 
        # mconcat          
        # centerXY
    <> p
    where
        p :: Diagram B
        p = (square 3) # lw 0.2 # lc c

        sublines = map (\i -> cubeParts !! i) indicies


tile :: Colour Double -> Colour Double -> Colour Double -> [Int] -> Diagram B
tile c1 c2 c3 xs = cubeWith c2 xs # lw thin
                                  # lc c1
                                  # bg c3


someIndicies :: IO [Int]
someIndicies = do
    totalIndicies <- randomRIO (0, length cubeParts - 1)
    shuffled      <- runRVar (shuffle [0..length cubeParts - 1]) StdRandom

    let indicies = take totalIndicies shuffled

    return indicies


-- Let's do something else. Let's pick a random set of indicies,
-- then increment it randomly. Either by:
--
--  1. Removing some element
--  2. Adding some element
nextSet :: [Int] -> IO [Int]
nextSet xs = do
    b :: Int <- randomRIO (0, 1)
    i <- randomRIO (0, length xs - 1)

    let remaining = [0..length cubeParts - 1] \\ xs
    shuffled <- runRVar (shuffle remaining) StdRandom

    let forcedDrop = length xs == length cubeParts

    let xs' = if length xs > 1 && (b == 0 || forcedDrop) 
                then dropIndex i xs 
                else (xs ++ take 1 shuffled)

    return xs'


dropIndex :: Int -> [a] -> [a]
dropIndex 0 [] = []
dropIndex i [] = []
dropIndex 0 xs = drop 1 xs
dropIndex i xs = let (h, t) = splitAt i xs in h ++ dropIndex i (drop 1 t)


d :: IO (Diagram B)
d =  do
    let chunks  = 20
        items = chunks * chunks
        tile' = tile gray white black

    colourIndex <- randomRIO (0, length allColours - 1)

    let colourSet = fst $ allColours !! colourIndex

    -- Method 1:
    --  Random variations in each step
    --
    let f (cur, xs) _ = do xs' <- nextSet xs
                           return (xs' : cur, xs')

    seqs    <- foldM f (return [], [1, 2]) [0.. items - 2] >>= return . fst
    colours <- replicateM items (runRVar (shuffle colourSet) StdRandom)

    let getTile s cs = tile (head cs) (head cs) black s
    let tiles        = zipWith getTile seqs colours


    -- Method 2:
    --  Just completely random.
    --
    -- tiles <- replicateM items (someIndicies >>= return . tile')

    let diag = vcat (map hcat (chunksOf chunks tiles))

    return (diag # frame 0.2)

