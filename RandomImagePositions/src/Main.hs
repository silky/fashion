{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List hiding (find)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import GSL.Random.Quasi (halton, QRNGType, getListSample, newQRNG)
import System.FilePath
import System.FilePath.Find
import Data.Array.IO
import System.Random
import Data.List.Split (chunksOf)

lowerCase :: String -> String
lowerCase = map toLower


getByExts :: [String] -> FilePath -> IO [FilePath]
getByExts exts = find always (foldl1 (||?) tests)
    where   
        tests = map (\e -> (lowerCase <$> extension) ==? e) exts 


getPoints :: QRNGType -> Int -> IO [Point V2 Double]
getPoints qrngType n = do
    rng     <- newQRNG qrngType 2
    points  <- replicateM n (getListSample rng)

    return $ map (\[a,b] -> mkP2 a b) points


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


d :: FilePath -> IO (Diagram B)
d dir = do
    let n       = 300
        density = 0.03

    points <- getPoints halton n

    allFiles  <- getByExts [".png", ".jpg"] dir
    files     <- take n <$> shuffle allFiles
    baseDiags <- scale density <$> mapM simpleImage files
    
    -- Lay things out in a grid instead:
    -- let ds = vcat (map hcat (chunksOf 10 baseDiags))
    
    rs <- replicateM n (randomRIO (0, 1))
    let diags = zipWith (\r d -> d # rotateBy r) rs baseDiags
    let ds    = position (zip points diags)
    
    return $ ds # bg white


simpleImage :: FilePath -> IO (Diagram B) 
simpleImage f = do
    r <- loadImageEmb f
    return $ case r of
               Left _  -> mempty
               Right i -> image i # sized (dims2D 1 1)


main :: IO ()
main = mainWith d
