{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Nvds.Designs.GlitterPainting where

import Control.Monad
import Data.IORef (newIORef)
import Data.Random.Source.StdGen (mkStdGen)
-- import qualified Data.Random.Distribution.Normal  as D
import qualified Data.Random.Distribution.Uniform as D
import Data.Random (runRVar)
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List.Split

star' :: Colour Double -> Diagram B
star' c = interior <> exterior
  where
    s n = star (StarSkip 3) (regPoly n 1) 
                  # strokeP
                  # lw none

    exterior = s 9 # fc c
    interior = s 5 # fc white # scale 0.7



d :: IO (Diagram B)
d = do
  blackStars   <- stars black  15000 1
  goldStars    <- stars gold     200 2
  yellowStars  <- stars yellow   200 3 
  blueStars    <- stars blue     200 4
  silverStars  <- stars silver   200 5
  magentaStars <- stars magenta  200 6

  let m =    goldStars
          <> blueStars
          <> silverStars
          <> yellowStars
          <> magentaStars
          --
          <> blackStars

  return $ m



stars :: Colour Double
      -> Int
      -> Int
      -> IO (Diagram B)
stars colour count s0 = do
  let dist  = D.uniform (-1) 1
      -- count = 10000

  r <- newIORef (mkStdGen s0)
  [xs, ys] :: [[Double]] <- chunksOf count <$> ( flip runRVar r $ replicateM (count * 2) dist )

  let pts = zipWith (^&) xs ys
      m   = position $ zip pts (repeat (star' colour # scale 0.01))

  return $ m
  -- Plan:
  -- 1. Millions of circle
  -- 2. A few colours
  -- 3. Black background
  -- 4. That's it.
  return $ m
