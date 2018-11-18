{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import Control.Monad
import Data.IORef (newIORef)
import Data.Random.Source.StdGen (mkStdGen)
import qualified Data.Random.Distribution.Uniform as D
import Data.Random (runRVar)
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List.Split


main :: IO ()
main = mainWith (frame 0.05 <$> d) >> putStrLn "Done!"


star' :: Colour Double 
      -> Double 
      -> Diagram B
star' c r = circ
  where
    circ = circle r # fc c # lw none


landing =  c # centerXY
             # clipTo (rect 2 0.6)
             # alignB
  where
    c = m # rotate (-40 @@ deg)
    m = lines # centerXY # alignB
          <> rect w h # alignB
              # lw none # fc darksalmon
    w      = 3
    h      = 3
    count  = 50
    line x = (x ^& 0) ~~ (x ^& h)
    lines  = mconcat (map line [0, w/count .. w ])
              # lc black
              # lw 0.8


d :: IO (Diagram B)
d = do
  blueStars  <- stars royalblue 90  1
  redStars   <- stars red       90  2
  whiteStars <- stars white     140 3 

  orangeStars  <- stars' (D.uniform (-1) 1) 0.1 pink 700  4

  let m =     blueStars
          <> redStars
          <> whiteStars
          <> orangeStars

  let b = landing # alignB <> m # alignB
      c = b # bg black 
            # rotate (-30 @@ deg)
            # centerXY
            # clipTo (rect 1.2 1.2)

  -- return $ b
  return $ c



stars :: Colour Double
      -> Int
      -> Int
      -> IO (Diagram B)
stars = stars' (D.uniform (-1) 1) 0.3
  

stars' dist radius colour count s0 = do

  r <- newIORef (mkStdGen s0)
  [xs, ys] :: [[Double]] <- chunksOf count <$> ( flip runRVar r $ replicateM (count * 2) dist )

  let pts = zipWith (^&) xs ys
      m   = position $ zip pts (repeat (star' colour radius # scale 0.01))

  return $ m
