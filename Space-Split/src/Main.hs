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
import Nvds.Svg


main :: IO ()
main = mainWith d >> putStrLn "Done!"


star' :: Colour Double 
      -> Double 
      -> Diagram B
star' c r = circ
  where
    circ = circle r # fc c # lw none


landing =  c # centerXY
             # clipTo (rect 2 0.7)
             # alignB
  where
    c = m # rotate (-40 @@ deg)
    m = lines 
          # centerXY 
          # alignB
        <> rect w h 
            # alignB
            # lw none 
            # fc mediumslateblue

    w      = 3
    h      = 3
    count  = 100
    line x = (x ^& 0) ~~ (x ^& h)
    lines  = mconcat (map line [0, w/count .. w ])
              # lc black
              # lw 0.8


ifilter :: (Int -> Bool) -> [a] -> [a]
ifilter p xs = map snd $ filter (p . fst) (zip [1..] xs)


d :: IO (Diagram B)
d = do
  Just emuPath <- singlePathFromFile "../svg-input/emu.svg"

  blueStars  <- stars royalblue 90  1
  redStars   <- stars red       90  2
  whiteStars <- stars black     140 3 
  bgStars    <- stars' (D.uniform (-1) 1) 0.15 1 1 gold 700  4
  farStars   <- stars' (D.uniform (-1) (-0.2)) 0.12 (2 / 0.8) 1 gold 2000  5
  -- farStars   <- stars' (D.uniform (-1) 1) 0.12 1 1 gold 2000  5

  let m = (    blueStars
          <> redStars
          <> whiteStars
          <> bgStars ) # alignB
          <> farStars # centerXY # alignB

  let emuPoints = ifilter (\i -> (i `mod` 1200) == 0) (toNormalisedPoints emuPath)
                     # scale 2
      emuStars  = mconcat (map (\pt -> star' orange 0.0012 # moveTo pt) emuPoints)
      emu       = emuStars # centerXY

  let b = (landing # alignB <> m # alignB)
            # centerXY
  -- let b = m
      t = text "BRANESHOP" # fontSize (local 1.1)
            # font "Fira Code"
            # bold
          <> rect 10 2 # fc white # lw none
          <> (rect 10 2 # fc royalblue # lw none # translate (0.4 ^& (-0.4)))

      c = b # bg black 
            # rotate (-30 @@ deg)
            # centerXY
            # clipTo (rect 1.2 0.4)

  -- return $ b # bg black

  return $ (emu <> c # centerXY) # bg black



stars :: Colour Double
      -> Int
      -> Int
      -> IO (Diagram B)
stars = stars' (D.uniform (-1) 1) 0.3 1 1
  

stars' dist radius rx ry colour count s0 = do

  r <- newIORef (mkStdGen s0)
  [xs, ys] :: [[Double]] <- chunksOf count <$> ( flip runRVar r $ replicateM (count * 2) dist )

  let pts = zipWith (\x y -> (rx * x) ^& (ry * y)) xs ys
      m   = position $ zip pts (repeat (star' colour radius # scale 0.01))

  return $ m
