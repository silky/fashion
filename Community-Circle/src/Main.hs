{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
-- Static
main = mainWith (frame 0.2 <$> static) >> putStrLn "Done!"
-- main = mainWith ( gif ) >> putStrLn "Done!"

static = d 90

gif :: IO [(Diagram B, Int)]
gif = do
  let n = 360

  imgs <- mapM (\k -> bg white . frame 0.2 <$> d k) [1,10..n]

  let half = zip imgs [ 10 | k <- [1..n] ]
      
  return $ half ++ reverse half



base :: Diagram B
base = circle 1 # lw none


d :: Double 
  -> IO (Diagram B)
d n = do
  let colours = [ orange
                , purple
                , blue
                , cyan
                , black
                , red
                , magenta
                , brown
                , gold
                , gray
                ]
      nc   = length colours
      frac = 360 / fromIntegral nc

  let anchorAngles = [ fromIntegral k * frac | k <- [0..nc-1] ]
      anchorPts    = map mkPt anchorAngles
      mkPt a       = sin (a / (180/pi)) ^& cos (a / (180/pi))

  let c = circle 0.05 # fc black
      m = base 
 
  let totalLines = n - 80
      -- range      = 360 / (fromIntegral nc/2) 
      range      = 270 

  let lines a = mconcat $ map (f a) [0..totalLines-1]
                  # lw 0.2

      f a k   = mkPt a ~~
                mkPt (a + ((k+1) * (range/totalLines)))

  let c1:cs    = colours
  let topLayer = (lines (last anchorAngles)) # lc c1
      crop     = topLayer # clipBy (wedge 1 xDir (90 @@ deg))

  return $ (m <> crop 
              <> mconcat (zipWith (\c f -> lines f # lc c) cs (init anchorAngles))
              <> topLayer
           ) # reflectX 

