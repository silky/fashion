{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
-- Static
-- main = mainWith (frame 0.2 <$> static) >> putStrLn "Done!"
main = mainWith ( gif ) >> putStrLn "Done!"


gif :: IO [(Diagram B, Int)]
gif = do
  let n = 200

  imgs <- mapM (\k -> bg white . frame 0.2 <$> d k) [1,5..n]

  let half = zip imgs [ 5 | k <- [1..n] ]
      
  return $ half ++ reverse half

static = d 500


base :: Diagram B
base = circle 1 # lw none


d :: Double 
  -> IO (Diagram B)
d n = do
  let anchorAngles = [ 0, 90, 180, 270 ]
      anchorPts    = map mkPt anchorAngles
      mkPt a       = sin (a / (180/pi)) ^& cos (a / (180/pi))

  let c = circle 0.05 # fc black
      m = base 
 
  let totalLines = n
      range      = pi

  let lines a = mconcat $ map (f a) [0..totalLines-1]
                  # lw 0.4

      f a k   = mkPt a ~~
                mkPt (a + ((k+1) * (180/totalLines)))

  let topLayer = (lines 270) # lc orange
      crop     = topLayer # clipBy (wedge 1 xDir (90 @@ deg))

  return $ (m <> crop 
              <> (lines   0) # lc purple
              <> (lines  90) # lc blue
              <> (lines 180) # lc cyan
              <> topLayer
           ) # reflectX 

