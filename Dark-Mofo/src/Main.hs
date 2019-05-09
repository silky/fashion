{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f val = foldl (\s e -> e s) val [f | x <- [1..n]]

d :: Diagram B
d = applyNTimes 8 tree mempty


tree :: Diagram B -> Diagram B
tree d' = ( (r 10 1 # alignR <> d # rotateBy (-1/4)) # rotateBy (1/8) # alignL
         <> (r 10 1 # alignL <> d # rotateBy (1/4)) # rotateBy (-1/8) # alignR
          )
  where
    d = d' # scale 0.5
           # alignB
          


base = (rect 10 1 <> rect 1 10)
        # fc blue
        # lw none

r x y = rect x y # bg mediumslateblue
                 # lc black


