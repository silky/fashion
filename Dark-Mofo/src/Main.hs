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


arm :: Int -> Diagram B
arm n = applyNTimes n tree mempty


d :: Diagram B
d = r <> r # rotateBy (1/16)
  where
    r = s <> s # rotateBy (1/8)
    s = branches <> branches # rotateBy (1/4)
    depth    = 10
    arm'     = arm depth
    branches = (  
                 (base # alignT <> arm' # alignB # translateY (-1)) # alignB 
                 <> 
                 (arm' # rotateBy (1/2) # alignT # translateY 1)
               )
               # centerXY


tree :: Diagram B -> Diagram B
tree d' = ( (r 10 1 # alignR <> d # rotateBy (-1/4)) # rotateBy (1/8) # alignL # translateX (-0.5)
         <> (r 10 1 # alignL <> d # rotateBy (1/4)) # rotateBy (-1/8) # alignR # translateX ( 0.5)
          ) 
  where
    d = d' # scale 0.6
           # alignB
           # translateY (-1)


base = (r 10 1 <> r 1 10)
        # scale 2


r x y = rect x y # bg mediumslateblue
                 # lc mediumslateblue
                 # lw none

