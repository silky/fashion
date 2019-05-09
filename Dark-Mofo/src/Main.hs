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
d = applyNTimes 6 tree mempty


r x y = rect x y # bg mediumslateblue
                 # lw none
                 # alignT

tree :: Diagram B -> Diagram B
tree d = d # alignT <> (  (r 10 1 # alignT <> d) # rotateBy (1/8)  # alignL
                       <> (r 10 1 # alignT <> d) # rotateBy (-1/8) # alignR
                       )


base = (rect 10 1 <> rect 1 10)
        # fc blue
        # lw none
