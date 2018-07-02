{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

-- stack run -- -- --loop -h 200 -w 200 -s src/Main.hs -o a.svg

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.List hiding (union)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude


-- v: an angle
-- d: beizer curve
-- s: two beizer curves

logo :: Diagram B
logo =  s
         # frame 0.2

-- TODO: Make this into an "S"
-- https://hackage.haskell.org/package/diagrams-lib-1.1.0.3/candidate/docs/Diagrams-Segment.html
-- https://en.wikipedia.org/wiki/B%C3%A9zier_curve
-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/manual.html
s :: Diagram B
s = fromSegments [bézier3 c1 c2 x2]
    where
        x2       = r2 (0, 1) :: V2 Double
        [c1, c2] = map r2 [(0, 0), (1, 0)]


main :: IO ()
main = mainWith logo
