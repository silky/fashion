{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.List hiding (union)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union)
import Diagrams.TwoD.Offset (expandPath)
import Diagrams.TwoD.Path.Boolean (union)


n,v,d,s :: [(Double, Double)]

n = [ (0, 0), (0, 5) , (3, 0), (3, 5) ]
v = drop 1 n
d = take 2 v ++ [(0, 0), (2, 2)]

-- This is an independent letter.
s = [ (0, 0), (2, 3), (0, 4), (2, 7) ]


logo :: Diagram B
logo =  (  mkPath n # lc black
        <> mkPath v # lc magenta # translate (r2 ( 0.2, 0))
        <> mkPath d # lc magenta # translate (r2 (-0.2, 0))
        )
        ||| strutX 0.8
        ||| mkPath s # lc black # scale 0.7
    where
        mkPath ps = Path ([trails ps]) # strokeP
        trails ps = fromVertices (map p2 ps)


diagram :: Diagram B
diagram = (logo 
            # centerXY
            # lw 10
            -- <> (rect 3 5 # lc lightgray)
          )
          # frame 2

main :: IO ()
main = mainWith diagram
