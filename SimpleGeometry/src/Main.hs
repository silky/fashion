{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.List hiding (union)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union, intersection, difference)
import Diagrams.TwoD.Offset (expandPath)
import Diagrams.TwoD.Path.Boolean (union, intersection, difference)


-- single :: Diagram B
single c1 c2 c3 = (intersection Winding
                (circle 2 # translateY (-2.5))
                (square 5)
		  )
          # strokeP
          # fc c3
          # lw 0
		  <> polygon ( with
			& polyType .~ PolySides
				[ 135 @@ deg, 90 @@ deg]
				[ 5        , sqrt (5 ^ 2 + 5 ^ 2) ]
			) # centerXY
			  # fc c2
			  # lw 0
          <> square 5 # fc c1 # lw 0


diagram :: Diagram B
diagram = single magenta blue cyan 
		  -- ||| reflectX (single blue yellow red)
		  === (reflectY (single blue cyan magenta))
		  -- ||| reflectY (single red blue red))



main :: IO ()
main = mainWith (diagram # frame 1)
