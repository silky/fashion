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


diagram :: Diagram B
diagram = (intersection Winding
                (circle 2 # translateY (-2.5))
                (square 5)
          )
          # strokeP
          # fc red
          # lw 0
		  <> polygon ( with
			& polyType .~ PolySides
				[ 135 @@ deg, 90 @@ deg]
				[ 5        , sqrt (5 ^ 2 + 5 ^ 2) ]
			) # centerXY
			  # fc blue
			  # lw 0
          <> square 5 # fc yellow # lw 0


main :: IO ()
main = mainWith (diagram # frame 1)
