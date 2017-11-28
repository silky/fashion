{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Control.Monad (replicateM, liftM2)
import Control.Monad.IO.Class (liftIO)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union, intersection, difference)
import Diagrams.TwoD.Path.Boolean (union, intersection, difference)
import Nvds.Colours.ColourSets
import System.Random
import Data.List.Split (chunksOf)


single :: Colour Double -> Colour Double -> Colour Double -> Diagram B
single c1 c2 c3 = 
          (intersection Winding
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


transforms :: [Diagram B -> Diagram B]
transforms = liftM2 (.) [ id , reflectX, reflectY ] [ reflectX, reflectY ]


diagram :: Diagram B
diagram = (single magenta blue cyan 
		  ||| reflectX (single blue yellow magenta))
		  === ((reflectY (single blue cyan magenta))
          ||| (reflectX (reflectY (single cyan magenta yellow ))))



myColours = [ magenta, blue, cyan, yellow ]


randDiagram :: IO (Diagram B)
randDiagram = do
    let rows    = 2
        columns = 3

    all <- replicateM (rows * columns) $ do
        i1 <- randomRIO (0, length myColours - 1)
        i2 <- randomRIO (0, length myColours - 1)
        i3 <- randomRIO (0, length myColours - 1)

        let c1 = myColours !! i1
            c2 = myColours !! i2
            c3 = myColours !! i3

        i4 <- randomRIO (0, length transforms - 1)

        return $ (transforms !! i4) (single c1 c2 c3)

    return $ vcat (map hcat (chunksOf columns all))
             # frame 1


main :: IO ()
-- main = mainWith (diagram # frame 1)
main = mainWith (randDiagram)
