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

radial c = mkRadialGradient (mkStops [(white,0,1), (c,1,1)])
                          ((-1.10) ^& (1.10)) 0.3 (0 ^& 0) 2.5
                          GradPad

single :: Colour Double -> Colour Double -> Colour Double -> Diagram B
single c1 c2 c3 = 
          (intersection Winding
                -- (circle 2 # translateY (-2.5))
                (circle 2)
                (square 5)
          )
          # strokeP
          # fillTexture (radial c3)
          -- # fc c3
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


randDiagram :: Colours -> IO (Diagram B)
randDiagram colours = do
    let rows    = 1
        columns = 1

    all <- replicateM (rows * columns) $ do
        i1 <- randomRIO (0, length colours - 1)
        i2 <- randomRIO (0, length colours - 1)
        i3 <- randomRIO (0, length colours - 1)

        let c1 = colours !! i1
            c2 = colours !! i2
            c3 = colours !! i3

        i4 <- randomRIO (0, length transforms - 1)

        return $ (transforms !! i4) (single c1 c2 c3)

    return $ vcat (map hcat (chunksOf columns all))


randWithColours :: IO (Diagram B)
randWithColours = do
    let rows    = 5
        columns = 10
        -- Drop the last thing, which is the 'alwaysWhite' colour
        colours = init allColours

    all <- replicateM (rows * columns) $ do
        i <- randomRIO (0, length colours - 1)
        randDiagram (fst (colours !! i))

    return $ vcat (map hcat (chunksOf columns all))
             # frame 1

main :: IO ()
-- main = mainWith (diagram # frame 1)
-- main = mainWith (randDiagram myColours)
main = mainWith (randWithColours)
