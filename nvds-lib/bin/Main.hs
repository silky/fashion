{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List.Split (chunksOf)
import Nvds.Colours.ColourSets

main :: IO ()
main = mainWith ( d # frame 3 ) >> putStrLn "Done!"

d :: Diagram B
d = vsep 0.3 $ (map mkThing allColours)
  where
    mkThing (colours, name) = 
      (text name <> rect 20 2 # lw 0) # fontSize (local 1) # scale 0.7 
        ||| hcat (map (\c -> rect 1.5 1 # fc c # lw 0) colours)
