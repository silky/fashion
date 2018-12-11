{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where


import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union)
import Nvds.Designs.RetroHaskell

-- main = mainWith gif
main = mainWith (retroHaskell)
-- main = mainWith (colourSample)

