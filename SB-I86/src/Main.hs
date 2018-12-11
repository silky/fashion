{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Prelude hiding (Left)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (tri')
import Nvds.Designs.SBI86

main :: IO ()
-- main = mainWith (frame 0.5 <$> diagTiled)
-- main = mainWith (frame 0.5 <$> diag1)
main = mainWith diagTiled

