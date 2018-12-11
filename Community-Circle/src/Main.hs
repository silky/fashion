{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Designs.CommunityCircle

main :: IO ()
-- Static
main = mainWith (frame 0.2 <$> static) >> putStrLn "Done!"
-- main = mainWith ( gif ) >> putStrLn "Done!"

