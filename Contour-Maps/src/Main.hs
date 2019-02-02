{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Canny
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"

d :: IO (Diagram B)
d = do

  -- TODO: Make arguments, etc.
  let fileIn  = "../images/less.bmp"
      fileOut = "../images/out.bmp"

  run 0 5 fileIn fileOut

  return $ mempty
