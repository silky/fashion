module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import qualified Nvds.Designs.CubicLimitSeries  as CLS
import qualified Nvds.Designs.Tilings95         as T95
import qualified Nvds.Designs.MoonTile          as MT

main :: IO ()
main = do
  mainWith CLS.design
  -- putStrLn "hello world"
