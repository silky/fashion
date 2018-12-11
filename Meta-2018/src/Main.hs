{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import qualified Nvds.Designs.CommunityCircle   as CC 
import qualified Nvds.Designs.CubicLimitSeries  as CLS
import qualified Nvds.Designs.Cylinders         as C
-- Dance
import qualified Nvds.Designs.DoubleSlit        as DS
import qualified Nvds.Designs.FusionCompression as FC
import qualified Nvds.Designs.GlitterPainting   as GP
import qualified Nvds.Designs.MagicCarpet       as MC
import qualified Nvds.Designs.MoonTile          as MT
-- Quantum-AI-Blocktee
import qualified Nvds.Designs.RetroHaskell      as RH
import qualified Nvds.Designs.SBI86             as SB
import qualified Nvds.Designs.ScanLines         as SL
-- SimpleGeometry
-- Sketchy-Depths
-- Space-Split
import qualified Nvds.Designs.SunMountainScape as SMS
import qualified Nvds.Designs.Teseloni         as T
-- Text-Path
import qualified Nvds.Designs.Tilings95        as T95
-- Vector-Field
import qualified Nvds.Designs.WarpSpeed        as WS
-- Wave-Axis
-- Wavey-Paper
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (frame 0.2 <$> e) >> putStrLn "Done!"


e :: IO (Diagram B)
e = do
  wsd <- WS.d
  sms <- SMS.d

  let ds :: [Diagram B]
      ds = [ wsd
           , T95.dd 
           , T.d
           , sms
           ]

  return $ mconcat ds
  where

