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
import qualified Nvds.Designs.SketchyDepths    as SD
-- Space-Split
import qualified Nvds.Designs.SunMountainScape as SMS
import qualified Nvds.Designs.Teseloni         as T
-- Text-Path
import qualified Nvds.Designs.Tilings95        as T95
-- Vector-Field
import qualified Nvds.Designs.WarpSpeed        as WS
import qualified Nvds.Designs.WaveAxis         as WA
-- Wave-Axis
-- Wavey-Paper
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (frame 0.2 <$> e) >> putStrLn "Done!"


e :: IO (Diagram B)
e = do
  wsd  <- WS.d
  sms  <- SMS.d
  cc   <- CC.d 100
  cls1 <- CLS.pdesign (CLS.circleParts 80) 1 False white
  cls2 <- CLS.pdesign (CLS.cubeParts) 5 True black
  gp   <- GP.d
  mc   <- MC.d
  sketches1 <- SD.d "flower"
  sketches2 <- SD.d "tornado"

  let ds :: [Diagram B]
      ds = map (alignT . alignL)
           [ wsd # scale 1.2
           , cc # scale 0.6
           , T.d # scale 0.15
           , sms # scale 0.080
           , T95.dd  # scale 0.15
           ]
      ds2 = map (alignT . alignL)
            [ cls1 # scale 0.4
            , cls2 # scale (0.4 / 5)
            , gp # scale 0.65
            , FC.d # scale 0.75
            , (mc # centerXY # scale 0.2 # clipTo (square 1.5))
            ]
      ds3 = map (alignT . alignL)
            -- [ sketches1 # scale 0.0005 # alignL
            [ sketches2 # scale 0.0005 # alignL # lc mediumslateblue
            , WA.d # scale 0.000035
            ]


  return $ vsep 0.1 [hsep 0.1 ds, hsep 0.1 ds2, hsep 0.1 ds3]
  -- return $ vsep 0.1 [hsep 0.1 ds, hsep 0.1 ds3]
            # bg white

