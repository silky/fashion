module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Colours.ColourSets

import qualified Nvds.Designs.CubicLimitSeries  as CLS
import qualified Nvds.Designs.Tilings95         as T95
import qualified Nvds.Designs.MoonTile          as MT

main :: IO ()
main = do
  mainWith (bg white . frame 0.2 <$> d) >> putStrLn "Done!"


-- room :: Diagram B
-- room = roomLeft <> roomMiddle <> roomRight <> roomBottom
--   where
--     roomLeft   = mempty
--     roomRight  = mempty
--     roomMiddle = squareParts # map fromVertices # mconcat
--     roomBottom = mempty


d :: IO (Diagram B)
d = do
  -- d1 <- CLS.design

  -- let cs = cycle pisos
  -- let cs = cycle vaporWaves
  -- let cs = cycle memphis
  -- let cs = cycle lookMeInTheEye
  let cs = cycle noodles

  let d1 = MT.tiledMoon 15 (take 3 cs) # scale 0.1
  let d2 = MT.tiledMoon 15 (take 3 (drop 3 cs)) # scale 0.1
  let d3 = MT.tiledMoon 15 (take 3 (drop 6 cs)) # scale 0.1

  let sq  = square 1 # lw 2 # lc white

  let d1' = (sq <> d1 # centerXY)
                # clipTo (square 1)
                -- # clipBy (square 1)
                -- # withEnvelope (square 1 :: Diagram B)

  let d2' = (sq <> d2 # centerXY)
                # clipTo (square 1)
                -- # clipBy (square 1)
                -- # withEnvelope (square 1 :: Diagram B)
                # snugR # snugT

  let d3' = (sq <> d3 # centerXY)
                # clipTo (square 1)
                -- # clipBy (square 1)
                -- # withEnvelope (square 1 :: Diagram B)

  let roomMiddle = d1' -- # centerXY
                       # snugL # snugT


      roomLeft   = d2'-- # centerXY
                       # rotateBy (1/8)
                       # scaleY 2
                       # rotateBy ( 1 - ( atan (1/2) / (2 * pi) ) )
                       # scale (1/sqrt 2.5)

      roomRight   = d3' # rotateBy (1/8)
                        # scaleY 2
                        # rotateBy ( 1 - ( atan (1/2) / (2 * pi) ) )
                        # scale (1/sqrt 2.5)
                        -- # showEnvelope
                        # reflectX
                        # snugL # snugT
                        -- # showEnvelope
                        -- # showOrigin
                        -- # showOrigin
                        -- # showEnvelope
          

  let room = (roomLeft <> roomMiddle) # snugR # snugT
              <> roomRight 


  -- return roomLeft
  -- return $ vsep 0 [room, room # reflectY]
  return $ room
  -- return d1'

