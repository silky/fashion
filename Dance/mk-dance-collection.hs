#!/usr/bin/env stack
{- stack script --resolver lts-9.11
		--package turtle
        --package text
        --package string-conv
-}

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (unwords)
import Turtle
import Control.Monad
import Data.Text (unwords)
import Data.String.Conv

tshow = toS . show
run   = flip shell empty

main :: IO ()
main = do
    let combos :: [(Text, Text)]
        -- combos = [ ("fiesta",           "aquamarine") 
        --          , ("brewerSet3_12",    "blue")
        --          , ("incaTawantinsuyo", "black")
        --          , ("takoTank",         "black")
        --          , ("atTheBeach",       "mediumpurple")
        --          , ("fiesta",           "mediumpurple")
        --          , ("carnaval",         "pink")
        --          , ("christmas",        "white")
        --          , ("brewerSet3_12",    "white") -- original
        --          ]

        -- combos = [ ("brewerSet3_12", "white") ]
        combos = [ ("takoTank", "black") ]

    let designs = [ tshirt
                  , hoodie 
                  , sweatshirt 
                  , raincoat 
                  , bomber 
                  , silkTop 
                  , shiftDress 
                  , squareDress 
                  , pants 
                  , leggings 
                  , yogaPants 
                  , totebag 
                  , backpack 
                  , clutch 
                  , silkScarf 
                  , hat
                  , suitJacket
                  , suitPants
                  , neopreneSkirt
                  , summerSkirt
                  , onesie
                  ]

    forM_ [ (c, bg, f) | (c,bg) <- combos, f <- designs ] $ \(colourSet, bg, f) ->
    -- forM_ combos $ \(colourSet, bg) -> do
    --     forM_ designs $ \f -> do
        f colourSet bg


basic mw mh cropArgs name colourSet bg = do
    let fname = name <> "-base.png"
        --
        mparam :: Text -> (Maybe Int) -> Text
        mparam _ Nothing       = ""
        mparam prefix (Just v) = unwords [prefix, (tshow (dpi * v))]


    -- stack run -- -- do-montage --video-width 384 --video-height 288
    --    --source-directory ~/tmp/eod-poses/ --rows 18 --columns 23 --out-file
    --    a.png --out-width 3360
    run $ unwords ["dance-view"
                  , "do-montage"
                  , "--video-width", "1280"
                  , "--video-height", "720"
                  , "--source-directory", "/home/noon/ml-data/pose/gala1/"
                  , "--rows", "9"
                  , "--columns", "15"
                  , "--out-file", fname
                  , mparam "--out-width"  mw
                  , mparam "--out-height" mh
                  ]

    -- 2. Crop it
    run $ unwords ["convert"
                  , fname
                  , "-crop", cropArgs
                  , name <> "-paom.png"
                  ]


--
-- 72 doesn't look good. 96 does.
--
dpi                = 96
fromInches w h     = (dpi * w, dpi * h)
defaultCrop (w, h) = tshow w <> "x" <> tshow h <> "+0+0+0"
ca w h             = defaultCrop (fromInches w h)


-- ==============================================================================
-- Tops
-- cotton t-shirt    : 35x20
tshirt colourSet bg = basic (Just 35) Nothing (ca 35 20)
                        (colourSet <> "-" <> bg <> "-tee") colourSet bg

-- hoodie            : 35x20
hoodie colourSet bg  = basic (Just 35) Nothing (ca 35 20)
                        (colourSet <> "-" <> bg <> "-hoodie") colourSet bg

-- cotton sweatshirt : 35x20
sweatshirt colourSet bg  = basic (Just 35) Nothing (ca 35 20)
                        (colourSet <> "-" <> bg <> "-sweatshirt") colourSet bg

-- raincoat          : 40x30
raincoat colourSet bg  = basic (Just 40) Nothing (ca 40 30)
                        (colourSet <> "-" <> bg <> "-raincoat") colourSet bg

-- bomber jacket     : 40x30
bomber colourSet bg  = basic (Just 40) Nothing (ca 40 30)
                        (colourSet <> "-" <> bg <> "-bomber") colourSet bg

-- vip silk top      : 35x20
silkTop colourSet bg  = basic (Just 35) Nothing (ca 35 20)
                        (colourSet <> "-" <> bg <> "-silkTop") colourSet bg

suitJacket colourSet bg  = basic (Just 40) Nothing (ca 40 30)
                        (colourSet <> "-" <> bg <> "-suitJacket") colourSet bg

suitPants colourSet bg  = basic Nothing (Just 40) (ca 35 40)
                        (colourSet <> "-" <> bg <> "-suitPants") colourSet bg


-- ==============================================================================
-- Dresses
-- shift dress       : 35x40
shiftDress colourSet bg  = basic Nothing (Just 40) (ca 35 40)
                        (colourSet <> "-" <> bg <> "-shiftDress") colourSet bg

-- vp square dress   : 35x45
squareDress colourSet bg  = basic Nothing (Just 45) (ca 35 45)
                        (colourSet <> "-" <> bg <> "-squareDress") colourSet bg

-- vp neoprene skirt : 35x30
neopreneSkirt colourSet bg  = basic (Just 35) Nothing (ca 35 30)
                        (colourSet <> "-" <> bg <> "-neopreneSkirt") colourSet bg

summerSkirt colourSet bg  = basic (Just 35) Nothing (ca 35 20)
                        (colourSet <> "-" <> bg <> "-summerSkirt") colourSet bg

-- ==============================================================================
-- Pants
-- sweatpant         : 35x40
pants colourSet bg  = basic Nothing (Just 40) (ca 35 40)
                        (colourSet <> "-" <> bg <> "-pants") colourSet bg

-- leggings          : 35x40
leggings colourSet bg  = basic Nothing (Just 40) (ca 35 40)
                        (colourSet <> "-" <> bg <> "-leggings") colourSet bg

-- yoga pants        : 35x40
yogaPants colourSet bg  = basic Nothing (Just 40) (ca 35 40)
                        (colourSet <> "-" <> bg <> "-yogaPants") colourSet bg

-- ==============================================================================
-- Bags
-- tote bag          : 25x20
totebag colourSet bg  = basic (Just 25) Nothing (ca 25 20)
                        (colourSet <> "-" <> bg <> "-totebag") colourSet bg
-- backpack          : 25x20
backpack colourSet bg  = basic (Just 25) Nothing (ca 25 20)
                        (colourSet <> "-" <> bg <> "-backpack") colourSet bg
-- neoprene clutch   : 15x10
clutch colourSet bg  = basic (Just 15) Nothing (ca 15 10)
                        (colourSet <> "-" <> bg <> "-clutch") colourSet bg


-- ==============================================================================
-- Misc
-- silk scarf        : 45x60
silkScarf colourSet bg  = basic Nothing (Just 60) (ca 45 60)
                        (colourSet <> "-" <> bg <> "-silkScarf") colourSet bg

-- hat               : 20x20
-- Note: Squares don't work well, so we make it slightly larger then crop.
hat colourSet bg  = basic Nothing (Just 22) (ca 20 20)
                        (colourSet <> "-" <> bg <> "-hat") colourSet bg

onesie colourSet bg  = basic Nothing (Just 55) (ca 40 55)
                        (colourSet <> "-" <> bg <> "-onesie") colourSet bg


-- basic sweatshirt  : 35x20
-- basic t-shirt     : 35x20
--
