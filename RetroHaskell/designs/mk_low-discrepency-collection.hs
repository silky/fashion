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
run   =  flip shell empty

main :: IO ()
main = do
    let combos :: [(Text, Text)]
        combos = [ ("fiesta",           "aquamarine") 
                 , ("brewerSet3_12",    "blue")
                 , ("incaTawantinsuyo", "black")
                 , ("atTheBeach",       "purple")
                 , ("fiesta",           "purple")
                 , ("carnaval",         "pink")
                 ]

    forM_ combos $ \(colourSet, bg) -> do
        tshirt colourSet bg
        pants  colourSet bg


basic mw mh cropArgs name colourSet bg = do
    let fname = name <> "-base.png"
        --
        mparam :: Text -> (Maybe Int) -> Text
        mparam _ Nothing       = ""
        mparam prefix (Just v) = unwords [prefix, (tshow v)]

    -- 1. Generate a base file
    run $ unwords ["stack run -- --"
                    , mparam "-w" mw 
                    , mparam "-h" mh
                    , "-o", fname 
                    , colourSet
                    , bg
                    ]

    -- 2. Crop it
    run $ unwords ["convert"
                  , fname
                  , "-crop", cropArgs
                  , name <> "-paom.png"
                  ]


dpi                = 96
fromInches w h     = (dpi * w, dpi * h)
defaultCrop (w, h) = tshow w <> "x" <> tshow h <> "+0+0+0"
ca w h             = defaultCrop (fromInches w h)


tshirt colourSet bg = basic (Just 3360) Nothing (ca 35 20)
                        (colourSet <> "-" <> bg <> "-tee") colourSet bg

pants colourSet bg  = basic Nothing (Just 5000) (ca 35 40)
                        (colourSet <> "-" <> bg <> "-pants") colourSet bg


