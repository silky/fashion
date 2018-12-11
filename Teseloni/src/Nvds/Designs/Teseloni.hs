{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Nvds.Designs.Teseloni where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


d :: Diagram B
d = tsstst
  where
    tsstst = tssts # reflectY === tssts

    tssts  = foldl (\d' (d, s) -> d # scaleY s === d')
              mempty (zip (repeat tsst) [1 - (0.1*s) | s <- [1..7] ])

    tsst   = ts ||| ts # reflectX

    ts     = foldl (\d' (d, s) -> d' ||| d # scaleX s)
              mempty (zip (repeat t) [1 - (0.1*s) | s <- [1..7] ])

    t = polygon ( with
          & polyOrient .~ NoOrient
          & polyType   .~ PolySides
          [ 90 @@ deg , 90 @@ deg ]
          [ 1         , 1         ]
          )
          # fc black
          # reflectX
