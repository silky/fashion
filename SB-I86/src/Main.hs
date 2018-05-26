{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (tri')

s = sRGB24read


p as xs = 
    polygon 
        ( with
            & polyOrient .~ NoOrient
            & polyType   .~ PolySides as xs
        )
        # lw 0
        # lc white


diag :: Diagram B
diag = 
    (left # alignT # snugR <> right # alignT # snugL)
        # bg (s "#22274c")
    where
        left  = section (s "#37657a") (s "#2f6baf") (s "#5aafda") (s "#fdde4b")
        right = section (s "#c9452e") (s "#81313a") (s "#d67357") (s "#e09ea6")
                    # reflectY
                    # reflectX

        section c1 c2 c3 c4
          = t1' c1 c2 c3 <> t4 c4

        t1' c1 c2 c3 = (t1 c1 <> t3' c2 c3)
                # alignL
                # snugB

        t4 c = p [ 90 @@ deg , 270 @@ deg , 270 @@ deg     ]
               [ 0         , 4.6        ,  2.7         , 4 ]
               # fc c
               # alignL
               # snugT

        a1 = (atan (0.4/4) * (180 / pi))
        d1 = (sqrt ( 4 ** 2 + 0.4 ** 2 )) / 2 

        t3' c1 c2 = (t3_1 c1 <> t3_2 c2)
                # center
                # snugT

        t3_1 c = p [ (270 + a1) @@ deg , 0 @@ deg ]
                   [ 6.6                 , d1 ]
                   # fc c
                   # alignB
                   # snugR

        t3_2 c = p [ 270 @@ deg , 90 @@ deg , (90 + a1) @@ deg     ]
                   [ 0          , 4         , 7               , d1 ]
                   # fc c
                   # alignB
                   # snugL



        -- top green-ish thing
        t1 c = p [ 90 @@ deg, 90 @@ deg, 90 @@ deg ]
                 [ 0.8        , 4        , 1.2     ]
               # fc c
               # snugB

main :: IO ()
main = mainWith (diag # frame 0.5)
