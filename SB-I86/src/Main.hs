{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Prelude hiding (Left)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (tri')
import Nvds.Colours.ColourSets
import System.Random
import Control.Monad
import Data.List.Split (chunksOf)

s = sRGB24read


p c as xs =
    polygon
        ( with
            & polyOrient .~ NoOrient
            & polyType   .~ PolySides as xs
        )
        # lw 5
        # lc black


main :: IO ()
-- main = mainWith (frame 0.5 <$> diagTiled)
-- main = mainWith (frame 0.5 <$> diag1)
main = mainWith diagTiled
                


diagTiled :: IO (Diagram B)
diagTiled = do
    tiles <- replicateM (1) ( frame 0.5 <$> diag1 )
    -- tiles <- replicateM (16) ( id <$> diag1 )

    let ds = vcat (map hcat (chunksOf 1 tiles))

    return ds


diag1 :: IO (Diagram B)
diag1 = do
    let len = length allColours - 2

    c1 <- randomRIO (0, len)
    c2 <- randomRIO (0, len)
    c3 <- randomRIO (0, len)
    c4 <- randomRIO (0, len)
    c5 <- randomRIO (0, len)
    c6 <- randomRIO (0, len)

    let colourSet1 = take 4 . cycle . fst $ allColours !! c1
        colourSet2 = take 4 . cycle . fst $ allColours !! c2
        colourSet3 = take 4 . cycle . fst $ allColours !! c3
        colourSet4 = take 4 . cycle . fst $ allColours !! c4
        colourSet5 = take 4 . cycle . fst $ allColours !! c5
        colourSet6 = take 4 . cycle . fst $ allColours !! c6

        colouredSection [c1, c2, c3, c4] = section c1 c2 c3 c4

        -- left  = colouredSection [s "#37657a", s "#2f6baf", s "#5aafda", s "#fdde4b"]
        -- right = colouredSection [s "#c9452e", s "#81313a", s "#d67357", s "#e09ea6"]
        --             # reflectY
        --             # reflectX

        -- left  = colouredSection colourSet1
        right = colouredSection colourSet2
                    # reflectY
                    # reflectX

    let d = (
            ( -- ( left # alignT # snugR 
              ( right # alignT # snugL ) 
                # snugR # alignT  <> ( colouredSection colourSet3 # reflectY # snugL # alignT )
            ) # snugB <>
            --
            ( -- ( colouredSection colourSet4 # alignT # snugR 
              ( colouredSection colourSet6 # reflectX # reflectY # alignT # snugL ) 
                # snugR # alignT  <> ( colouredSection colourSet5 # reflectY # snugL # alignT )
            ) # reflectY # snugT
              
            )
            # bg (s "#22274c")
    return ( d # rotateBy (1/4) ) -- # frame 0.5)
    -- return ( d ) -- # frame 0.5)
    where
        section c1 c2 c3 c4
          = t1' c1 c2 c3 <> t4 c4


        t1' c1 c2 c3 = (t1 c1 <> t3' c2 c3)
                # alignL
                # snugB


        t4 c = p c  [ 90 @@ deg , 270 @@ deg , 270 @@ deg    ]
                    [ 0         , 4.6        , 2.7       , 4 ]
               # fc c
               # alignL
               # snugT


        -- Combined blue/blue thing
        t3' c1 c2 = (t3_1 c1 <> t3_2 c2)
                # center
                # snugT


        a1 = (atan (0.4/4) * (180 / pi))
        d  = (sqrt ( 4 ** 2 + 0.4 ** 2 ))
        d1 = d * 0.5
        d2 = d1 - d


        -- Triangle part of blue/blue thing
        t3_1 c = p c [ (270 + a1) @@ deg , mempty ]
                     [ 5.6               , d2     ]
                   # fc c
                   # alignB
                   # snugR


        -- Polygon part of blue/blue thing
        t3_2 c = p c [ 270 @@ deg , 90 @@ deg , (90 + a1) @@ deg     ]
                     [ 0          , 4         , 6               , d1 ]
                   # fc c
                   # alignB
                   # snugL


        -- top green-ish thing
        t1 c = p c [ 90 @@ deg, 90 @@ deg, 90 @@ deg ]
                   [ 0.8        , 4        , 1.2     ]
               # fc c
               # snugB





-- | Creates the basic "Stu Brown" tile that we can repeat indefinitely.
-- baseDiagram :: Colour Double
--             -> Colour Double
--             -> Colour Double
--             -> Colour Double
--             -> Diagram B
-- baseDiagram c1 c2 c3 c4 = t1' c1 c2 c3 <> t4 c4
--     where
--         t1' c1 c2 c3 = (t1 c1 <> t3' c2 c3)


--         t4 c = p c  [ 90 @@ deg , 270 @@ deg , 270 @@ deg  ]
--                     [ 0         , 4.6        , 2.7       , 4 ]
--                # fc c


--         -- Combined blue/blue thing
--         t3' c1 c2 = (t3_1 c1 <> t3_2 c2)
--                 # center


--         a1 = (atan (0.4/4) * (180 / pi))
--         d  = (sqrt ( 4 ** 2 + 0.4 ** 2 ))
--         d1 = d * 0.55
--         d2 = d1 - d


--         -- Triangle part of blue/blue thing
--         t3_1 c = p c [ (270 + a1) @@ deg , 0 @@ deg ]
--                      [ 5.6                 , d1 ]
--                    # fc c


--         -- Polygon part of blue/blue thing
--         t3_2 c = p c [ 270 @@ deg , 90 @@ deg , (90 + a1) @@ deg     ]
--                      [ 0          , 4         , 6               , d1 ]
--                    # fc c


--         -- top green-ish thing
--         t1 c = p c [ 90 @@ deg, 90 @@ deg, 90 @@ deg ]
--                    [ 0.8        , 4        , 1.2     ]
--                # fc c

