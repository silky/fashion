{-# LANGUAGE FlexibleContexts   #-}

-- Run like:
--
--  > stack run -- -- --loop -w 200 -s src/Main.hs -o a.svg
--
module Nvds.Designs.Cylinders where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

-- TODO: Figure out why I need 3 arcs.
ss :: Bool -> Diagram B
ss b = 
    (
        (arc d a1)
    <>
        (arc d1 a) 
    <>
        maybeDashed
    )
   # scaleX 0.7
   # scaleY 0.25
       where
           -- Cleanup
           maybeDashed = if b then arc d1 a1 else arc d1 a1 # dashing [3,3] 0
           d           = rotateBy (0) xDir
           a           =  pi @@ rad
           a1          = -pi @@ rad
           d1          = rotateBy (1/2) xDir


 
basic :: Diagram B
basic = (
        ((
            cyl # dashing [] 0 # lw 1 # lc black)  # centerXY 
            # linkDiag          "Top" "Bottom" # lw 0.8 # dashing [2,2] 0)  # lc magenta
            # linkCenter        "Top" "Bottom" # lw 0.4 # dashing [2,2] 0   # lc blue
            # linkAdditional    "Top" "Bottom" # lw 0.4 # lc blue

        <> cyld # scale 0.7
        <> cyld # scale 0.4
        <> cyld # scale 0.1
        )
        # frame 0.5
            where
                cyld = cyl # dashing [4,4] 0
                           # centerXY
                           # lc red
                           # lw 0.6

-- TODO: Unify all the linking things.
linkDiag n1 n2 = 
    withName n1 $ \b1 ->
    withName n2 $ \b2 ->
        atop $
            let lb1 = boundaryFrom b1 unit_X
                lb2 = boundaryFrom b2 unit_X
                lb3 = boundaryFrom b1 (-unit_X)
                lb4 = boundaryFrom b2 (-unit_X)
             in (lb1 ~~ lb4) <> (lb2 ~~ lb3)

linkCenter n1 n2 = 
    withName n1 $ \b1 ->
    withName n2 $ \b2 ->
        atop $
            (location b1 ~~ location b2)

linkAdditional n1 n2 = 
    withName n1 $ \b1 ->
    withName n2 $ \b2 ->
        atop $
           (location b1 ~~ (location b1 .+^ r2 (0, 0.5)))
           <> (location b2 ~~ (location b2 .+^ r2 (0, -0.5)))

link n1 n2 = 
    withName n1 $ \b1 ->
    withName n2 $ \b2 ->
        atop $
            let lb1 = boundaryFrom b1 unit_X
                lb2 = boundaryFrom b2 unit_X
                lb3 = boundaryFrom b1 (-unit_X)
                lb4 = boundaryFrom b2 (-unit_X)
             in    (lb1 ~~ lb2) <> (lb3 ~~ lb4)



cyl = (vsep 1 ds # link "Top" "Bottom")
    where
        ds = [ ss True  # named "Top"
             , mempty === ss False # named "Bottom"
             ]

