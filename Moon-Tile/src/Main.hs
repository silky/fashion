module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Designs.MoonTile

main :: IO ()
-- main = mainWith ( moon # frame 0.1 ) >> putStrLn "Done"
-- main = mainWith ( moonBg # frame 0.1 ) >> putStrLn "Done"
-- main = mainWith ( diamond # frame 0.1 ) >> putStrLn "Done"
-- main = mainWith ( hexDiamond # frame 0.1 ) >> putStrLn "Done"
-- main = mainWith ( sinFunc # frame 2 ) >> putStrLn "Done"
main = mainWith ( tiledMoon 2 # frame 2 ) >> putStrLn "Done"
-- main = mainWith ( waveyThing # frame 2 ) >> putStrLn "Done"


