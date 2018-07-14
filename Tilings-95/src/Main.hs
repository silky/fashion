module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Designs.Tilings95

main :: IO ()
-- main = mainWith ( tile red (tile blue mempty # scale 0.4) # frame 0.1 # bg white ) >> putStrLn "Done"
-- main = mainWith ( d 3 # frame 0.1 # bg white ) >> putStrLn "Done"
-- main = mainWith ( circleTile # frame 0.1 # bg white ) >> putStrLn "Done"
main = mainWith ( dd # frame 0.1 # bg white ) >> putStrLn "Done"
-- main = mainWith ( rose # frame 0.1 # bg white ) >> putStrLn "Done"


