#!/usr/bin/env stack
{- stack script
      --resolver lts-12.0
      --package split
-}

import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- lines <$> readFile "x.md"
  let newLines = concat $ zipWith nextLines ("//" : content) content

  putStrLn $ "# fashion"
  putStrLn $ ""
  putStrLn $ "## example images from the code in repo"
  putStrLn $ unlines newLines


nextLines :: String
           -> String
           -> [String]
nextLines a b
  = if matches then [b] else ["", link, "", b]
    where
      a'       = folder a
      b'       = folder b
      matches  = a' == b'
      link     = "#### [" ++ b' ++ "](" ++ b' ++ ")"
      folder x = (splitOn "/" x) !! 1
