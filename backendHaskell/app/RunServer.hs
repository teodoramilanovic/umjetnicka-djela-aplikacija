module Main where

import qualified BasicServer as B

main :: IO ()
main = putStrLn "Running Basic Server" >> B.runServer