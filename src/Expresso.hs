module Main

where

import Examples
import CoreEval
import Core
import Type
import Prim
import Utils

main :: IO ()
main = do
  putStrLn "Hi"
  print fac
  putStrLn $ show $ extract $ eval fac
