module Main

where

import Examples
import CoreEval
-- import Core
-- import CoreCheck
-- import Type

main :: IO ()
main = do
  putStrLn "Hi"
  print fac
  -- print $ coreCheck emptyTyVarContext primVarContext fac -- TODO: Implement 'coreCheck' then uncomment this
  putStrLn $ show $ extract $ eval fac
