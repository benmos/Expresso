module Main

where

import Examples
import CoreEval
-- import Core
-- import CoreCheck

main :: IO ()
main = do
  putStrLn "Hi"
  print fac
  -- print $ coreCheck primKindEnv primTypeEnv fac -- TODO: Implement 'coreCheck' then uncomment this
  putStrLn $ show $ extract $ eval fac
