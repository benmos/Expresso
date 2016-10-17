module Main

where

import Examples
import CoreEval

import Core
import CoreCheck
import Type

main :: IO ()
main = do
  putStrLn "Term:"
  print fac
  putStrLn "Eval:"
  putStrLn $ show $ extract $ eval fac
  putStrLn "TypeCheck:"
  print $ coreCheck emptyTyVarContext primVarContext fac -- TODO: Implement 'coreCheck' then uncomment this
