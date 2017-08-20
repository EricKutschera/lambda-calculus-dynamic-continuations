module Spec where

import Tests (allTests)

main :: IO ()
main = do putStrLn ""
          putStrLn "Running Tests"
          putStrLn "============="
          mapM_ (putStrLn . showTR) allTests
          putStrLn ""

type TestResult = (String, [Bool])

showTR :: TestResult -> String
showTR (name, results) = name ++ ": " ++ correct ++ "/" ++ total ++ extra
  where correct = show $ length $ filter id results
        total = show $ length results
        extra = if total /= correct then ": " ++ (show results) else ""
