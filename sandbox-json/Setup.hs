module Main (main) where

import Distribution.Simple
import System.Cmd (system)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {runTests = runMyTests}

runMyTests a b pd lb = system "runhaskell ./UnitTest.hs" >> return ()
