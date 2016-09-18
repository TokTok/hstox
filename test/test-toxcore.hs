module Main (main) where

import           Control.Concurrent  (threadDelay)
import           System.Environment  (getArgs, withArgs)
import           System.Process      (createProcess, proc, terminateProcess)

import           Network.Tox.Testing (serve)
import qualified TestSuite


foreign import ccall test_main :: IO ()


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--sut"] -> test_main
    _ -> do
      (_, _, _, sut) <- createProcess $ proc "dist/build/test-toxcore/test-toxcore" ["--sut"]
      -- 100ms delay to give the SUT time to set up its socket before we try to
      -- build connections in the test runner.
      threadDelay $ 100 * 1000
      withArgs (["--print-cpu-time", "--color"] ++ args) TestSuite.main
      terminateProcess sut
