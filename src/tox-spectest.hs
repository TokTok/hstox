module Main (main) where

import           Control.Concurrent (threadDelay)
import           System.Environment (getArgs, withArgs)
import           System.Process     (createProcess, proc, terminateProcess)

import qualified TestSuite


getSutAndArgs :: IO (String, [String])
getSutAndArgs = do
  args <- getArgs
  case args of
    []         -> fail "Usage: tox-spectest <sut> [args...]"
    sut : rest -> return (sut, rest)


main :: IO ()
main = do
  (sut, args) <- getSutAndArgs
  (_, _, _, sutProc) <- createProcess $ proc sut []
  -- 100ms delay to give the SUT time to set up its socket before we try to
  -- build connections in the test runner.
  threadDelay $ 100 * 1000
  withArgs (["--print-cpu-time", "--color"] ++ args) TestSuite.main
  terminateProcess sutProc
