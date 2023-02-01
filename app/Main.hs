module Main
  ( main
  )
  where

import Lib (startApp)
import Opts (parseOpts)

main :: IO ()
main = parseOpts >>= startApp
