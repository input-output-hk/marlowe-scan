{-# LANGUAGE OverloadedStrings #-}
module Language.Marlowe.Runtime.Types.General(Range(..), setRangeHeader, parseRangeHeader) where

import Data.ByteString (ByteString)
import Network.HTTP.Client (Request)
import Network.HTTP.Simple (setRequestHeader)

data Range
  = Start
  | Next ByteString
  | Done
  deriving (Eq, Show)

setRangeHeader :: Range -> Request -> Request
setRangeHeader (Next bs) = setRequestHeader "Range" [bs]
setRangeHeader _ = id

parseRangeHeader :: [ByteString] -> Range
parseRangeHeader [bs] = Next bs
parseRangeHeader _ = Done
