{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Types.Common
  ( Block(..)
  ) where

import Data.Aeson ( withObject, (.:), FromJSON(parseJSON) )


data Block = Block
  { blockHeaderHash :: String
  , blockNo :: Integer
  , slotNo :: Integer
  }
  deriving (Show, Eq)

instance FromJSON Block where
  parseJSON = withObject "Block" $ \v -> Block
    <$> v .: "blockHeaderHash"
    <*> v .: "blockNo"
    <*> v .: "slotNo"
