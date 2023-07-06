module Main where

import           Data.UUID.V7
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Binary.Put
import           Data.Binary.Get
import           Data.Int

main :: IO ()
main = do
  timestamp <- getEpochMilli
  print timestamp
