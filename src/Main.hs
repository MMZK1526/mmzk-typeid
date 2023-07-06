module Main where

import           Data.TypeID
import Control.Exception

main :: IO ()
main = do
  putStrLn "Making a TypeID:"
  typeID <- genTypeID "mmzk"
  putStrLn $ toString typeID
  putStrLn "Making a TypeID from a String:"
  case parseString "mmzk_01h455vb4pex5vsknk084sn02q" of
    Left err     -> throwIO err
    Right typeID -> putStrLn $ toString typeID
