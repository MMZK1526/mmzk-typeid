module Main where

import           Data.TypeID
import qualified Data.UUID.V7 as V7
import Control.Exception

main :: IO ()
main = do
  putStrLn "Making a typeid:"
  typeID <- genTypeID "mmzk"
  putStrLn $ toString typeID
  putStrLn "Make 10 typeids in a batch. They are guaranteed to have the same timestamp and of ascending order:"
  typeIDs <- genTypeIDs "mmzk" 10
  mapM_ (putStrLn . toString) typeIDs
  putStrLn "Making a typeid from a String:"
  case parseString "mmzk_01h455vb4pex5vsknk084sn02q" of
    Left err     -> throwIO err
    Right typeID -> putStrLn $ toString typeID
  putStrLn "Making a typeid from a given prefix and a UUID:"
  case decorate "mmzk" V7.nil of
    Left err     -> throwIO err
    Right typeID -> putStrLn $ toString typeID
