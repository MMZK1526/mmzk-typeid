{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.KindID (KindID)
import qualified Data.KindID as KID
import qualified Data.Text as T
import           Data.TypeID (TypeID, TypeIDError)
import qualified Data.TypeID as TID
import qualified Data.UUID.V7 as V7
import           GHC.Generics (Generic)
import           Test.Hspec

data TestData = TestData { name        :: String
                         , typeid      :: String
                         , prefix      :: Maybe String
                         , uuid        :: Maybe String }
  deriving (Generic, FromJSON, ToJSON)

anyTypeIDError :: Selector TypeIDError
anyTypeIDError = const True

main :: IO ()
main = do
  invalid <- BSL.readFile "test/invalid.json" >>= throwDecode :: IO [TestData]
  valid   <- BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestData]

  hspec do
    describe "Generate typeid" do
      it "can generate typeid with prefix" do
        tid <- TID.genTypeID "mmzk"
        TID.getPrefix tid `shouldBe` "mmzk"
      it "can generate typeid without prefix" do
        tid <- TID.genTypeID ""
        TID.getPrefix tid `shouldBe` ""
      it "can parse typeid from String" do
        case TID.parseString "mmzk_00041061050r3gg28a1c60t3gf" of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right tid -> pure ()
      it "has the correct nil" do
        Right TID.nil `shouldBe` TID.parseString "00000000000000000000000000"
      it "can generate in batch with same timestamp and in ascending order" do
        tids <- TID.genTypeIDs "mmzk" 1526
        all ((== "mmzk") . TID.getPrefix) tids `shouldBe` True
        let timestamp = TID.getTime $ head tids
        all ((== timestamp) . TID.getTime) tids `shouldBe` True
        all (uncurry (<)) (zip tids $ tail tids) `shouldBe` True

    describe "Parse typeid" do
      let invalidPrefixes = [ ("caps", "PREFIX")
                            , ("numeric", "12323")
                            , ("symbols", "pre.fix")
                            , ("spaces", "  ")
                            , ("long", "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
                            , ("ascii", "château") ]
      describe "can detect invalid prefix" do
        forM_ invalidPrefixes \(reason, prefix) -> it reason do
          TID.genTypeID prefix `shouldThrow` anyTypeIDError
          case TID.decorate prefix V7.nil of
            Left _  -> pure ()
            Right _ -> expectationFailure "Should not be able to decorate with invalid prefix"
      let invalidSuffixes = [ ("spaces", " ")
                            , ("short", "01234")
                            , ("long", "012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
                            , ("caps", "00041061050R3GG28A1C60T3GF") -- Would be valid in lowercase
                            , ("hyphens", "00041061050-3gg28a1-60t3gf")
                            , ("crockford_ambiguous", "ooo41o61o5or3gg28a1c6ot3gi") -- Would be valid if we followed Crockford's substitution rules
                            , ("symbols", "00041061050.3gg28a1_60t3gf")
                            , ("wrong_alphabet", "ooooooiiiiiiuuuuuuulllllll") ]
      describe "can detect invalid suffix" do
        forM_ invalidSuffixes \(reason, suffix) -> it reason do
          case TID.parseStringWithPrefix "mmzk" suffix of
            Left _    -> pure ()
            Right tid -> expectationFailure $ "Parsed typeid: " ++ TID.toString tid

    describe "Parse special values" do
      let specialValues = [ ("nil", "00000000000000000000000000", "00000000-0000-0000-0000-000000000000")
                          , ("one", "00000000000000000000000001", "00000000-0000-0000-0000-000000000001")
                          , ("ten", "0000000000000000000000000a", "00000000-0000-0000-0000-00000000000a")
                          , ("sixteen", "0000000000000000000000000g", "00000000-0000-0000-0000-000000000010")
                          , ("thirty-two", "00000000000000000000000010", "00000000-0000-0000-0000-000000000020") ]
      forM_ specialValues \(reason, typeid, uuid) -> it reason do
        case TID.parseString typeid of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right tid -> V7.toString (TID.getUUID tid) `shouldBe` uuid

    describe "Test invalid.json" do
      forM_ invalid \(TestData name typeid _ _) -> it name do 
        case TID.parseString typeid of
          Left _    -> pure ()
          Right tid -> expectationFailure $ "Parsed typeid: " ++ TID.toString tid

    describe "Test valid.json" do
      forM_ valid \(TestData name typeid (Just prefix) (Just uuid)) -> it name do
        case TID.parseString typeid of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right tid -> do
            TID.getPrefix tid `shouldBe` T.pack prefix
            V7.toString (TID.getUUID tid) `shouldBe` uuid

    describe "Generate type-level typeid" do
      it "can generate typeid with prefix" do
        tid <- KID.genKindID @"mmzk"
        KID.getPrefix tid `shouldBe` "mmzk"
      it "can generate typeid without prefix" do
        tid <- KID.genKindID @""
        KID.getPrefix tid `shouldBe` ""
      it "can parse typeid from String" do
        case KID.parseString @"mmzk" "mmzk_00041061050r3gg28a1c60t3gf" of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right tid -> pure ()
      it "cannot parse typeid into wrong prefix" do
        case KID.parseString @"foo" "mmzk_00041061050r3gg28a1c60t3gf" of
          Left err  -> pure ()
          Right tid -> expectationFailure $ "Parsed typeid: " ++ KID.toString tid
      it "has the correct nil" do
        Right KID.nil `shouldBe` KID.parseString @"" "00000000000000000000000000"
      it "can generate in batch with same timestamp and in ascending order" do
        kids <- KID.genKindIDs @"mmzk" 1526
        all ((== "mmzk") . KID.getPrefix) kids `shouldBe` True
        let timestamp = KID.getTime $ head kids
        all ((== timestamp) . KID.getTime) kids `shouldBe` True
        all (uncurry (<)) (zip kids $ tail kids) `shouldBe` True
