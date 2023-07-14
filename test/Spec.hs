{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.KindID
import           Data.KindID.Class
import qualified Data.KindID as KID
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.TypeID
import           Data.TypeID.Error
import qualified Data.TypeID as TID
import           Data.UUID.V7 (UUID)
import qualified Data.UUID.V7 as V7
import           GHC.Generics (Generic)
import           Test.Hspec

data TestData = TestData { name   :: String
                         , typeid :: String
                         , prefix :: Maybe Text
                         , uuid   :: Maybe String }
  deriving (Generic, FromJSON, ToJSON)

data TestDataUUID = TestDataUUID { name   :: String
                                 , typeid :: TypeID
                                 , prefix :: Text
                                 , uuid   :: UUID }
  deriving (Generic, FromJSON, ToJSON)

data Prefix = User | Post | Comment

instance ToPrefix 'User where
  type PrefixSymbol 'User = "user"

instance ToPrefix 'Post where
  type PrefixSymbol 'Post = "post"

instance ToPrefix 'Comment where
  type PrefixSymbol 'Comment = "comment"

anyTypeIDError :: Selector TypeIDError
anyTypeIDError = const True

main :: IO ()
main = do
  invalid   <- BSL.readFile "test/invalid.json" >>= throwDecode :: IO [TestData]
  valid     <- BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestData]
  validUUID <- BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestDataUUID]

  hspec do
    describe "Generate TypeID" do
      it "can generate TypeID with prefix" do
        tid <- TID.genTypeID "mmzk"
        getPrefix tid `shouldBe` "mmzk"
      it "can generate TypeID without prefix" do
        tid <- TID.genTypeID ""
        getPrefix tid `shouldBe` ""
      it "can parse TypeID from String" do
        case string2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right tid -> getPrefix tid `shouldBe` "mmzk"
      it "has the correct nil" do
        Right TID.nilTypeID `shouldBe` string2ID "00000000000000000000000000"
      it "can generate in batch with same timestamp and in ascending order" do
        tids <- TID.genTypeIDs "mmzk" 1526
        all ((== "mmzk") . getPrefix) tids `shouldBe` True
        let timestamp = getTime $ head tids
        all ((== timestamp) . getTime) tids `shouldBe` True
        all (uncurry (<)) (zip tids $ tail tids) `shouldBe` True

    describe "Parse TypeID" do
      let invalidPrefixes = [ ("caps", "PREFIX")
                            , ("numeric", "12323")
                            , ("symbols", "pre.fix")
                            , ("spaces", "  ")
                            , ("long", "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
                            , ("ascii", "château") ]
      describe "can detect invalid prefix" do
        forM_ invalidPrefixes \(reason, prefix) -> it reason do
          TID.genTypeID prefix `shouldThrow` anyTypeIDError
          case TID.decorateTypeID prefix V7.nil of
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
          case string2ID @TypeID suffix of
            Left _    -> pure ()
            Right tid -> expectationFailure $ "Parsed TypeID: " ++ show tid

    describe "Parse special values" do
      let specialValues = [ ("nil", "00000000000000000000000000", "00000000-0000-0000-0000-000000000000")
                          , ("one", "00000000000000000000000001", "00000000-0000-0000-0000-000000000001")
                          , ("ten", "0000000000000000000000000a", "00000000-0000-0000-0000-00000000000a")
                          , ("sixteen", "0000000000000000000000000g", "00000000-0000-0000-0000-000000000010")
                          , ("thirty-two", "00000000000000000000000010", "00000000-0000-0000-0000-000000000020") ]
      forM_ specialValues \(reason, tid, uuid) -> it reason do
        case string2ID @TypeID tid of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right tid -> show (KID.getUUID tid) `shouldBe` uuid

    describe "TypeID valid JSON instances" do
      it "Decode and then encode should be identity" do
        tid  <- TID.genTypeID "mmzk"
        tid' <- TID.genTypeID "foo"
        let mapping = M.fromList [(tid, tid')]
        let json    = encode mapping
        decode json `shouldBe` Just mapping
        fmap encode (decode json :: Maybe (Map TypeID TypeID)) `shouldBe` Just json
      describe "Valid JSON value" do
        forM_ valid \(TestData name tid (Just prefix) (Just uuid)) -> it name do
          case decode (fromString $ show tid) :: Maybe TypeID of
            Nothing  -> expectationFailure "Parse JSON failed!"
            Just tid -> do
              getPrefix tid `shouldBe` prefix
              show (KID.getUUID tid) `shouldBe` uuid
      describe "Valid JSON key" do
        forM_ valid \(TestData name tid (Just prefix) (Just uuid)) -> it name do
          case decode (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") :: Maybe (Map TypeID Int) of
            Nothing  -> expectationFailure "Parse JSON failed!"
            Just tid -> do
              let (tid', _) = M.elemAt 0 tid
              getPrefix tid' `shouldBe` prefix
              show (KID.getUUID tid') `shouldBe` uuid

    describe "TypeID invalid JSON instances" do
      describe "Invalid JSON value" do
        forM_ invalid \(TestData name tid _ _) -> it name do 
          case decode (fromString $ show tid) :: Maybe TypeID of
            Nothing  -> pure ()
            Just tid -> expectationFailure $ "Parsed TypeID: " ++ show tid
      describe "Invalid JSON key" do
        forM_ invalid \(TestData name tid _ _) -> it name do 
          case decode (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") :: Maybe (Map TypeID Int) of
            Nothing  -> pure ()
            Just tid -> expectationFailure "Invalid TypeID key shouldn't be parsed!"

    describe "Test invalid.json" do
      forM_ invalid \(TestData name tid _ _) -> it name do 
        case string2ID @TypeID tid of
          Left _    -> pure ()
          Right tid -> expectationFailure $ "Parsed TypeID: " ++ show tid

    describe "Test valid.json (TypeID as literal)" do
      forM_ valid \(TestData name tid (Just prefix) (Just uuid)) -> it name do
        case string2ID @TypeID tid of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right tid -> do
            getPrefix tid `shouldBe` prefix
            show (KID.getUUID tid) `shouldBe` uuid

    describe "Test valid.json (TypeID as JSON)" do
      forM_ validUUID \(TestDataUUID name tid prefix uuid) -> it name do
        getPrefix tid `shouldBe` prefix
        KID.getUUID tid `shouldBe` uuid

    describe "Generate type-level TypeID with 'Symbol' prefixes" do
      it "can generate TypeID with prefix" do
        kid <- KID.genKindID @"mmzk"
        getPrefix kid `shouldBe` "mmzk"
      it "can generate TypeID without prefix" do
        kid <- KID.genKindID @""
        getPrefix kid `shouldBe` ""
      it "can parse TypeID from String" do
        case string2ID @(KindID "mmzk") "mmzk_00041061050r3gg28a1c60t3gf" of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right kid -> getPrefix kid `shouldBe` "mmzk"
      it "cannot parse TypeID into wrong prefix" do
        case string2ID @(KindID "foo") "mmzk_00041061050r3gg28a1c60t3gf" of
          Left err  -> pure ()
          Right kid -> expectationFailure $ "Parsed TypeID: " ++ show kid
      it "has the correct nil" do
        Right KID.nilKindID `shouldBe` string2ID "00000000000000000000000000"
      it "can generate in batch with same timestamp and in ascending order" do
        kids <- KID.genKindIDs @"mmzk" 1526
        all ((== "mmzk") . getPrefix) kids `shouldBe` True
        let timestamp = getTime $ head kids
        all ((== timestamp) . getTime) kids `shouldBe` True
        all (uncurry (<)) (zip kids $ tail kids) `shouldBe` True
  
    describe "Generate type-level TypeID with custom data kind prefixes" do
      it "can generate TypeID with prefix" do
          kid <- KID.genKindID @'Post
          getPrefix kid `shouldBe` "post"
      it "can parse TypeID from String" do
        case string2ID @(KindID User) "user_00041061050r3gg28a1c60t3gf" of
          Left err  -> expectationFailure $ "Parse error: " ++ show err
          Right kid -> getPrefix kid `shouldBe` "user"
      it "cannot parse TypeID into wrong prefix" do
        case string2ID @(KindID Comment) "user_00041061050r3gg28a1c60t3gf" of
          Left err  -> pure ()
          Right kid -> expectationFailure $ "Parsed TypeID: " ++ show kid
      it "can generate in batch with same timestamp and in ascending order" do
        kids <- KID.genKindIDs @'Comment 1526
        all ((== "comment") . getPrefix) kids `shouldBe` True
        let timestamp = getTime $ head kids
        all ((== timestamp) . getTime) kids `shouldBe` True
        all (uncurry (<)) (zip kids $ tail kids) `shouldBe` True
