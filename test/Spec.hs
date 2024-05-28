{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

import           Control.Monad
import           Data.Aeson
import           Data.Binary (get, put)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BSL
import           Data.KindID.V1 (KindIDV1)
import           Data.KindID.V4 (KindIDV4)
import           Data.KindID.V5 (KindIDV5)
import           Data.KindID
import           Data.KindID.Class
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.TypeID
import           Data.TypeID.V1 (TypeIDV1)
import           Data.TypeID.V4 (TypeIDV4)
import           Data.TypeID.V5 (TypeIDV5)
import           Data.TypeID.Class
import           Data.TypeID.Error
import           Data.UUID.Types (nil)
import           Data.UUID.V4 (nextRandom)
import           Data.UUID.V7 (UUID)
import qualified Data.UUID.V7 as V7
import           Foreign
import           GHC.Generics (Generic)
import           Test.Hspec

data TestData = TestData { name   :: String
                         , typeid :: String
                         , prefix :: Maybe Text
                         , uuid   :: Maybe String }
  deriving (Generic, FromJSON, ToJSON)

data TestDataUUID tid = TestDataUUID { name   :: String
                                     , typeid :: tid
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

withCheck :: HasCallStack => (IDConv a, IDGen a) => IO a -> IO a
withCheck action = do
  tid         <- action
  checkResult <- checkIDWithEnv tid
  case checkResult of
    Just _  -> do
      expectationFailure $ concat ["The ID ", id2String tid, " does not pass validity check"]
      undefined
    Nothing -> pure tid

withChecks :: HasCallStack => IDGen a => IO [a] -> IO [a]
withChecks action = do
  tids        <- action
  checkResult <- msum <$> mapM checkIDWithEnv tids
  case checkResult of
    Just _  -> expectationFailure "The IDs do not pass validity check" >> undefined
    Nothing -> pure tids

main :: IO ()
main = hspec do
  v7Test
  v1Test
  v4Test
  v5Test

v7Test :: Spec
v7Test = do
  invalid   <- runIO (BSL.readFile "test/invalid.json" >>= throwDecode :: IO [TestData])
  valid     <- runIO (BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestData])
  validUUID <- runIO (BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestDataUUID TypeID])
  describe "Generate TypeID" do
    it "can generate TypeID with prefix" do
      start <- V7.getEpochMilli
      tid   <- withCheck $ genID @TypeID "mmzk"
      getPrefix tid `shouldBe` "mmzk"
      getTime tid `shouldSatisfy` \t -> t >= start
    it "can generate TypeID without prefix" do
      start <- V7.getEpochMilli
      tid   <- withCheck $ genID @TypeID ""
      getPrefix tid `shouldBe` ""
      getTime tid `shouldSatisfy` \t -> t >= start
    it "can generate TypeID with stateless UUIDv7" do
      start <- V7.getEpochMilli
      tid   <- withCheck $ genID' @TypeID "mmzk"
      getPrefix tid `shouldBe` "mmzk"
      getTime tid `shouldSatisfy` \t -> t >= start
    it "can generate in batch with same timestamp and in ascending order" do
      start <- V7.getEpochMilli
      tids  <- withChecks $ genIDs @TypeID "mmzk" 32768
      all ((== "mmzk") . getPrefix) tids `shouldBe` True
      let timestamp = getTime $ head tids
      all ((== timestamp) . getTime) tids `shouldBe` True
      all (uncurry (<)) (zip tids $ tail tids) `shouldBe` True
      timestamp `shouldSatisfy` \t -> t >= start
    it "can parse TypeID from String" do
      case string2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeID from Text" do
      case text2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeID from ByteString" do
      case byteString2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"

  describe "Parse TypeID" do
    let invalidPrefixes = [ ("caps", "PREFIX")
                          , ("numeric", "12323")
                          , ("symbols", "pre.fix")
                          , ("spaces", "  ")
                          , ("long", "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
                          , ("ascii", "château") ]
    describe "can detect invalid prefix" do
      forM_ invalidPrefixes \(reason, pref) -> it reason do
        genID @TypeID pref `shouldThrow` anyTypeIDError
        case decorate @TypeID pref nil of
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
    forM_ specialValues \(reason, tid, uid) -> it reason do
      case string2ID @TypeID tid of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right t  -> show (getUUID t) `shouldBe` uid

  describe "TypeID valid JSON instances" do
    it "Decode and then encode should be identity" do
      tid  <- genID @TypeID "mmzk"
      tid' <- genID @TypeID "foo"
      let mapping = M.fromList [(tid, tid')]
      let tJson   = encode mapping
      decode tJson `shouldBe` Just mapping
      fmap encode (decode @(Map TypeID TypeID) tJson) `shouldBe` Just tJson
    describe "Valid JSON value" do
      forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
        case decode @TypeID (fromString $ show tid) of
          Nothing -> expectationFailure "Parse JSON failed!"
          Just t  -> do
            getPrefix t `shouldBe` pref
            show (getUUID t) `shouldBe` uid
            Right t `shouldBe` string2ID tid
            Right t `shouldBe` text2ID (T.pack tid)
            Right t `shouldBe` byteString2ID (BSL.fromStrict . encodeUtf8 $ T.pack tid)
    describe "Valid JSON key" do
      forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
        case decode @(Map TypeID Int) (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") of
          Nothing      -> expectationFailure "Parse JSON failed!"
          Just mapping -> do
            let (t, _) = M.elemAt 0 mapping
            getPrefix t `shouldBe` pref
            show (getUUID t) `shouldBe` uid

  describe "TypeID invalid JSON instances" do
    describe "Invalid JSON value" do
      forM_ invalid \(TestData n tid _ _) -> it n do
        case decode @TypeID (fromString $ show tid) of
          Nothing -> pure ()
          Just t  -> expectationFailure $ "Parsed TypeID: " ++ show t
    describe "Invalid JSON key" do
      forM_ invalid \(TestData n tid _ _) -> it n do
        case decode @(Map TypeID Int) (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") of
          Nothing -> pure ()
          _       -> expectationFailure "Invalid TypeID key shouldn't be parsed!"

  describe "Test invalid.json" do
    forM_ invalid \(TestData n tid _ _) -> it n do
      case string2ID @TypeID tid of
        Left _  -> pure ()
        Right t -> expectationFailure $ "Parsed TypeID: " ++ show t

  describe "Test valid.json (TypeID as literal)" do
    forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
      case string2ID @TypeID tid of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right t  -> do
          getPrefix t `shouldBe` pref
          show (getUUID t) `shouldBe` uid

  describe "Test valid.json (TypeID as JSON)" do
    forM_ validUUID \(TestDataUUID n tid pref uid) -> it n do
      getPrefix tid `shouldBe` pref
      getUUID tid `shouldBe` uid

  describe "Generate KindID with 'Symbol' prefixes" do
    it "can generate KindID with prefix" do
      start <- V7.getEpochMilli
      kid   <- withCheck $ genID @(KindID "mmzk")
      getPrefix kid `shouldBe` "mmzk"
      getTime kid `shouldSatisfy` \t -> start <= t
    it "can generate KindID without prefix" do
      start <- V7.getEpochMilli
      kid   <- withCheck $ genID @(KindID "")
      getPrefix kid `shouldBe` ""
      getTime kid `shouldSatisfy` \t -> start <= t
    it "can generate KindID with stateless UUIDv7" do
      start <- V7.getEpochMilli
      kid   <- withCheck $ genID' @(KindID "mmzk")
      getPrefix kid `shouldBe` "mmzk"
      getTime kid `shouldSatisfy` \t -> start <= t
    it "can generate in batch with same timestamp and in ascending order" do
      start <- V7.getEpochMilli
      kids  <- withChecks $ genIDs @(KindID "mmzk") 32768
      all ((== "mmzk") . getPrefix) kids `shouldBe` True
      let timestamp = getTime $ head kids
      all ((== timestamp) . getTime) kids `shouldBe` True
      all (uncurry (<)) (zip kids $ tail kids) `shouldBe` True
      timestamp `shouldSatisfy` \t -> start <= t
    it "can parse KindID from String" do
      case string2ID @(KindID "mmzk") "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right kid -> getPrefix kid `shouldBe` "mmzk"
    it "cannot parse KindID into wrong prefix" do
      case string2ID @(KindID "foo") "mmzk_00041061050r3gg28a1c60t3gf" of
        Left _    -> pure ()
        Right kid -> expectationFailure $ "Parsed TypeID: " ++ show kid

  describe "Generate KindID with custom data kind prefixes" do
    it "can generate KindID with prefix" do
        kid <- withCheck $ genID @(KindID 'Post)
        getPrefix kid `shouldBe` "post"
    it "can parse KindID from String" do
      case string2ID @(KindID 'User) "user_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right kid -> getPrefix kid `shouldBe` "user"
    it "cannot parse KindID into wrong prefix" do
      case string2ID @(KindID 'Comment) "user_00041061050r3gg28a1c60t3gf" of
        Left _    -> pure ()
        Right kid -> expectationFailure $ "Parsed KindID: " ++ show kid
    it "can generate in batch with same timestamp and in ascending order" do
      kids <- withChecks $ genIDs @(KindID 'Comment) 32768
      all ((== "comment") . getPrefix) kids `shouldBe` True
      let timestamp = getTime $ head kids
      all ((== timestamp) . getTime) kids `shouldBe` True
      all (uncurry (<)) (zip kids $ tail kids) `shouldBe` True

  describe "Binary instance for TypeID and KindID" do
    it "has correct binary instance for TypeID" do
      tids <- withChecks $ genIDs @TypeID "abcdefghijklmnopqrstuvwxyz" 114
      forM_ tids \tid -> do
        let bytes = runPut (put tid)
        let tid'  = runGet get bytes
        tid' `shouldBe` tid
    it "has correct binary instance for KindID" do
      kids <- withChecks $ genIDs @(KindID "abcdefghijklmnopqrstuvwxyz") 114
      forM_ kids \kid -> do
        let bytes = runPut (put kid)
        let kid'  = runGet get bytes
        kid' `shouldBe` kid

  describe "Storable instance for TypeID and KindID" do
    it "has correct Storable instance for TypeID" do
      tids <- withChecks $ genIDs @TypeID "abcdefghijklmnopqrstuvwxyz" 114
      forM_ tids \tid -> do
        ptr   <- new tid
        tid'  <- peek ptr
        tid' `shouldBe` tid
        poke ptr tid'
        tid'' <- peek ptr
        tid'' `shouldBe` tid'
        free ptr
    it "has correct Storable instance for KindID" do
      kids <- withChecks $ genIDs @(KindID "abcdefghijklmnopqrstuvwxyz") 114
      forM_ kids \kid -> do
        ptr   <- new kid
        kid'  <- peek ptr
        kid' `shouldBe` kid
        poke ptr kid'
        kid'' <- peek ptr
        kid'' `shouldBe` kid'
        free ptr

v1Test :: Spec
v1Test = do
  invalid   <- runIO (BSL.readFile "test/invalid.json" >>= throwDecode :: IO [TestData])
  valid     <- runIO (BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestData])
  validUUID <- runIO (BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestDataUUID TypeIDV1])
  describe "Generate TypeIDV1" do
    it "can generate TypeIDV1 with prefix" do
      tid <- withCheck $ genID @TypeIDV1 "mmzk"
      getPrefix tid `shouldBe` "mmzk"
    it "can generate TypeIDV1 without prefix" do
      tid <- withCheck $ genID @TypeIDV1 ""
      getPrefix tid `shouldBe` ""
    it "can generate TypeIDV1 with insecure UUIDv4" do
      tid   <- withCheck $ genID' @TypeIDV1 "mmzk"
      getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeIDV1 from String" do
      case string2ID @TypeIDV1 "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeIDV1 from Text" do
      case text2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeIDV1 from ByteString" do
      case byteString2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"

  describe "Parse TypeIDV1" do
    let invalidPrefixes = [ ("caps", "PREFIX")
                          , ("numeric", "12323")
                          , ("symbols", "pre.fix")
                          , ("spaces", "  ")
                          , ("long", "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
                          , ("ascii", "château") ]
    describe "can detect invalid prefix" do
      forM_ invalidPrefixes \(reason, pref) -> it reason do
        genID @TypeIDV1 pref `shouldThrow` anyTypeIDError
        case decorate @TypeIDV1 pref nil of
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
        case string2ID @TypeIDV1 suffix of
          Left _    -> pure ()
          Right tid -> expectationFailure $ "Parsed TypeID: " ++ show tid

  describe "Parse special values" do
    let specialValues = [ ("nil", "00000000000000000000000000", "00000000-0000-0000-0000-000000000000")
                        , ("one", "00000000000000000000000001", "00000000-0000-0000-0000-000000000001")
                        , ("ten", "0000000000000000000000000a", "00000000-0000-0000-0000-00000000000a")
                        , ("sixteen", "0000000000000000000000000g", "00000000-0000-0000-0000-000000000010")
                        , ("thirty-two", "00000000000000000000000010", "00000000-0000-0000-0000-000000000020") ]
    forM_ specialValues \(reason, tid, uid) -> it reason do
      case string2ID @TypeIDV1 tid of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right t  -> show (getUUID t) `shouldBe` uid

  describe "TypeIDV1 valid JSON instances" do
    it "Decode and then encode should be identity" do
      tid  <- genID @TypeIDV1 "mmzk"
      tid' <- genID @TypeIDV1 "foo"
      let mapping = M.fromList [(tid, tid')]
      let tJson   = encode mapping
      decode tJson `shouldBe` Just mapping
      fmap encode (decode @(Map TypeIDV1 TypeIDV1) tJson) `shouldBe` Just tJson
    describe "Valid JSON value" do
      forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
        case decode @TypeIDV1 (fromString $ show tid) of
          Nothing  -> expectationFailure "Parse JSON failed!"
          Just t -> do
            getPrefix t `shouldBe` pref
            show (getUUID t) `shouldBe` uid
            Right t `shouldBe` string2ID tid
            Right t `shouldBe` text2ID (T.pack tid)
            Right t `shouldBe` byteString2ID (BSL.fromStrict . encodeUtf8 $ T.pack tid)
    describe "Valid JSON key" do
      forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
        case decode @(Map TypeIDV1 Int) (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") of
          Nothing      -> expectationFailure "Parse JSON failed!"
          Just mapping -> do
            let (t, _) = M.elemAt 0 mapping
            getPrefix t `shouldBe` pref
            show (getUUID t) `shouldBe` uid

  describe "TypeIDV1 invalid JSON instances" do
    describe "Invalid JSON value" do
      forM_ invalid \(TestData n tid _ _) -> it n do
        case decode @TypeIDV1 (fromString $ show tid) of
          Nothing -> pure ()
          Just t  -> expectationFailure $ "Parsed TypeID: " ++ show t
    describe "Invalid JSON key" do
      forM_ invalid \(TestData n tid _ _) -> it n do
        case decode @(Map TypeIDV1 Int) (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") of
          Nothing  -> pure ()
          _        -> expectationFailure "Invalid TypeID key shouldn't be parsed!"

  describe "Test invalid.json" do
    forM_ invalid \(TestData n tid _ _) -> it n do
      case string2ID @TypeIDV1 tid of
        Left _  -> pure ()
        Right t -> expectationFailure $ "Parsed TypeID: " ++ show t

  describe "Test valid.json (TypeIDV1 as literal)" do
    forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
      case string2ID @TypeIDV1 tid of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right t  -> do
          getPrefix t `shouldBe` pref
          show (getUUID t) `shouldBe` uid

  describe "Test valid.json (TypeIDV1 as JSON)" do
    forM_ validUUID \(TestDataUUID n tid pref uid) -> it n do
      getPrefix tid `shouldBe` pref
      getUUID tid `shouldBe` uid

  describe "Generate KindIDV1 with 'Symbol' prefixes" do
    it "can generate KindIDV1 with prefix" do
      kid <- withCheck $ genID @(KindIDV1 "mmzk")
      getPrefix kid `shouldBe` "mmzk"
    it "can generate KindIDV1 without prefix" do
      kid <- withCheck $ genID @(KindIDV1 "")
      getPrefix kid `shouldBe` ""
    it "can parse KindID from String" do
      case string2ID @(KindIDV1 "mmzk") "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right kid -> getPrefix kid `shouldBe` "mmzk"
    it "cannot parse KindID into wrong prefix" do
      case string2ID @(KindIDV1 "foo") "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left _    -> pure ()
        Right kid -> expectationFailure $ "Parsed TypeID: " ++ show kid

  describe "Generate KindIDV1 with custom data kind prefixes" do
    it "can generate KindIDV1 with prefix" do
        kid <- withCheck $ genID @(KindID 'Post)
        getPrefix kid `shouldBe` "post"
    it "can parse KindIDV1 from String" do
      case string2ID @(KindIDV1 'User) "user_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right kid -> getPrefix kid `shouldBe` "user"
    it "cannot parse KindIDV1 into wrong prefix" do
      case string2ID @(KindIDV1 'Comment) "user_00041061050r3gg28a1c60t3gf" of
        Left _    -> pure ()
        Right kid -> expectationFailure $ "Parsed KindIDV1: " ++ show kid

  describe "Binary instance for TypeIDV1 and KindIDV1" do
    it "has correct binary instance for TypeIDV1" do
      tids <- withChecks $ genIDs @TypeIDV1 "abcdefghijklmnopqrstuvwxyz" 114
      forM_ tids \tid -> do
        let bytes = runPut (put tid)
        let tid'  = runGet get bytes
        tid' `shouldBe` tid
    it "has correct binary instance for KindIDV1" do
      kids <- withChecks $ genIDs @(KindIDV1 "abcdefghijklmnopqrstuvwxyz") 114
      forM_ kids \kid -> do
        let bytes = runPut (put kid)
        let kid'  = runGet get bytes
        kid' `shouldBe` kid

  describe "Storable instance for TypeIDV1 and KindIDV1" do
    it "has correct Storable instance for TypeIDV1" do
      tids <- withChecks $ genIDs @TypeIDV1 "abcdefghijklmnopqrstuvwxyz" 114
      forM_ tids \tid -> do
        ptr   <- new tid
        tid'  <- peek ptr
        tid' `shouldBe` tid
        poke ptr tid'
        tid'' <- peek ptr
        tid'' `shouldBe` tid'
        free ptr
    it "has correct Storable instance for KindIDV1" do
      kids <- withChecks $ genIDs @(KindIDV1 "abcdefghijklmnopqrstuvwxyz") 114
      forM_ kids \kid -> do
        ptr   <- new kid
        kid'  <- peek ptr
        kid' `shouldBe` kid
        poke ptr kid'
        kid'' <- peek ptr
        kid'' `shouldBe` kid'
        free ptr

v4Test :: Spec
v4Test = do
  invalid   <- runIO (BSL.readFile "test/invalid.json" >>= throwDecode :: IO [TestData])
  valid     <- runIO (BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestData])
  validUUID <- runIO (BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestDataUUID TypeIDV4])
  describe "Generate TypeIDV4" do
    it "can generate TypeIDV4 with prefix" do
      tid <- withCheck $ genID @TypeIDV4 "mmzk"
      getPrefix tid `shouldBe` "mmzk"
    it "can generate TypeIDV4 without prefix" do
      tid <- withCheck $ genID @TypeIDV4 ""
      getPrefix tid `shouldBe` ""
    it "can generate TypeIDV4 with insecure UUIDv4" do
      tid   <- withCheck $ genID' @TypeIDV4 "mmzk"
      getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeIDV4 from String" do
      case string2ID @TypeIDV4 "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeIDV4 from Text" do
      case text2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeIDV4 from ByteString" do
      case byteString2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"

  describe "Parse TypeIDV4" do
    let invalidPrefixes = [ ("caps", "PREFIX")
                          , ("numeric", "12323")
                          , ("symbols", "pre.fix")
                          , ("spaces", "  ")
                          , ("long", "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
                          , ("ascii", "château") ]
    describe "can detect invalid prefix" do
      forM_ invalidPrefixes \(reason, pref) -> it reason do
        genID @TypeIDV4 pref `shouldThrow` anyTypeIDError
        case decorate @TypeIDV4 pref nil of
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
        case string2ID @TypeIDV4 suffix of
          Left _    -> pure ()
          Right tid -> expectationFailure $ "Parsed TypeID: " ++ show tid

  describe "Parse special values" do
    let specialValues = [ ("nil", "00000000000000000000000000", "00000000-0000-0000-0000-000000000000")
                        , ("one", "00000000000000000000000001", "00000000-0000-0000-0000-000000000001")
                        , ("ten", "0000000000000000000000000a", "00000000-0000-0000-0000-00000000000a")
                        , ("sixteen", "0000000000000000000000000g", "00000000-0000-0000-0000-000000000010")
                        , ("thirty-two", "00000000000000000000000010", "00000000-0000-0000-0000-000000000020") ]
    forM_ specialValues \(reason, tid, uid) -> it reason do
      case string2ID @TypeIDV4 tid of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right t  -> show (getUUID t) `shouldBe` uid

  describe "TypeIDV4 valid JSON instances" do
    it "Decode and then encode should be identity" do
      tid  <- genID @TypeIDV4 "mmzk"
      tid' <- genID @TypeIDV4 "foo"
      let mapping = M.fromList [(tid, tid')]
      let tJson   = encode mapping
      decode tJson `shouldBe` Just mapping
      fmap encode (decode @(Map TypeIDV4 TypeIDV4) tJson) `shouldBe` Just tJson
    describe "Valid JSON value" do
      forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
        case decode @TypeIDV4 (fromString $ show tid) of
          Nothing -> expectationFailure "Parse JSON failed!"
          Just t  -> do
            getPrefix t `shouldBe` pref
            show (getUUID t) `shouldBe` uid
            Right t `shouldBe` string2ID tid
            Right t `shouldBe` text2ID (T.pack tid)
            Right t `shouldBe` byteString2ID (BSL.fromStrict . encodeUtf8 $ T.pack tid)
    describe "Valid JSON key" do
      forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
        case decode @(Map TypeIDV4 Int) (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") of
          Nothing      -> expectationFailure "Parse JSON failed!"
          Just mapping -> do
            let (t, _) = M.elemAt 0 mapping
            getPrefix t `shouldBe` pref
            show (getUUID t) `shouldBe` uid

  describe "TypeIDV4 invalid JSON instances" do
    describe "Invalid JSON value" do
      forM_ invalid \(TestData n tid _ _) -> it n do
        case decode @TypeIDV4 (fromString $ show tid) of
          Nothing -> pure ()
          Just t  -> expectationFailure $ "Parsed TypeID: " ++ show t
    describe "Invalid JSON key" do
      forM_ invalid \(TestData n tid _ _) -> it n do
        case decode @(Map TypeIDV4 Int) (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") of
          Nothing -> pure ()
          _       -> expectationFailure "Invalid TypeID key shouldn't be parsed!"

  describe "Test invalid.json" do
    forM_ invalid \(TestData n tid _ _) -> it n do
      case string2ID @TypeIDV4 tid of
        Left _  -> pure ()
        Right t -> expectationFailure $ "Parsed TypeID: " ++ show t

  describe "Test valid.json (TypeIDV4 as literal)" do
    forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
      case string2ID @TypeIDV4 tid of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right t  -> do
          getPrefix t `shouldBe` pref
          show (getUUID t) `shouldBe` uid

  describe "Test valid.json (TypeIDV4 as JSON)" do
    forM_ validUUID \(TestDataUUID n tid pref uid) -> it n do
      getPrefix tid `shouldBe` pref
      getUUID tid `shouldBe` uid

  describe "Generate KindIDV4 with 'Symbol' prefixes" do
    it "can generate KindIDV4 with prefix" do
      kid <- withCheck $ genID @(KindIDV4 "mmzk")
      getPrefix kid `shouldBe` "mmzk"
    it "can generate KindIDV4 without prefix" do
      kid <- withCheck $ genID @(KindIDV4 "")
      getPrefix kid `shouldBe` ""
    it "can generate KindIDV4 with insecure UUIDv4" do
      kid <- withCheck $ genID' @(KindIDV4 "mmzk")
      getPrefix kid `shouldBe` "mmzk"
    it "can parse KindID from String" do
      case string2ID @(KindIDV4 "mmzk") "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right kid -> getPrefix kid `shouldBe` "mmzk"
    it "cannot parse KindID into wrong prefix" do
      case string2ID @(KindIDV4 "foo") "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left _    -> pure ()
        Right kid -> expectationFailure $ "Parsed TypeID: " ++ show kid

  describe "Generate KindIDV4 with custom data kind prefixes" do
    it "can generate KindIDV4 with prefix" do
        kid <- withCheck $ genID @(KindID 'Post)
        getPrefix kid `shouldBe` "post"
    it "can parse KindIDV4 from String" do
      case string2ID @(KindIDV4 'User) "user_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right kid -> getPrefix kid `shouldBe` "user"
    it "cannot parse KindIDV4 into wrong prefix" do
      case string2ID @(KindIDV4 'Comment) "user_00041061050r3gg28a1c60t3gf" of
        Left _    -> pure ()
        Right kid -> expectationFailure $ "Parsed KindIDV4: " ++ show kid

  describe "Binary instance for TypeIDV4 and KindIDV4" do
    it "has correct binary instance for TypeIDV4" do
      tids <- withChecks $ genIDs @TypeIDV4 "abcdefghijklmnopqrstuvwxyz" 114
      forM_ tids \tid -> do
        let bytes = runPut (put tid)
        let tid'  = runGet get bytes
        tid' `shouldBe` tid
    it "has correct binary instance for KindIDV4" do
      kids <- withChecks $ genIDs @(KindIDV4 "abcdefghijklmnopqrstuvwxyz") 114
      forM_ kids \kid -> do
        let bytes = runPut (put kid)
        let kid'  = runGet get bytes
        kid' `shouldBe` kid

  describe "Storable instance for TypeIDV4 and KindIDV4" do
    it "has correct Storable instance for TypeIDV4" do
      tids <- withChecks $ genIDs @TypeIDV4 "abcdefghijklmnopqrstuvwxyz" 114
      forM_ tids \tid -> do
        ptr   <- new tid
        tid'  <- peek ptr
        tid' `shouldBe` tid
        poke ptr tid'
        tid'' <- peek ptr
        tid'' `shouldBe` tid'
        free ptr
    it "has correct Storable instance for KindIDV4" do
      kids <- withChecks $ genIDs @(KindIDV4 "abcdefghijklmnopqrstuvwxyz") 114
      forM_ kids \kid -> do
        ptr   <- new kid
        kid'  <- peek ptr
        kid' `shouldBe` kid
        poke ptr kid'
        kid'' <- peek ptr
        kid'' `shouldBe` kid'
        free ptr

v5Test :: Spec
v5Test = do
  invalid   <- runIO (BSL.readFile "test/invalid.json" >>= throwDecode :: IO [TestData])
  valid     <- runIO (BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestData])
  validUUID <- runIO (BSL.readFile "test/valid.json" >>= throwDecode :: IO [TestDataUUID TypeIDV5])
  
  describe "Generate TypeIDV5" do
    uid <- runIO nextRandom
    it "can generate TypeIDV5 with prefix" do
      tid <- withCheck $ genID @TypeIDV5 "mmzk" uid [11, 45, 14]
      getPrefix tid `shouldBe` "mmzk"
    it "can generate TypeIDV5 without prefix" do
      tid <- withCheck $ genID @TypeIDV5 "" uid [11, 45, 14]
      getPrefix tid `shouldBe` ""
    it "can parse TypeIDV5 from String" do
      case string2ID @TypeIDV5 "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeIDV5 from Text" do
      case text2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"
    it "can parse TypeIDV5 from ByteString" do
      case byteString2ID @TypeID "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> getPrefix tid `shouldBe` "mmzk"

  describe "Parse TypeIDV5" do
    uid <- runIO nextRandom
    let invalidPrefixes = [ ("caps", "PREFIX")
                          , ("numeric", "12323")
                          , ("symbols", "pre.fix")
                          , ("spaces", "  ")
                          , ("long", "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
                          , ("ascii", "château") ]
    describe "can detect invalid prefix" do
      forM_ invalidPrefixes \(reason, pref) -> it reason do
        genID @TypeIDV5 pref uid [11, 45, 14] `shouldThrow` anyTypeIDError
        case decorate @TypeIDV5 pref nil of
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
        case string2ID @TypeIDV5 suffix of
          Left _    -> pure ()
          Right tid -> expectationFailure $ "Parsed TypeID: " ++ show tid

  describe "Parse special values" do
    let specialValues = [ ("nil", "00000000000000000000000000", "00000000-0000-0000-0000-000000000000")
                        , ("one", "00000000000000000000000001", "00000000-0000-0000-0000-000000000001")
                        , ("ten", "0000000000000000000000000a", "00000000-0000-0000-0000-00000000000a")
                        , ("sixteen", "0000000000000000000000000g", "00000000-0000-0000-0000-000000000010")
                        , ("thirty-two", "00000000000000000000000010", "00000000-0000-0000-0000-000000000020") ]
    forM_ specialValues \(reason, tid, uid) -> it reason do
      case string2ID @TypeIDV5 tid of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right t  -> show (getUUID t) `shouldBe` uid

  describe "TypeIDV5 valid JSON instances" do
    it "Decode and then encode should be identity" do
      uid <- nextRandom
      tid  <- genID @TypeIDV5 "mmzk" uid [11, 45, 14]
      tid' <- genID @TypeIDV5 "foo" uid [11, 45, 14]
      let mapping = M.fromList [(tid, tid')]
      let tJson   = encode mapping
      decode tJson `shouldBe` Just mapping
      fmap encode (decode @(Map TypeIDV5 TypeIDV5) tJson) `shouldBe` Just tJson
    describe "Valid JSON value" do
      forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
        case decode @TypeIDV5 (fromString $ show tid) of
          Nothing -> expectationFailure "Parse JSON failed!"
          Just t  -> do
            getPrefix t `shouldBe` pref
            show (getUUID t) `shouldBe` uid
            Right t `shouldBe` string2ID tid
            Right t `shouldBe` text2ID (T.pack tid)
            Right t `shouldBe` byteString2ID (BSL.fromStrict . encodeUtf8 $ T.pack tid)
    describe "Valid JSON key" do
      forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
        case decode @(Map TypeIDV5 Int) (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") of
          Nothing      -> expectationFailure "Parse JSON failed!"
          Just mapping -> do
            let (t, _) = M.elemAt 0 mapping
            getPrefix t `shouldBe` pref
            show (getUUID t) `shouldBe` uid

  describe "TypeIDV5 invalid JSON instances" do
    describe "Invalid JSON value" do
      forM_ invalid \(TestData n tid _ _) -> it n do
        case decode @TypeIDV5 (fromString $ show tid) of
          Nothing -> pure ()
          Just t  -> expectationFailure $ "Parsed TypeID: " ++ show t
    describe "Invalid JSON key" do
      forM_ invalid \(TestData n tid _ _) -> it n do
        case decode @(Map TypeIDV5 Int) (fromString $ "{" ++ show tid ++ ":" ++ "114514" ++ "}") of
          Nothing -> pure ()
          _       -> expectationFailure "Invalid TypeID key shouldn't be parsed!"

  describe "Test invalid.json" do
    forM_ invalid \(TestData n tid _ _) -> it n do
      case string2ID @TypeIDV5 tid of
        Left _    -> pure ()
        Right t -> expectationFailure $ "Parsed TypeID: " ++ show t

  describe "Test valid.json (TypeIDV5 as literal)" do
    forM_ valid \(TestData n tid (Just pref) (Just uid)) -> it n do
      case string2ID @TypeIDV5 tid of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right t  -> do
          getPrefix t `shouldBe` pref
          show (getUUID t) `shouldBe` uid

  describe "Test valid.json (TypeIDV5 as JSON)" do
    forM_ validUUID \(TestDataUUID n tid pref uid) -> it n do
      getPrefix tid `shouldBe` pref
      getUUID tid `shouldBe` uid

  describe "Generate KindIDV5 with 'Symbol' prefixes" do
    uid <- runIO nextRandom
    it "can generate KindIDV5 with prefix" do
      kid <- withCheck $ genID @(KindIDV5 "mmzk") uid [11, 45, 14]
      getPrefix kid `shouldBe` "mmzk"
    it "can generate KindIDV5 without prefix" do
      kid <- withCheck $ genID @(KindIDV5 "") uid [11, 45, 14]
      getPrefix kid `shouldBe` ""
    it "can parse KindID from String" do
      case string2ID @(KindIDV5 "mmzk") "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right kid -> getPrefix kid `shouldBe` "mmzk"
    it "cannot parse KindID into wrong prefix" do
      case string2ID @(KindIDV5 "foo") "mmzk_5hjpeh96458fct8t49fnf9farw" of
        Left _    -> pure ()
        Right kid -> expectationFailure $ "Parsed TypeID: " ++ show kid

  describe "Generate KindIDV5 with custom data kind prefixes" do
    it "can generate KindIDV5 with prefix" do
        kid <- withCheck $ genID @(KindID 'Post)
        getPrefix kid `shouldBe` "post"
    it "can parse KindIDV5 from String" do
      case string2ID @(KindIDV5 'User) "user_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right kid -> getPrefix kid `shouldBe` "user"
    it "cannot parse KindIDV5 into wrong prefix" do
      case string2ID @(KindIDV5 'Comment) "user_00041061050r3gg28a1c60t3gf" of
        Left _    -> pure ()
        Right kid -> expectationFailure $ "Parsed KindIDV5: " ++ show kid

  describe "Binary instance for TypeIDV5 and KindIDV5" do
    uid <- runIO nextRandom
    it "has correct binary instance for TypeIDV5" do
      tids <- withChecks $ genIDs @TypeIDV5 "abcdefghijklmnopqrstuvwxyz" uid [11, 45, 14] 114
      forM_ tids \tid -> do
        let bytes = runPut (put tid)
        let tid'  = runGet get bytes
        tid' `shouldBe` tid
    it "has correct binary instance for KindIDV5" do
      kids <- withChecks $ genIDs @(KindIDV5 "abcdefghijklmnopqrstuvwxyz") uid [11, 45, 14] 114
      forM_ kids \kid -> do
        let bytes = runPut (put kid)
        let kid'  = runGet get bytes
        kid' `shouldBe` kid

  describe "Storable instance for TypeIDV5 and KindIDV5" do
    uid <- runIO nextRandom
    it "has correct Storable instance for TypeIDV5" do
      tids <- withChecks $ genIDs @TypeIDV5 "abcdefghijklmnopqrstuvwxyz" uid [11, 45, 14] 114
      forM_ tids \tid -> do
        ptr   <- new tid
        tid'  <- peek ptr
        tid' `shouldBe` tid
        poke ptr tid'
        tid'' <- peek ptr
        tid'' `shouldBe` tid'
        free ptr
    it "has correct Storable instance for KindIDV5" do
      kids <- withChecks $ genIDs @(KindIDV5 "abcdefghijklmnopqrstuvwxyz") uid [11, 45, 14] 114
      forM_ kids \kid -> do
        ptr   <- new kid
        kid'  <- peek ptr
        kid' `shouldBe` kid
        poke ptr kid'
        kid'' <- peek ptr
        kid'' `shouldBe` kid'
        free ptr
