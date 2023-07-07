import           Control.Monad
import           Data.TypeID (TypeID, TypeIDError)
import qualified Data.TypeID as TID
import qualified Data.UUID.V7 as V7
import           Test.Hspec

anyTypeIDError :: Selector TypeIDError
anyTypeIDError = const True

main :: IO ()
main = hspec do
  describe "Generate typeid" do
    it "Can generate typeid with prefix" do
      newID <- TID.genTypeID "mmzk"
      putStrLn $ "New typeid: " ++ TID.toString newID
    it "Can generate typeid without prefix" do
      newID <- TID.genTypeID ""
      putStrLn $ "New typeid without prefix: " ++ TID.toString newID
    it "Can parse typeid from String" do
      case TID.parseString "mmzk_00041061050r3gg28a1c60t3gf" of
        Left err  -> expectationFailure $ "Parse error: " ++ show err
        Right tid -> putStrLn $ "Parsed typeid: " ++ TID.toString tid
  describe "Parse typeid" do
    let invalidPrefixes = [ ("caps", "PREFIX")
                          , ("numeric", "12323")
                          , ("symbols", "pre.fix")
                          , ("spaces", "  ")
                          , ("long", "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
                          , ("ascii", "château") ]
    describe "Can detect invalid prefix" do
      forM_ invalidPrefixes \(reason, prefix) -> it reason do
        TID.genTypeID prefix `shouldThrow` anyTypeIDError
    let invalidSuffixes = [ ("spaces", " ")
                          , ("short", "01234")
                          , ("long", "012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
                          , ("caps", "00041061050R3GG28A1C60T3GF") -- Would be valid in lowercase
                          , ("hyphens", "00041061050-3gg28a1-60t3gf")
                          , ("crockford_ambiguous", "ooo41o61o5or3gg28a1c6ot3gi") -- Would be valid if we followed Crockford's substitution rules
                          , ("symbols", "00041061050.3gg28a1_60t3gf")
                          , ("wrong_alphabet", "ooooooiiiiiiuuuuuuulllllll") ]
    describe "Can detect invalid suffix" do
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
