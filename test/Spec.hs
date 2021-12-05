import qualified NetworkManager as NM
import System.Directory
import Test.Hspec
import qualified Toml

main :: IO ()
main = hspec $ do
  describe "NetworkManager.configCodec" $ do
    it "parses the example config correctly" $ do
      path <- getCurrentDirectory
      config <- Toml.decodeFile NM.configCodec $ path <> "/test/sample.toml"
      config `shouldBe` NM.sampleConfig
