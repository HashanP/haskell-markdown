import Test.Hspec
import Text.Parsec (ParseError)
import Lib (parseText, parseEmph, parseItalic, parseParagraph, parseBody, Block(..), regularParse)

getBlock :: Either ParseError Block -> Block
getBlock (Right blk) = blk
getBlock (Left err) = error $ show err

main :: IO ()
main = hspec $ do
  describe "parseParagraph" $ do
    it "works with bold" $ do
      getBlock (regularParse parseEmph "**Bob**") `shouldBe` Emph [Text "Bob"]
    it "works with italic" $ do
      getBlock (regularParse parseItalic "*Bob*") `shouldBe` Italic [Text "Bob"]
    it "works with bold in italic" $ do
      getBlock (regularParse parseItalic "***B**ob*") `shouldBe` Italic [Emph [Text "B"], Text "ob"]
    it "works with italic in bold" $ do
      getBlock (regularParse parseItalic "***B*ob**") `shouldBe` Emph [Italic [Text "B"], Text "ob"]
