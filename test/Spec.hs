import Test.Hspec
import Text.Parsec (ParseError)
import Lib (parseText, parseEmph, parseItalic, parseParagraph, parseBody, Block(..), regularParse)
import Control.Exception (evaluate)

getBlock :: Either ParseError a -> a
getBlock (Right blk) = blk
getBlock (Left err) = error $ show err

main :: IO ()
main = hspec $ do
  describe "parseParagraph" $ do
    it "should work with bold" $ do
      getBlock (regularParse parseParagraph "**Bob**") `shouldBe` Paragraph [Emph [Text "Bob"]]
    it "should work with italic" $ do
      getBlock (regularParse parseParagraph "*Bob*") `shouldBe` Paragraph [Italic [Text "Bob"]]
    it "should work with bold in italic" $ do
      getBlock (regularParse parseParagraph "***B**ob*") `shouldBe` Paragraph [Italic [Emph [Text "B"], Text "ob"]]
    it "should work with italic in bold" $ do
      getBlock (regularParse parseParagraph "***B*ob**") `shouldBe` Paragraph [Emph [Italic [Text "B"], Text "ob"]]
    it "should work with inline" $ do
      getBlock (regularParse parseParagraph "`Bob`") `shouldBe` Paragraph [Inline "Bob"]
    it "should work with inline in bold" $ do
      getBlock (regularParse parseParagraph "**`Bob`**") `shouldBe` Paragraph [Emph [Inline "Bob"]]
    it "should work with inline in italic" $ do
      getBlock (regularParse parseParagraph "*`Bob`*") `shouldBe` Paragraph [Italic [Inline "Bob"]]
    it "should work with bold over several lines" $ do
      getBlock (regularParse parseParagraph "**Bob\nBob\nBob**") `shouldBe` Paragraph [Emph [Text "Bob Bob Bob"]]
    it "shouldn't work with bold over several lines with a blank line" $ do
      evaluate (getBlock (regularParse parseParagraph "**Bob\nBob\n\nBob**")) `shouldThrow` anyException
  describe "parseBody" $ do
    it "should work with headings" $ do
      getBlock (regularParse parseBody "# Bob") `shouldBe` [Heading 1 [Text "Bob"]]
      getBlock (regularParse parseBody "# Bob\n") `shouldBe` [Heading 1 [Text "Bob"]]
    it "should work with indented code blocks" $ do
      getBlock (regularParse parseBody "    Bob") `shouldBe` [Code "Bob"]
      getBlock (regularParse parseBody "    Bob\n") `shouldBe` [Code "Bob"]
      getBlock (regularParse parseBody "    Bob\n    Bobby") `shouldBe` [Code "Bob\nBobby"]
