{-# LANGUAGE OverloadedStrings #-}

module VadoSpec (spec) where

import Data.Maybe (fromJust)
import Test.Hspec
import Vado
import Vado.CSS


spec :: Spec
spec = do
  describe "textTokens" $ do
    let f = textTokens
    it "should parse empty input"   $ f "" `shouldBe` []
    it "should parse one word"      $ f "hello" `shouldBe` ["hello"]
    it "should keep begin/end gaps" $ f " world " `shouldBe` [" ", "world", " "]
    it "should keep end space"      $ f "hello world " `shouldBe` ["hello", " ", "world", " "]
    it "should produce two spaces"  $ f "hello  world" `shouldBe` ["hello", " ", " ", "world"]
    it "should produce newlines" $
      f "hello \nworld\n" `shouldBe` ["hello", " ", "\n", "world", "\n"]

  describe "chunksFromTokens" $ do
    describe "for whitespace:normal" $ do
      let f = chunksFromTokens (CSS_Keyword "normal")
      it "should handle empty without gap"  $ f (False, []) `shouldBe` (False, [])
      it "should hanlde empty with gap"     $ f (True, []) `shouldBe` (True, [])
      it "should introduce gap"             $ f (False, [" "]) `shouldBe` (True, [" "])
      it "should make a \\n into gap"       $ f (True, ["\n"]) `shouldBe` (True, [])
      it "should compress spaces"           $ f (False, [" ", " ", " "]) `shouldBe` (True, [" "])
      it "should compress start gap and space" $
        f (True, [" ", "hello"]) `shouldBe` (False, ["hello"])
      it "should compress end spaces" $
        f (False, ["hello", " ", " "]) `shouldBe` (True, ["hello", " "])
      it "should keep start space without gap" $
        f (False, [" ", "hello"]) `shouldBe` (False, [" ", "hello"])
      it "should convert \\n into space" $
        f (False, ["\n", "hello", "\n"]) `shouldBe` (True, [" ", "hello", " "])
      it "should convert \\n into space unless gap" $
        f (True, ["\n", "hello", "\n"]) `shouldBe` (True, ["hello", " "])

    describe "for whitespace:pre" $ do
      let f = chunksFromTokens (CSS_Keyword "pre")
      it "should handle empty without gap"  $ f (False, []) `shouldBe` (False, [])
      it "should handle empty with gap"     $ f (True, []) `shouldBe` (True, [])
      it "should keep newlines"  $ f (True, ["\n", "\n"]) `shouldBe` (True, ["\n", "\n"])
      it "should keep input without gap" $
        f (False, ["hello"]) `shouldBe` (False, ["hello"])
      it "should introduce gap after space" $
        f (False, ["hello", " "]) `shouldBe` (True, ["hello "])
      it "should introduce gap after \\n" $
        f (False, ["hello", " ", "\n"]) `shouldBe` (True, ["hello ", "\n"])
      it "should put consequent spaces into one token" $
        f (False, [" ", " ", "hello", " ", "\n", "world"])
         `shouldBe` (False, ["  hello ", "\n", "world"])
      it "should put gap after \\n" $
        f (False, [" ", " ", "hello", " ", "\n", "world", "\n"])
          `shouldBe` (True, ["  hello ", "\n", "world", "\n"])
      it "should keep complex newlines" $
        f (False, ["hello", " ", " ", "world", " ", "\n", "world", "\n", "\n"])
          `shouldBe` (True, ["hello  world ", "\n", "world", "\n", "\n"])

  describe "domMatchSelector" $ do
    let match = domMatchSelector
    let doc = vadoHome -- TODO: a test document
    eid <- fmap fromJust $ inDocument doc $ getElementById "vado"
    let elt = elementRef doc eid
    it "*" $ match elt SelAny `shouldBe` Just (0, 0, 0)
    it "tag" $ match elt (SelTag "input") `shouldBe` Just (0, 0, 1)
    it "not tag" $ match elt (SelTag "a") `shouldBe` Nothing
    it "attr" $ match elt (SelHasAttr "autofocus") `shouldBe` Just (0, 1, 0)
    it "not attr" $ match elt (SelHasAttr "whatever") `shouldBe` Nothing
    it "attr=val" $ match elt (SelAttrEq "type" "text") `shouldBe` Just (0, 1, 0)
    it "not attr=val" $ match elt (SelAttrEq "type" "telepathy") `shouldBe` Nothing
    it "#id" $ match elt (SelId "vado") `shouldBe` Just (1, 0, 0)
    it "not #id" $ match elt (SelId "heyhey") `shouldBe` Nothing
    it "multiple selectors" $
      match elt (SelAnd [SelTag "input", SelClass "test1", SelClass "test2"])
       `shouldBe` Just (0, 2, 1)
    it "not multiple selectors" $
      match elt (SelAnd [SelTag "input", SelClass "nosuch", SelClass "test2"])
       `shouldBe` Nothing
    it "child" $ match elt (SelChild (SelId "vado") (SelId "the-form")) `shouldBe` Just (2, 0, 0)
    it "!child" $ match elt (SelChild (SelId "vado") (SelTag "body")) `shouldBe` Nothing
    it "!siblings" $ match elt (SelSiblings (SelId "vado") SelAny) `shouldBe` Nothing
    it "descends" $
      match elt (SelDescends (SelId "vado") (SelTag "body")) `shouldBe` Just (1, 0, 1)
    it "!descends" $ match elt (SelDescends (SelId "vado") (SelTag "a")) `shouldBe` Nothing
    it "descends chain" $
      match elt (SelDescends (SelId "vado") (SelDescends (SelTag "form") (SelTag "body")))
        `shouldBe` Just (1, 0, 2)
