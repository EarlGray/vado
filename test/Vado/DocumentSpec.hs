{-# LANGUAGE OverloadedStrings #-}

module Vado.DocumentSpec (spec) where

import qualified Control.Monad.Identity as MId
import qualified Control.Monad.State as St
import qualified Data.Maybe as Mb

import qualified Network.URI as URI
import Test.Hspec

import Vado.CSS
import Vado.Document


testdoc :: Document
testdoc =
  MId.runIdentity $ fromEmptyDocument $ do
    St.modify $ \doc -> doc{ documentLocation = Mb.fromJust $ URI.parseURI "vado:test" }
    htmlDOMFromXML $ xmlHtml body
  where
    body =
      xmlNode' "body" [("style", "text-align: center; white-space: pre")]
        [ xmlNode "h1" [ xmlText "\n\n\nVado"]
        , xmlNode "hr" []
        , xmlNode' "form" [("action", "vado:go"), ("method", "POST"), ("id", "the-form")]
          [ xmlNode' "input" inputAttrs []
          , xmlNode "br" []
          , xmlNode' "input" [("type", "submit"), ("value", "I go!")] []
          ]
        ]
    inputAttrs =
        [ ("type", "text"), ("name", "url"), ("id", "vado")
        , ("class", "test1 test2"), ("autofocus", "")
        ]

spec :: Spec
spec = do
  describe "domMatchSelector" $ do
    let match = domMatchSelector
    let doc = testdoc
    eid <- fmap Mb.fromJust $ inDocument doc $ getElementById "vado"
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
