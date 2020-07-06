{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Vado.CSSSpec (spec) where

import qualified Data.Map as M

import           Test.Hspec
import qualified Data.Text as T
import           Text.RawString.QQ

import           Vado.CSS


style :: [(CSSOwnProperty, CSSValue)] -> [(CSSProperty, CSSValue)] -> Style
style owns props = Style{ styleOwn = M.fromList owns, styleInherit = M.fromList props }

spec :: Spec
spec = do
  describe "cssReadValue" $ do
    it "keywords" $ cssReadValue "green" `shouldBe` Right (CSS_Keyword "green")

  describe "cssSelectors" $ do
    it "any" $ cssSelectors "*" `shouldBe` Right [SelAny]
    it "tag" $ cssSelectors "div" `shouldBe` Right [SelTag "div"]
    it ".class" $ cssSelectors ".small" `shouldBe` Right [SelClass "small"]
    it "#my-id1" $ cssSelectors "#my-id1" `shouldBe` Right [SelId "my-id1"]
    --it "#id with nonalphanum" $ cssSelectors "#id_\\#\\:\\." `shouldBe` Right [SelId "id_#:."]
    it ":first-line" $
      cssSelectors ":first-line" `shouldBe`
        Right [SelPseudo "first-line"]
    it "p:first-line" $
      cssSelectors "p:first-line" `shouldBe`
        Right [SelAnd [SelTag "p", SelPseudo "first-line"]]
    it "[attr]" $
      cssSelectors "[data-attr]" `shouldBe` Right [SelHasAttr "data-attr"]
    it "[attr=value]" $
      cssSelectors "[data-attr=value]" `shouldBe`
        Right [SelAttrEq "data-attr" "value"]
    it "[attr=\"string\"]" $
      cssSelectors "[data-attr=\"\\\"value\\\"\"]" `shouldBe`
        Right [SelAttrEq "data-attr" "\"value\""]
    it "[attr~=word]" $
      cssSelectors "[data-attr~=wo-rd]" `shouldBe`
        Right [SelAttrWord "data-attr" "wo-rd"]
    it "[attr|=fi]" $
      cssSelectors "[lang|=fi]" `shouldBe` Right [SelAttrPre "lang" "fi"]
    it "attribute='string'" $
      cssSelectors "a[href=\"http://www.w3.org/\"]" `shouldBe`
        Right [SelAnd [SelTag "a", SelAttrEq "href" "http://www.w3.org/"]]
    it "attribute='tricky string'" $
      cssSelectors "[attr=\"\\\"]\"]" `shouldBe` Right [SelAttrEq "attr" "\"]"]
    it "grouping: h1, h2, h3" $
      cssSelectors "h1, h2,h3" `shouldBe` Right [SelTag "h1", SelTag "h2", SelTag "h3"]
    it "descends: div.important em" $
      cssSelectors ".important em" `shouldBe`
        Right [SelDescends (SelTag "em") (SelClass "important")]
    it "descends: div * p" $
      cssSelectors "div * p" `shouldBe`
        Right [SelDescends (SelTag "p") (SelDescends SelAny (SelTag "div"))]
    it "combines: p.class1.class2" $
      cssSelectors "p.class1.class2" `shouldBe`
        Right [SelAnd [SelTag "p", SelClass "class1", SelClass "class2"]]
    it "descendant and child" $
      cssSelectors "div ol>li p" `shouldBe`
        Right [SelDescends (SelTag "p")
                           (SelChild (SelTag "li") (SelDescends (SelTag "ol") (SelTag "div")))]
    it "groups before combinators" $
      cssSelectors "body h1, h2" `shouldBe`
        Right [SelDescends (SelTag "h1") (SelTag "body"), SelTag "h2"]
    it "should fail: .." $
      cssSelectors "..whut" `shouldBe` Left "Failed reading: empty selector"
    it "should fail: `body, `" $
      cssSelectors "body, " `shouldBe` Left "endOfInput"


  describe "cssParser" $ do
    let testee = cssParser
    it "case-insensitive" $
      testee "*{COLOR: RED !IMPORTANT}" `shouldBe`
        [([], SelAny, style [] [(CSSColor, CSS_RGB 255 0 0)], noStyle)]
    -- syntax
    it "empty selector block" $
      testee "*{font-size:85%}" `shouldBe`
        [([], SelAny, noStyle, style [] [(CSSFontSize, CSS_Percent 85)])]
    it "a single block" $
      testee "div{color:red}" `shouldBe`
        [([], SelTag "div", noStyle, style [] [(CSSColor, CSS_RGB 255 0 0)])]
    it "a single block with semicolon" $
      testee "div{color:red;}" `shouldBe`
        [([], SelTag "div", noStyle, style [] [(CSSColor, CSS_RGB 255 0 0)])]
    it "understands !important" $
      testee ".small{font-size:85% !important}" `shouldBe`
        [([], SelClass "small", style [] [(CSSFontSize, CSS_Percent 85)], noStyle)]
    it "two properties block" $
      testee "quack{display: inline; color: green}" `shouldBe`
        [([], SelTag "quack", noStyle, style
           [(CSSDisplay, CSS_Keyword "inline")]
           [(CSSColor, CSS_RGB 0 128 0)])
        ]
    it "two blocks" $
      testee ".small{font-size:85% !important}quack{color: green}" `shouldBe`
        [ ([], SelClass "small", style [] [(CSSFontSize, CSS_Percent 85)], noStyle)
        , ([], SelTag "quack", noStyle, style [] [(CSSColor, CSS_RGB 0 128 0)])
        ]

    it "@media" $
      testee "@media print {a{color:black}}" `shouldBe`
        [(["@media print"], SelTag "a", noStyle, style [] [(CSSColor, CSS_RGB 0 0 0)])]
    it "@media: group selectors" $
      testee "@media print {a,b{color:black}}" `shouldBe`
        [ (["@media print"], SelTag "a", noStyle, style [] [(CSSColor, CSS_RGB 0 0 0)])
        , (["@media print"], SelTag "b", noStyle, style [] [(CSSColor, CSS_RGB 0 0 0)])
        ]
    it "@media: nested" $ do
      let _text = T.pack [r|
        @media print {
          a {color: black !important}
          @media(max-size: 800px) {
            img { display: block }
          }
        }
      |]
      pendingWith "Text.CSS.Parse.parseNestedBlocks parses the query as a LeafBlock!"
      --testee text `shouldBe`
      --  [ (["@media print"], SelTag "a", style [] [(CSSColor, CSS_RGB 0 0 0)], noStyle)
      --  , (["@media print", "@media(max-size: 800px)"],
      --      SelTag "img", noStyle, style [(CSSDisplay, CSS_Keyword "block")] [])
      --  ]
