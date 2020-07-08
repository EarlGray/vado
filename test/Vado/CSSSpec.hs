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
  describe "css_properties_support" $ do
    -- CSS 2.1, Appendix F
    it "background-attachment" $ pendingWith "TODO"
    it "background-color" $
      --css [("background-color", "yellow")] `shouldBe`
      --  style [(CSSBackgroundColor, CSS_Keyword "yellow")] []
      pendingWith "Make it an own property"
    it "background-image" $ pendingWith "TODO"
    it "background-position" $ pendingWith "TODO"
    it "background-repeat" $ pendingWith "TODO"
    it "background" $ pendingWith "TODO"
    it "border-collapse" $ pendingWith "TODO"
    it "border-color" $ pendingWith "TODO"
    it "border-spacing" $ pendingWith "TODO"
    it "border-style" $ pendingWith "TODO"
    it "border-top|right|bottom|left" $ pendingWith "TODO"
    it "border-top|right|bottom|left-color" $ pendingWith "TODO"
    it "border-top|right|bottom|left-style" $ pendingWith "TODO"
    it "border-top|right|bottom|left-width" $ pendingWith "TODO"
    it "border-width" $ pendingWith "TODO"
    it "border" $ pendingWith "TODO"
    it "bottom" $ pendingWith "TODO"
    it "caption-side" $ pendingWith "TODO"
    it "clear" $ pendingWith "TODO"
    it "content" $ pendingWith "TODO"
    it "counter-increment" $ pendingWith "TODO"
    it "counter-reset" $ pendingWith "TODO"
    it "cursor" $ pendingWith "TODO"
    it "direction" $ pendingWith "TODO"
    it "display" $
      css [("display", "table-cell")] `shouldBe` style [(CSSDisplay, CSS_Keyword "table-cell")] []
    it "empty-cells" $ pendingWith "TODO"
    it "float" $ pendingWith "TODO"
    it "font-family" $
      css [("font-family", "sans")] `shouldBe` style [] [(CSSFontFamily, CSS_Keyword "sans")]
    it "font-size" $
      css [("font-size", "xx-large")] `shouldBe` style [] [(CSSFontSize, CSS_Keyword "xx-large")]
    it "font-style" $
      css [("font-style", "oblique")] `shouldBe` style [] [(CSSFontStyle, CSS_Keyword "oblique")]
    it "font-weight" $
      css [("font-weight", "bolder")] `shouldBe` style [] [(CSSFontWeight, CSS_Keyword "bolder")]
    it "font" $
      css [("font", "normal bold 12pt \"Noto Serif\"")] `shouldBe`
        style [] [ (CSSFontWeight, CSS_Keyword "bold")
                 , (CSSFontStyle, CSS_Keyword "normal")
                 , (CSSFontSize, CSS_Px 16)
                 , (CSSFontFamily, CSS_String "Noto Serif")
                 ]
    it "font" $
      --css [("font", "normal bold small-caps 12pt/1.2 \"Noto Serif\", serif")] `shouldBe`
      --  style [] [ (CSSFontWeight, CSS_Keyword "bold")
      --           , (CSSFontStyle, "normal")
      --           , (CSSFontSize, CSS_Pt 12)
      --           , (CSSFontFamily, CSS_List [CSS_String "Noto Serif", CSS_Keyword "serif"])
      --           , (CSSFontVariant, CSS_Keyword "small-caps")
      --           , (CSSLineHeight, CSS_Num 1.2)
      --           ]
      pendingWith "TODO: line-height, font-variant"
    it "height" $ pendingWith "TODO"
    it "left" $ pendingWith "TODO"
    it "letter-spacing" $ pendingWith "TODO"
    it "line-height" $ pendingWith "TODO"
    it "list-style-image" $ pendingWith "TODO"
    it "list-style-position" $ pendingWith "TODO"
    it "list-style-type" $ pendingWith "TODO"
    it "list-style" $ pendingWith "TODO"
    it "margin-top|right|bottom|let" $ pendingWith "TODO"
    it "margin" $ pendingWith "TODO"
    it "max-height" $ pendingWith "TODO"
    it "max-width" $ pendingWith "TOOD"
    it "min-height" $ pendingWith "TODO"
    it "min-width" $ pendingWith "TODO"
    it "orphans" $ pendingWith "TODO"
    it "outline-color" $ pendingWith "TODO"
    it "outline-width" $ pendingWith "TODO"
    it "outline" $ pendingWith "TODO"
    it "padding-top|right|bottom|left" $ pendingWith "TODO"
    it "padding" $ pendingWith "TODO"
    it "position"  $ pendingWith "TODO"
    it "right" $ pendingWith "TODO"
    it "table-layout" $ pendingWith "TODO"
    it "table-align" $ pendingWith "TODO"
    it "text-align" $
      css [("text-align", "center")] `shouldBe` style [] [(CSSTextAlign, CSS_Keyword "center")]
    it "text-decoration-line" $
      --css [("text-decoration", "underline")] `shouldBe`
      --  style [(CSSTextDecorationLine, CSS_Keyword "underline")] []
      pendingWith "TODO: make own property"
    it "text-transform" $ pendingWith "TODO"
    it "top" $ pendingWith "TODO"
    it "unicode-bidi" $ pendingWith "TODO"
    it "vertical-align" $ pendingWith "TODO"
    it "visibility" $ pendingWith "TODO"
    it "white-space" $
      css [("white-space", "pre")] `shouldBe`
        style [] [(CSSWhiteSpace, CSS_Keyword "pre")]
    it "widows" $ pendingWith "TODO"
    it "width" $ pendingWith "TODO"
    it "word-spacing" $ pendingWith "TODO"
    it "z-index" $ pendingWith "TODO"

  describe "cssReadValue" $ do
    it "keywords" $ cssReadValue "green" `shouldBe` Right (CSS_Keyword "green")

  describe "cssSelectors" $ do
    it "any" $ cssSelectors "*" `shouldBe` Right [SelAny]
    it "tag" $ cssSelectors "div" `shouldBe` Right [SelTag "div"]
    it ".class" $ cssSelectors ".small" `shouldBe` Right [SelClass "small"]
    it "#my-id1" $ cssSelectors "#my-id1" `shouldBe` Right [SelId "my-id1"]
    --it "#id with nonalphanum" $ cssSelectors "#id_\\#\\:\\." `shouldBe` Right [SelId "id_#:."]
    it ":active" $
      cssSelectors ":active" `shouldBe`
        Right [SelPseudo "active"]
    it ":first-line" $
      cssSelectors ":first-line" `shouldBe`
        Right [SelPseudoElem "first-line"]
    it "::first-line" $     -- CSS 3 selectors
      cssSelectors "::first-line" `shouldBe`
        Right [SelPseudoElem "first-line"]
    it "p:first-line" $
      cssSelectors "p:first-line" `shouldBe`
        Right [SelAnd [SelTag "p", SelPseudoElem "first-line"]]
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

  describe "selectorSpecificity" $ do
    let testee = selectorSpecificity
    it "*" $ testee SelAny `shouldBe` (0, 0, 0)
    it "li" $ testee (SelTag "li") `shouldBe` (0, 0, 1)
    it "ul li" $
      testee (SelDescends (SelTag "li") (SelTag "ul")) `shouldBe` (0, 0, 2)
    it "ul ol+li" $
      testee (SelSiblings (SelTag "li") (SelDescends (SelTag "ol") (SelTag "ul")))
        `shouldBe` (0, 0, 3)
    it "h1 + *[rel=up]" $
      testee (SelSiblings (SelAnd [SelAny, SelAttrEq "rel" "up"]) (SelTag "h1"))
        `shouldBe` (0, 1, 1)
    it "li.red.level" $
      testee (SelAnd [SelTag "li", SelClass "red", SelClass "level"])
        `shouldBe` (0, 2, 1)
    it "#x334y" $
      testee (SelId "x344y") `shouldBe` (1, 0, 0)
    --it "#s12:not(foo)" $
    --  testee (SelAnd [SelId "s12", SelNot "foo"]) `shouldBe` (1, 0, 1)


  describe "cssParser" $ do
    let testee = cssParser
    it "case-insensitive" $
      testee "*{COLOR: RED !IMPORTANT}" `shouldBe`
        [([], SelAny, style [] [(CSSColor, CSS_RGB 255 0 0)], noStyle)]
    -- syntax
    it "empty selector block" $
      testee "*{}" `shouldBe`
        [([], SelAny, noStyle, style [] [])]
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
      --testee text `shouldBe`
      --  [ (["@media print"], SelTag "a", style [] [(CSSColor, CSS_RGB 0 0 0)], noStyle)
      --  , (["@media print", "@media(max-size: 800px)"],
      --      SelTag "img", noStyle, style [(CSSDisplay, CSS_Keyword "block")] [])
      --  ]
      pendingWith "css-text is wrong: it thinks it's a LeafBlock"

    -- error handling
    it "ignores unknown properties" $
      testee "h1 { color: red; rotation: 70minutes }" `shouldBe`
        [([], SelTag "h1", noStyle, style [] [(CSSColor, CSS_RGB 255 0 0)])]
    it "ignores malformed declarations" $
      --testee "p { color: green; color } a {}" `shouldBe` [([], SelTag "a", noStyle, noStyle)]
      pendingWith "css-text is wrong"
    --TODO: it "ignores unknown at-keywords" $
    it "autocloses at the end" $
      --testee "@media screen { p:before { font-family: 'Noto Sans" `shouldBe`
      --  [(["@media screen"], SelAnd [SelTag "p", SelPseudo "before"],
      --    noStyle, style [] [(CSSFontFamily, CSS_String "Noto Sans")])]
      pendingWith "css-text is wrong"
    it "discards blocks with newline in a non-closed string" $
      --testee "a{} p{color: green; font-family: 'Courier New\ncolor: red} b{}" `shouldBe`
      --  [ ([], SelTag "a", noStyle, noStyle)
      --  , ([], SelTag "b", noStyle, noStyle)
      --  ]
      pendingWith "css-text is wrong"
