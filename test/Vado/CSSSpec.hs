{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Vado.CSSSpec (spec) where

import qualified Data.Map as M

import           Data.Text (Text)
import qualified Data.Text as T
import           Test.Hspec
import           Text.RawString.QQ

import Vado.CSS

style :: [(CSSOwnProperty, CSSValue)] -> [(CSSProperty, CSSValue)] -> Style
style owns props = Style{ styleOwn = M.fromList owns, styleInherit = M.fromList props }

spec :: Spec
spec = do
  describe "cssReadValue" $ do
    it "keywords" $ cssReadValue "green" `shouldBe` Right (CSS_Keyword "green")

  describe "cssParser" $ do
    it "empty selector block" $
      cssParser "{font-size:85%}" `shouldBe`
        [("", noStyle, style [] [(CSSFontSize, CSS_Percent 85)])]
    it "a single block" $
      cssParser "div{color:red}"
        `shouldBe` [("div", noStyle, style [] [(CSSColor, CSS_RGB 255 0 0)])]
    it "a single block with semicolon" $
      cssParser "div{color:red;}"
        `shouldBe` [("div", noStyle, style [] [(CSSColor, CSS_RGB 255 0 0)])]
    it "understands !important" $
      cssParser ".small{font-size:85% !important}"
        `shouldBe` [(".small", style [] [(CSSFontSize, CSS_Percent 85)], noStyle)]
    it "two properties block" $
      cssParser "quack{display: inline; color: green}" `shouldBe`
        [("quack", noStyle, style
           [(CSSDisplay, CSS_Keyword "inline")]
           [(CSSColor, CSS_RGB 0 128 0)])
        ]
    it "two blocks" $
      cssParser ".small{font-size:85% !important}quack{color: green}" `shouldBe`
        [ (".small", style [] [(CSSFontSize, CSS_Percent 85)], noStyle)
        , ("quack", noStyle, style [] [(CSSColor, CSS_RGB 0 128 0)])
        ]
