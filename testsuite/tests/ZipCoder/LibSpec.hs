module ZipCoder.LibSpec (spec) where

import Data.Maybe (fromJust, isJust, isNothing)
import Test.Hspec
import Test.QuickCheck
import ZipCoder.Lib

prop_verifyValidCountry :: String -> Bool
prop_verifyValidCountry x = isNothing sut || isJust sut
  where
    sut = parseValidCountryCode x

validCountryElemsGenerator :: Gen ValidCountryCode
validCountryElemsGenerator = chooseEnum (AT, US)

prop_maybeValidationFunction :: Property
prop_maybeValidationFunction = forAll validCountryElemsGenerator (isJust . maybeValidationFunction)

spec :: Spec
spec = do
  describe "verifyValidCountryCode" $ do
    it "verifies that XX is not a valid country code" $ do
      parseValidCountryCode "XX" `shouldBe` Nothing
    it "verifies that NL is a valid country code" $ do
      parseValidCountryCode "NL" `shouldBe` Just NL
    it "ensures a correct Maybe value is always returned" $
      property prop_verifyValidCountry

  describe "extractBadEntries for NL" $ do
    let nlValidationFunction = fromJust . maybeValidationFunction $ NL

    it "extracts invalid entries as expected with 2 invalid and 0 valid" $ do
      let sut = ["11", "2K"]
      extractBadEntries nlValidationFunction sut `shouldBe` sut
    it "extracts invalid entries as expected with 0 invalid and 2 valid" $ do
      let sut = ["1141KH", "2234KM"]
      extractBadEntries nlValidationFunction sut `shouldBe` []
    it "extracts invalid entries as expected with 2 invalid and 2 valid" $ do
      let sut = ["11", "2K", "1141KH", "2234KM"]
      extractBadEntries nlValidationFunction sut `shouldBe` ["11", "2K"]
    it "extracts invalid entries as expected with empty list" $ do
      let sut = []
      extractBadEntries nlValidationFunction sut `shouldBe` sut

  describe "maybeValidationFunction" $ do
    it "ensures a correct Maybe value is always returned" $
      property prop_maybeValidationFunction

  describe "applyValidation" $ do
    let seValidationFunction = fromJust . maybeValidationFunction $ SE
    it "errors out when invalid zipcodes" $ do
      applyValidation SE seValidationFunction ["9702", "65412ABC"] `shouldThrow` errorCall "[ERROR] Detected bad data! Bad entries will be printed: [\"9702\",\"65412ABC\"]"
    it "succeeds when valid zipcodes" $ do
      applyValidation SE seValidationFunction ["97012", "65412"]
