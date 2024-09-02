{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module ZipCoder.Lib where

import Control.Monad (when)
import Data.Char (toUpper)
import Data.IntMap (fromList)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import GHC.Generics (Generic)
import Options.Applicative
import System.Exit
import Text.Printf (printf)
import Text.RawString.QQ
import Text.Read (readMaybe)
import Text.Regex.PCRE
import ZipCoder.Model
import ZipCoder.OptionsParser

type ZipCodeValidationFunction = (String -> Bool)

data ValidCountryCode = AT | AU | BE | CA | CH | CZ | DE | DK | ES | FI | FR | GB | HR | HU | IE | IT | JP | KR | NL | NO | PL | PT | RO | RS | RU | SE | SI | SK | US
  deriving (Show, Eq, Read, Ord, Enum)

parseValidCountryCode :: String -> Maybe ValidCountryCode
parseValidCountryCode x = findValidCountryInCode =<< parsedMaybe
  where
    parsedMaybe = readMaybe x :: Maybe ValidCountryCode
    findValidCountryInCode country =
      if Map.member country getMasterZipCodeValidator
        then Just country
        else Nothing

getMasterZipCodeValidator :: Map.Map ValidCountryCode ZipCodeValidationFunction
getMasterZipCodeValidator =
  Map.fromList
    [ (AT, (=~ [r|^[1-9]\d{3}$|])),
      (AU, (=~ [r|^\d{4}$|])),
      (BE, (=~ [r|^[1-9]\d{3}$|])),
      (CA, (=~ [r|^(?i)([^DFIOQUWZ\d]{1}\d{1}[^DFIOQU\d]{1})\s*(\d{1}[^DFIOQU\d]{1}\d{1})$|])),
      (CH, (=~ [r|^[1-9]\d{3}$|])),
      (CZ, (=~ [r|^[1-7]\d{4}$|])),
      (DE, (=~ [r|^\d{5}$|])),
      (DK, (=~ [r|^[1-9]\d{3}$|])),
      (ES, (=~ [r|^\d{5}$|])),
      (FI, (=~ [r|^\d{5}$|])),
      (FR, (=~ [r|^\d{5}$|])),
      (GB, (=~ [r|^(?i)[A-Z]{1,2}(\d{1,2})\s*([0-9A-Z]{3,4})$|])),
      (HR, (=~ [r|^[1-9]\d{4}$|])),
      (HU, (=~ [r|^[1-9]\d{3}$|])),
      (IE, (=~ [r|^(?i)[A-Z]{1}\d{1,2}[0-9W]{1}[ \-]?[0-9A-Z]{4}$|])),
      (IT, (=~ [r|^\d{5}$|])),
      (JP, (=~ [r|^\d{3}-\d{4}$|])),
      (KR, (=~ [r|^\d{4}$|])),
      (NL, (=~ [r|^(?i)[1-9]\d{3} ?(?!SA|SD|SS)[A-Z]{2}$|])),
      (NO, (=~ [r|^\d{4}$|])),
      (PL, (=~ [r|^\d{2}-\d{3}$|])),
      (PT, (=~ [r|^[1-9]\d{3}-\d{3}$|])),
      (RO, (=~ [r|^\d{6}$|])),
      (RS, (=~ [r|^[1-9]\d{4}$|])),
      (RU, (=~ [r|^[1-9]\d{5}$|])),
      (SE, (=~ [r|^[1-9]\d{4}$|])),
      (SI, (=~ [r|^[1-9]\d{3}$|])),
      (SK, (=~ [r|^[890]\d{4}$|])),
      (US, (=~ [r|^\d{5}$|]))
    ]

maybeValidationFunction :: ValidCountryCode -> Maybe ZipCodeValidationFunction
maybeValidationFunction validCountryCode = Map.lookup validCountryCode getMasterZipCodeValidator

extractBadEntries :: ZipCodeValidationFunction -> [String] -> [String]
extractBadEntries f = filter (not . f)

runApplication :: IO ()
runApplication = app =<< execParser opts
  where
    opts =
      info
        (zipCoderOptions <**> helper)
        ( fullDesc
            <> progDesc "Easy, fast and powerful zipcode validation, written in Haskell"
            <> header "λ ZipCoder - Powerful zipcode validation"
        )

app :: ZipCoderOptions -> IO ()
app options = do
  putStrLn "λ ZipCoder - Powerful zip-code validation"
  putStrLn "λ Starting ZipCoder after successful parsing of options.."

  case validCountryCodeMaybe of
    Nothing -> error "[ERROR] Invalid country code! Terminating program!"
    Just validCountryCode -> case maybeValidationFunction validCountryCode of
      Nothing -> error (printf "[ERROR] No parser exists for the desired country: %s" (show validCountryCode))
      Just validationFunction -> applyValidation validCountryCode validationFunction zipCodeList
  where
    zipCodeList = extractCodesFromOptions . zipCodes $ options
    validCountryCodeMaybe = parseValidCountryCode . map toUpper . countryCode $ options

applyValidation :: Show a => a -> (String -> Bool) -> [String] -> IO ()
applyValidation validCountryCode validationFunction zipCodeList = do
  putStrLn $ printf "λ Initiating validation for country %s..\n" (show validCountryCode)
  if all validationFunction zipCodeList
    then putStrLn "λ All zip-codes are valid!"
    else error (printf "[ERROR] Detected bad data! Bad entries will be printed: %s" (show $ extractBadEntries validationFunction zipCodeList))
