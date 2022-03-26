{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.List.Split
import Data.Text (Text)
import GHC.Generics
import System.Console.Docopt
import System.Environment (getArgs)

data Symbol = Symbol
  { input :: String,
    output :: String
  }
  deriving (Show, Generic)

instance FromJSON Symbol

instance ToJSON Symbol

jsonFileEncode :: FilePath
jsonFileEncode = "morse-code-encode.json"

getJSONEncode :: IO B.ByteString
getJSONEncode = B.readFile jsonFileEncode

jsonFileDecode :: FilePath
jsonFileDecode = "morse-code-decode.json"

getJSONDecode :: IO B.ByteString
getJSONDecode = B.readFile jsonFileDecode

findOutput :: String -> [Symbol] -> Either String String
findOutput n [] = Left n
findOutput n (x : xs)
  | n == input x = Right (output x)
  | otherwise = findOutput n xs

mapOutputEncode :: [Char] -> [Symbol] -> Either String String
mapOutputEncode [] _ = Right []
mapOutputEncode (x : xs) symbols =
  case findOutput [x] symbols of
    Left notValidSymbol -> Left notValidSymbol
    Right symbol -> case mapOutputEncode xs symbols of
      Left notValidSymbol -> Left notValidSymbol
      Right result -> Right (symbol ++ " " ++ result)

mapOutputDecode :: [String] -> [Symbol] -> Either String String
mapOutputDecode [] _ = Right []
mapOutputDecode (x : xs) symbols
  | x == "" = mapOutputDecode xs symbols
  | elem '/' x = case mapOutputDecode xs symbols of
    Left notValidSymbol -> Left notValidSymbol
    Right result -> Right (" " ++ result)
  | otherwise = case findOutput x symbols of
    Left notValidSymbol -> Left notValidSymbol
    Right symbol -> case mapOutputDecode xs symbols of
      Left notValidSymbol -> Left notValidSymbol
      Right result -> Right (symbol ++ result)

addSpaces :: [Char] -> [Char]
addSpaces [] = []
addSpaces (x : xs)
  | x == '/' = " / " ++ addSpaces xs
  | otherwise = x : addSpaces xs

whiteSpaces :: [Char]
whiteSpaces = [' ', '\n', '\t']

trim :: [Char] -> [Char]
trim [] = []
trim (x : y : xs)
  | elem x whiteSpaces && elem y whiteSpaces = trim (" " ++ xs)
  | otherwise = x : trim (y : xs)
trim [x]
  | elem x whiteSpaces = []
  | otherwise = [x]

version :: String
version = "Morse Code Encoder/Decoder 1.0"

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "encode")) $ do
    string <- args `getArgOrExit` (argument "string")
    let input =
          if args `isPresent` (longOption "raw")
            then string
            else trim string
    d <- (eitherDecode <$> getJSONEncode) :: IO (Either String [Symbol])
    case d of
      Left err -> putStrLn $ err
      Right ps -> case mapOutputEncode input ps of
        Left notValidSymbol -> putStr $ "The fuck is this: " ++ notValidSymbol ++ "?\n"
        Right output -> putStrLn $ output

  when (args `isPresent` (command "decode")) $ do
    string <- args `getArgOrExit` (argument "string")
    d <- (eitherDecode <$> getJSONDecode) :: IO (Either String [Symbol])
    case d of
      Left err -> putStrLn $ err
      Right ps -> case mapOutputDecode (splitOn " " (addSpaces string)) ps of
        Left notValidSymbol -> putStr $ "The fuck is this: " ++ notValidSymbol ++ "?\n"
        Right output ->
          if args `isPresent` (longOption "raw")
            then putStrLn $ output
            else putStrLn $ trim output

  when (args `isPresent` (command "version")) $ do
    putStrLn $ version