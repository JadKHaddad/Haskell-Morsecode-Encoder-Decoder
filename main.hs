{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
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

mapOutputEncode :: [Char] -> [Char] -> [Symbol] -> Either String String
mapOutputEncode [] c _ = Right (c ++ " ")
mapOutputEncode (x : xs) c symbols =
  case findOutput [x] symbols of
    Left notValidSymbol -> Left notValidSymbol
    Right y -> mapOutputEncode xs (c ++ " " ++ y) symbols

mapOutputDecode :: [String] -> [Char] -> [Symbol] -> Either String String
mapOutputDecode [] c _ = Right (c ++ " ")
mapOutputDecode (x : xs) c symbols
  | x == "" = mapOutputDecode xs c symbols
  | elem '/' x = mapOutputDecode xs (c ++ " ") symbols
  | otherwise = case findOutput x symbols of
    Left notValidSymbol -> Left notValidSymbol
    Right y -> mapOutputDecode xs (c ++ " " ++ y) symbols

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
    d <- (eitherDecode <$> getJSONEncode) :: IO (Either String [Symbol])
    case d of
      Left err -> putStrLn $ err
      Right ps -> case mapOutputEncode (trim string) "" ps of
        Left notValidSymbol -> putStr $ "The fuck is this: " ++ notValidSymbol ++ "?\n"
        Right output -> putStrLn $ output

  when (args `isPresent` (command "decode")) $ do
    string <- args `getArgOrExit` (argument "string")
    d <- (eitherDecode <$> getJSONDecode) :: IO (Either String [Symbol])
    case d of
      Left err -> putStrLn $ err
      Right ps -> case mapOutputDecode (splitOn " " string) "" ps of
        Left notValidSymbol -> putStr $ "The fuck is this: " ++ notValidSymbol ++ "?\n"
        Right output -> putStrLn $ trim output ++ " "

  when (args `isPresent` (command "version")) $ do
    putStrLn $ version