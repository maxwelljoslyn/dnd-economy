module CityParser where

import Text.Parsec
import Control.Monad (liftM)
import WorldParser (Coord, parseCoord)

--for testing purposes
main = do
  let x = parse parseMarket "source" "Market Name Mantarctica Coord (12,13,14)"
  case x of
    Left err -> putStrLn "err"
    Right cool -> putStrLn $ show cool

data Market = Market
  { name :: String
  , coord :: Coord
  } deriving (Show, Read)

parseName :: Parsec String () String
parseName = do
  string "Name"
  spaces
  n <- many letter
  return n

parseMarket :: Parsec String () Market
parseMarket = do
  string "Market"
  spaces
  n <- parseName
  spaces
  c <- parseCoord
  return $ Market n c

parseMarketFile = do
  let
    contents = (liftM lines) $ readFile "inputMarketParser.txt"
    f = liftM $ (mapM (parse parseMarket "source"))
  result = f contents
  return result
