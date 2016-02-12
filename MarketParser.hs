module MarketParser where

import Text.Parsec
import Control.Monad (liftM)
import WorldParser (Coord, parseCoord)
import Data.Map

data MarketData = MarketData { name :: String } deriving (Show, Read)

parseName :: Parsec String () String
parseName = do
  n <- many letter
  return n

parseMarketData :: Parsec String () (Coord,MarketData)
parseMarketData = do
  c <- parseCoord
  spaces
  string "Name"
  spaces
  n <- parseName
  return $ (c, MarketData n)

parseMarketFile = do
  let
    contents = (liftM lines) $ readFile "inputMarketParser.txt"
    f = liftM $ (mapM (parse parseMarketData "source"))
  res <- f contents
  return res
