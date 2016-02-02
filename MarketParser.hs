module MarketParser where

import Text.Parsec
import Control.Monad (liftM)
import WorldParser (Coord, parseCoord)

data Market = Market
  { coord :: Coord
  , name :: String
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
  c <- parseCoord
  spaces
  n <- parseName
  return $ Market c n

parseMarketFile = do
  let
    contents = (liftM lines) $ readFile "inputMarketParser.txt"
    f = liftM $ (mapM (parse parseMarket "source"))
  res <- f contents
  return res
