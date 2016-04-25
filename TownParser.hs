module TownParser where

import Text.Parsec
import Control.Monad (liftM)
import WorldParser (Coord, parseCoord)

data TownData = TownData { name :: String } deriving (Show, Read)

parseName :: Parsec String () String
parseName = do
  n <- many letter
  return n

parseTownData :: Parsec String () (Coord,TownData)
parseTownData = do
  c <- parseCoord
  spaces
  string "Name"
  spaces
  n <- parseName
  return $ (c, TownData n)

parseTownFile = do
  let
    contents = (liftM lines) $ readFile "inputTownParser.txt"
    f = liftM $ (mapM (parse parseTownData "source"))
  res <- f contents
  return res
