module RoadParser where

import Text.Parsec
import Control.Monad (liftM)
import WorldParser (Coord, parseCoord)

data Road = Road [Coord] deriving (Read, Show)

parseRoad :: Parsec String () Road
parseRoad = do
  char '['
  coords <- sepBy parseCoord (char ',')
  char ']'
  return $ Road coords

parseRoadFile = do
  let
    contents = (liftM lines) $ readFile "inputRoadParser.txt"
    f = liftM $ (mapM (parse parseRoad "source"))
  res <- f contents
  return res
