module WorldParser where

import Text.Parsec
import Control.Monad (liftM)
import Data.Map (Map)
import qualified Data.Map as Map

type Region = Int

data Coord = Coord Int Int Int
	deriving (Ord, Eq, Read, Show)

cq, cr, cs :: Coord -> Int
cq (Coord q _ _) = q
cr (Coord _ r _) = r
cs (Coord _ _ s) = s

data Elevation = Elevation Double
	deriving (Read, Show)

data Temperature = Temperature Double
	deriving (Read, Show)

data Climate = Desert | Mediterranean | HotSummerContinental | WarmSummerContinental | ColdContinental | WetContinental | Tundra | Savannah | Monsoon | Steppe | Oceanic | ColdOceanic | TropicalRainforest | HumidSubtropical | Taiga | IceCap | Water
	deriving (Read, Show)

data Direction = UP | UR | DR | DN | DL | UL
                 deriving (Read, Show, Enum, Ord, Eq)

data Quality = Civilized | Wild
               deriving (Read, Show, Eq)

type SubInfo = (Direction, Elevation, Quality)
data Sub = Sub Elevation Quality
           deriving (Read, Show)

--bringing all the above datatypes together
data Hex = Hex
        { coord :: Coord
	, elev	:: Elevation
	, temp :: Temperature
	, isLand :: Bool
	, moist :: Double
	, climate :: Climate
        , subs :: Map Direction Sub
	} deriving (Read, Show)

parseQuality :: Parsec String () Quality
parseQuality = do
  string "Quality"
  spaces
  q <- try (string "Civilized" <|> string "Wild")
  return $ read q
  

parseRegion :: Parsec String () Region
parseRegion = do
  string "Region"
  spaces
  r <- many $ digit
  return $ read r

parseDirection :: Parsec String () Direction
parseDirection = do
  c1 <- anyChar
  c2 <- anyChar
  let stringified = c1:c2:""
  return $ read stringified
  
parseSubInfo :: Parsec String () SubInfo 
parseSubInfo = do
  d <- parseDirection
  spaces
  e <- parseElevation
  spaces
  q <- parseQuality
  return (d, e, q)

parseSubInfos :: Parsec String () [SubInfo]
parseSubInfos = do
  first <- parseSubInfo
  rest <- remainingSubInfo
  return (first : rest)
  where
    remainingSubInfo = do
      (char ',' >> parseSubInfos) <|> (return [])
 

parseCoord :: Parsec String () Coord
parseCoord = do
	string "Coord"
	spaces
	char '('
	q <- coordShape
	char ','
	spaces
	r <- coordShape
	char ','
	spaces
	s <- coordShape
	char ')'
	return $ Coord (read q) (read r) (read s)
	where
		coordShape = many (oneOf "-0123456789")

parseElevation :: Parsec String () Elevation
parseElevation = do
	string "Elevation"
	spaces
	e <- many $ oneOf "-.0123456789"
	return $ Elevation (read e)

parseTemperature :: Parsec String () Temperature
parseTemperature = do
	string "Temperature"
	spaces
	t <- many $ oneOf "-.0123456789"
	return $ Temperature (read t)

parseLand :: Parsec String () Bool
parseLand = do
	string "Land"
	spaces
	l <- try (string "True" <|> string "False")
	return $ read l

parseMoisture :: Parsec String () Double
parseMoisture = do
	string "Moisture"
	spaces
	m <- many $ oneOf ".0123456789"
	return $ read m


parseClimate :: Parsec String () Climate
parseClimate = do
	string "Climate"
	spaces
	c <- try (string "Desert" <|> string "Oceanic" <|> string "IceCap" <|> parseInitialW <|> parseInitialM <|> parseInitialH <|> parseInitialS <|> parseInitialT <|> parseInitialCold)
	return $ read c

parseInitialW :: Parsec String () String
parseInitialW = do
	first <- char 'W'
	rest <- string "ater" <|> string "etContinental"
	return (first:rest)

parseInitialM :: Parsec String () String
parseInitialM = do
	first <- char 'M'
	rest	<- string "onsoon" <|> string "editerranean"
	return (first:rest)

parseInitialH :: Parsec String () String
parseInitialH = do
	first <- char 'H'
	rest	<- string "otSummerContinental" <|> string "umidSubtropical"
	return (first : rest)

parseInitialS :: Parsec String () String
parseInitialS = do
	first <- char 'S'
	rest	<- string "avannah" <|> string "teppe"
	return (first : rest)

parseInitialT :: Parsec String () String
parseInitialT = do
	first <- char 'T'
	rest <- string "aiga" <|> string "ropicalRainforest" <|> string "undra"
	return (first : rest)

parseInitialCold :: Parsec String () String
parseInitialCold = do
	first <- string "Cold"
	rest <- string "Continental" <|> string "Oceanic"
	return (first ++ rest)

parseHex :: Parsec String () Hex
parseHex = do
	string "Hex"
	spaces
	c <- parseCoord
	spaces
	e <- parseElevation
	spaces
	t <- parseTemperature
	spaces
	l <- parseLand
	spaces
	m <- parseMoisture
	spaces
	clim <- parseClimate
        spaces
        string "Subs"
        spaces
        char '['
        subs <- parseSubInfos
        char ']'
        let f = \(dir,elev,qual) -> (dir, Sub elev qual)
            subs' = map f subs
	return $ Hex c e t l m clim (Map.fromList subs')


parseWorld = do
	let
	  contents = (liftM lines) $ readFile "inputWorldParser.txt"
	  f = liftM $ (mapM (parse parseHex "SRC"))
	result <- f contents
	return result
