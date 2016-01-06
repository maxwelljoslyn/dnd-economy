module WorldParser where

import Text.Parsec
import Control.Monad (liftM)

data Coord = Coord (Int, Int, Int)
	deriving (Eq, Read, Show)

data Elevation = Elevation Double
	deriving (Read, Show)

data Temperature = Temperature Double
	deriving (Read, Show)

data Climate = Desert | Mediterranean | HotSummerContinental | WarmSummerContinental | ColdContinental | WetContinental | Tundra | Savannah | Monsoon | Steppe | Oceanic | ColdOceanic | TropicalRainforest | HumidSubtropical | Taiga | IceCap | Water
	deriving (Read, Show)


--bringing all the above datatypes together
data Hex = Hex
	{ coord :: Coord
	, elev	:: Elevation
	, temp :: Temperature
	, isLand :: Bool
	, moist :: Double
	, climate :: Climate
	} deriving (Read, Show)

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
	return $ Coord (read q,read r,read s)
	where
		coordShape = many (oneOf "-0123456789")

parseElevation :: Parsec String () Elevation
parseElevation = do
	string "Elevation"
	spaces
	e <- many	$ oneOf "-.0123456789"
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
	return $ Hex c e t l m clim


parseWorld = do
	let
		contents	= (liftM lines) $ readFile "inputForParser.txt"
		f					= liftM $ (mapM (parse parseHex "SRC"))
	result <- f contents
	return result
