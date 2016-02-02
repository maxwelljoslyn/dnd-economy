{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.SRGB

import WorldParser
import qualified MarketParser as MP

import Text.Parsec
import Control.Monad (liftM)

main = do
	worldInfo <- parseWorld
	--having parsed in the world data, now we can proceed to rendering
	--we have to do all the rendering under the Right branch of the case statement below
	--why? because we can't know ahead of time whether resultOfParse is Left or Right
	--the only way to extract the data inside, which we need for parsing, is a pattern match
        marketInfo <- MP.parseMarketFile
        case marketInfo of
          Left err -> mainWith failure2
          Right ms -> 
	    case worldInfo of
		Right v		-> mainWith $ drawGrid ms v
		Left err	-> mainWith failure

drawGrid :: [MP.Market] -> [Hex] -> Diagram B
drawGrid ms hs = atPoints pts $ map (drawHex ms) hs
	where
		pts :: [P2 Double]
		pts = map (coordToPixel . coord) hs


drawHex :: [MP.Market] -> Hex -> Diagram B
drawHex ms h =
  (drawMarket $ hasMarket h ms)
  `atop`
  hexagon 1 # fc (climateColor $ climate h) # lc black # lw veryThin
  where
    moistColor' = moistColor $ moist h
    elevColor' = elevColor (elev h) (isLand h)
    onlyColorLandBorder = if (isLand h) == True then black else elevColor'
    --onlyColorLandBorder creates weird image b/c hexes render in strange order,
    --and some border colors overlap onto others,
    --creating weird "bites" taken out of some hexes.

failure :: Diagram B
failure =  circle 1 # fc pink # lw thick

failure2 :: Diagram B
failure2 =  triangle 1 # fc pink # lw thick

--TODO: upgrade from list of markets to dict of markets,
--so info from the market data structure can be retrieved
--this requires rewriting the market parser to some degree or possibly entirely
--but it's worth it, since that's how we'll get info out of the market record
--while preserving it for later calls (unlike current approach which
--destroys info by returning a Bool)
hasMarket :: Hex -> [MP.Market] -> Bool
hasMarket h ms = (coord h) `elem` (map MP.coord ms)

drawMarket :: Bool -> Diagram B
drawMarket True = square 0.1 # fc black  # lc black
drawMarket False = mempty


coordToPixel :: Coord -> P2 Double
coordToPixel (Coord (q,r,s)) = p2 (x,y)
	where
		q' = fromIntegral q
		r' = fromIntegral r
		x = 3/2 * (q')
		y = (sqrt 3) * (r' + ((q')/2))

elevColor (Elevation e) l
	|l == False	= sRGB 0 0 e --it's sea, therefore color it blueish
	|e < 0.50		= lightgreen
	|e < 0.55		= green
	|e < 0.60		= darkgreen
	|e < 0.65		= pink
	|e < 0.70		= lavender
	|e < 0.75		= magenta
	|e < 0.80		= red
	|e < 0.85		= seagreen
	|e < 0.90		= goldenrod
	|e < 0.95		= slategray
	|otherwise	= white

tempColor (Temperature t)
	|t' <= 0	= sRGB (0) (0) (abs (t'))
	|otherwise	= sRGB t' 0 0
	where
		t' = t / 100

moistColor m
	|m == 1		= white --sea
	|otherwise	= sRGB 0 0 (m)



climateColor Water =
  blue
climateColor Desert =
  khaki
climateColor Steppe =
  darkkhaki
climateColor Mediterranean =
  yellow
climateColor HotSummerContinental =
  lightseagreen
climateColor ColdContinental =
  seagreen
climateColor WetContinental =
  darkseagreen
climateColor Savannah							= yellowgreen
climateColor Monsoon							= red
climateColor Oceanic							= lightsteelblue
climateColor ColdOceanic					= steelblue
climateColor TropicalRainforest		= darkgreen
climateColor HumidSubtropical			= lightgreen
climateColor Taiga								= brown
climateColor Tundra								= gray
climateColor IceCap						= white
