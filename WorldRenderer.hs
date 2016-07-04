{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.SRGB
import Diagrams.TwoD.Text
import Diagrams.TwoD.Arrow
import Diagrams.CubicSpline
import Diagrams.TwoD.Offset

import WorldParser
import TownParser
import RoadParser

import Text.Parsec
import Control.Monad (liftM)
import Data.Map (Map, (!))
import qualified Data.Map as DM

main = do
	worldInfo <- parseWorld
	--having parsed in the world data, now we can proceed to rendering
	--we have to do all the rendering under the patternmatch Right branch
	--why? because we can't know ahead of time,
        --whether resultOfParse is Left or Right.
	--the only way to extract the data inside, which we need for parsing,
        --is a pattern match
        townInfo <- parseTownFile
        roadInfo <- parseRoadFile
        case townInfo of
          Left err ->
            mainWith (triangle 1 # fc pink # lw thick :: Diagram B)
          Right ts -> 
	    case worldInfo of
              Left err ->
                mainWith (circle 1 # fc pink # lw thick :: Diagram B)
              Right hs ->
                case roadInfo of
                  Left err -> mainWith (square 1 # fc pink # lw thick :: Diagram B)
                  Right rs ->
                    mainWith $ drawFullWorld hs townMap rs
                    where
                      townMap = DM.fromList ts

drawFullWorld :: [Hex] -> Map Coord TownData -> [Road] -> Diagram B
drawFullWorld hs ts rs =
  drawTownLayer pts ts filtered
  `atop`
  drawRoadLayer rs
  `atop`
  drawHexLayer pts filtered
  where
    pts :: [P2 Double]
    pts = map (coordToPixel . coord) filtered
    filtered = filterHexes wanted hs
    --coord positions of each hex

wanted = [Coord q r (0-q-r) | q <- [30..55], r <-[(-75),(-74)..(-55)]]

--remove a hex from hs if its coord isn't in cs
filterHexes :: [Coord] -> [Hex] -> [Hex]
filterHexes cs hs =
  filter (\h -> (coord h) `elem` cs) hs

drawRoadLayer :: [Road] -> Diagram B
drawRoadLayer rs = foldr (atop) mempty $ fmap drawRoad rs

drawRoad :: Road -> Diagram B
drawRoad (Road cs) =
  (position (zip pts (repeat mempty)) <> cubicSpline False pts) # lc orchid # lw veryThin # lineCap LineCapRound
  where
    pts = fmap coordToPixel cs

drawHexLayer :: [P2 Double] -> [Hex] -> Diagram B
drawHexLayer pts hs = atPoints pts $ map drawHex hs

drawHex :: Hex -> Diagram B
drawHex h =
  alignedText 0.5 0 qAndR # fc black # scale 0.5
  `atop`
  alignedText 0.5 0.75 (show . cs . coord $ h) # fc black # scale 0.5
  `atop`
  hexagon 1 # fc climateColor' # lc black # lw veryThin
  where
    qAndR = (show . cq . coord $ h) ++ (',' : (show . cr . coord $ h))
    climateColor' = climateColor $ climate h
    moistColor' = moistColor $ moist h
    elevColor' = elevColor (elev h) (isLand h)
    onlyColorLandBorder = if (isLand h) == True then black else elevColor'
    --onlyColorLandBorder creates weird image,
    --b/c hexes render in strange order,
    --and some border colors overlap onto others,
    --creating weird "bites" taken out of some hexes.

drawTownLayer :: [P2 Double] -> Map Coord TownData -> [Hex] -> Diagram B
drawTownLayer pts ts hs = atPoints pts $ map (drawTown ts) hs

drawTown :: Map Coord TownData -> Hex -> Diagram B
drawTown ts h = case hasTown of
  Nothing -> mempty
  Just (TownData n) -> baselineText n # fc white # scale 0.5
  where
    hasTown = DM.lookup (coord h) ts

coordToPixel :: Coord -> P2 Double
coordToPixel (Coord q r s) = p2 (x,y)
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
climateColor Savannah =
  yellowgreen
climateColor Monsoon =
  red
climateColor Oceanic =
  lightsteelblue
climateColor ColdOceanic =
  steelblue
climateColor TropicalRainforest =
  darkgreen
climateColor HumidSubtropical =
  lightgreen
climateColor Taiga =
  brown
climateColor Tundra =
  gray
climateColor IceCap =
  white

regionColor r
  |r == 0 = blue
  |r == 1 = orange
  |r == 3 = brown
  |r == 12 = brown
  |r == 18 = yellow
  |r `elem` aColors = lightpink
  |r `elem` bColors = mediumorchid
  |r `elem` cColors = crimson
  |otherwise = olive
  where
    aColors = [1,8,13]
    bColors = [2,3,4,6,12,15]
    cColors = [5,7,16,17,18,20]
