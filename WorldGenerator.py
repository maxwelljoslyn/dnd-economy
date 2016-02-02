import noise
from enum import Enum
from decimal import *
from math import pi
from HexResources import *

# set up the Decimal environment
getcontext().prec = 6

# number of rings on the world map, with 0 = just the center hex
mapSize = 80

# equatorial diameter of the planet
# remember, I'm only generating one hemisphere right now
# one hex is 20 miles, and diameter == radius * 2pi
planetDiameter = mapSize * 20 * 2 * pi
# mapSize needed to match earth's equatorial diameter: 198

# All possible valid cube coordinates for a given mapSize,
# for a hexagon-shaped world map.
# In cube coordinates, there are three values,
#S not two as with cartesian coordinates,
# but the third value (denoted "s") is dependent on the other two.
# I store it so it doesn't have to be calculated every time;
# thus, there's less risk of an arithmetic mistake popping up elsewhere.
possibleCoords = set()
# algorithm (for the coords fitting into a hexagon-shaped hexmap) sourced from:
# http://www.redblobgames.com/grids/hexagons/implementation.html
for q in range(-mapSize, mapSize+1):
    rStart = max(-mapSize, -q - mapSize)
    rEnd = min(mapSize, -q + mapSize)
    for r in range(rStart, rEnd+1):
        possibleCoords.add((q,r,0-q-r))

class Direction(Enum):
    """The six directions on a grid of flat-topped hexagons: up-left, up,
    up-right, down-right, down, down-left.
    These aren't called NW/northwest, TN/true north, etc. because on my map,
    the coordinate (0,0,0), the center of the map, is the North Pole.
    So going north on the map is not the same as going up."""
    UL = (-1,1,0)
    UP = (0,1,-1)
    UR = (1,0,-1)
    DR = (1,-1,0)
    DN = (0,-1,1)
    DL = (-1,0,1)


def addCoord(coord1, coord2):
    """Addition of cubic coordinates."""
    q = coord1[0] + coord2[0]
    r = coord1[1] + coord2[1]
    s = coord1[2] + coord2[2]
    return q, r, s

def getNeighbor(coord, direction):
    """Return coord's neighboring coordinate in the given direction.
    Calculate what that neighbor is,
    regardless of validity of its coord for the mapSize.
    If it would be off the grid (i.e. is not in possibleCoords), return None.
    Otherwise, return the neighbor's coordinate as calculated."""
    intermediate = addCoord(coord, direction.value)
    if intermediate in possibleCoords:
        return intermediate
    else:
        return None

class HexData:
    """Stores the metadata information associated with a given hexagon."""
    def __init__(self):
        self.elevation = Decimal()
        self.temperature = Decimal()
        self.isLand = None
        self.neighbors = {}
        self.moisture = Decimal(0)
        self.climate = ""
        self.resources = {}
        self.services = {}

def cubeDistance(a,b):
    """Distance from a to b on a hexagonal grid,
    where a and b are cube coordinates."""
    return((abs(a[0] - b[0]) + abs(a[1] - b[1]) + abs(a[2] - b[2])) / 2)

def tempAtCoord(coord):
    """Return a heat number for the hex at coord.
    Low 'never hot', high means 'always hot',
    and in between means variation, meaning seasonal heat changes."""
    distanceFromCenter = cubeDistance((0,0,0),coord)
    # 0 at center hex, 1 on first ring, 2 on next ring, etc
    minTemp = Decimal(-30)
    # temp at (0,0,0), the center of the map and the North Pole
    maxTemp = Decimal(90)
    # temp at outer edge of map, the equator
    stepSize = Decimal((abs(minTemp) + maxTemp) / mapSize)
    adjustment = Decimal(distanceFromCenter) * stepSize
    return(minTemp + adjustment)

# TODO: write my own noise generator (see AmitP noise page)
def initialize():
    """Build up the HexData for each coord on the grid."""
    worldModel = {}
    for coord in possibleCoords:
        worldModel[coord] = HexData()
    octaves = 4
    frequency = 12.0 * octaves

    # first data-assignment for loop
    for coord, data in worldModel.items():
        # assign elevation
        q = coord[0]
        r = coord[1]
        data.elevation = (Decimal(noise.snoise3(q/frequency, r/frequency, 2, octaves)) + Decimal(1))
        # this will be changed in the next loop, it's just a starting point
        data.temperature = tempAtCoord(coord)
        # fill in neighbors list
        for name, member in Direction.__members__.items():
            data.neighbors[name] = getNeighbor(coord, member)

    # normalize terrain to be between 0 and 1
    elevations = [d.elevation for c,d in worldModel.items()]
    elevMax = max(elevations)
    elevMin = min(elevations)
    seaLevel = 0.46
    # this value gives most aesthetically-pleasing shapes for landmasses, islands

    # first for loop: assign elevation, land/sea
    for coord, data in worldModel.items():
        normalizedElevation = (data.elevation - elevMin) / (elevMax - elevMin)
        data.elevation = normalizedElevation

        # land/sea distinction
        if data.elevation >= seaLevel:
            data.isLand = True
        else:
            data.isLand = False

    # next for loop
    # moistureLine requires all the elevations to be assigned
    # before it works properly,
    # which means the previous loop has to be completely done before proceeding.
    # because there are six moisture passes,
    # any of which might alter part of the map,
    # we have to do all of them before proceeding onto the next assignment loop.
    for coord, data in worldModel.items():
        # initial conditions for moisture: 0 (default) for land, 1 for water
        if data.isLand == False:
            data.moisture = Decimal(1)

        def moistureLine(aCoord, aDir):
            """Move moisture from one hex to another.
            Defined within this loop because it uses generated elevations."""
            currentHex = worldModel[aCoord]
            currentMoisture = currentHex.moisture

            # abort if moisture is too low to keep spreading
            while currentMoisture > 0.3:
                neighborLocation = currentHex.neighbors[aDir]
                if neighborLocation is None:
                    break
                neighbor = worldModel[neighborLocation]
                if neighbor.isLand is False:
                    break
                else:
                    if neighbor.elevation < currentHex.elevation:
                        neighbor.moisture = max(neighbor.moisture, Decimal(0.8) * currentMoisture)
                    else:
                        deltaElevation = abs(currentHex.elevation - neighbor.elevation)
                        neighbor.moisture = max(neighbor.moisture, currentMoisture - (Decimal(2.5) * Decimal(deltaElevation)))
                currentHex = neighbor
                currentMoisture = neighbor.moisture

        # starting at the sea hexes, move in each direction,
        # spreading moisture as we go
        if data.isLand is False:
            for name, member in Direction.__members__.items():
                moistureLine(coord, name)

    # climate assignment
    for coord, data in worldModel.items():
        m = data.moisture
        t = data.temperature
        if m == 1:
            data.climate = "Water"
        elif m < 0.25 and t >= 60:
            data.climate = "Desert"
        elif m < 0.25 and t >= 45:
            data.climate = "Mediterranean"
        elif m < 0.25 and t >= 0:
            data.climate = "HotSummerContinental"
        elif m < 0.25:
            data.climate = "Tundra"
        elif m < 0.5 and t >= 60:
            data.climate = "Savannah"
        elif m < 0.5 and t >= 45:
            data.climate = "Mediterranean"
        elif m < 0.5 and t >= 0:
            data.climate = "HotSummerContinental"
        elif m < 0.5 and t >= -15:
            data.climate = "ColdContinental"
        elif m < 0.5:
            data.climate = "IceCap"
        elif m < 0.75 and t >= 75:
            data.climate = "Monsoon"
        elif m < 0.75 and t >= 60:
            data.climate = "Steppe"
        elif m < 0.75 and t >= 15:
            data.climate = "Oceanic"
        elif m < 0.75 and t >= 0:
            data.climate = "ColdOceanic"
        elif m < 0.75 and t >= -15:
            data.climate = "Taiga"
        elif m < 0.75:
            data.climate = "IceCap"
        elif m >= 0.75 and t >= 75:
            data.climate = "TropicalRainforest"
        elif m >= 0.75 and t >= 45:
            data.climate = "HumidSubtropical"
        elif m >= 0.75 and t >= 15:
            data.climate = "WetContinental"
        elif m >= 0.75 and t >= -15:
            data.climate = "Taiga"
        else:
            data.climate = "IceCap"

    # resource assignment
    for coord, data in worldModel.items():
        if data.isLand is False:
            pass
        else:
            # Different resources are available based on climate.
            # Some are given automatically (e.g. polar bear for icecaps).
            # Some are added to a "choices" variable, which means they
            # have a chance of being selected as a resource at that hex.
            # And some climates have no resources of a certain kind (e.g. icecap has no plants or fruits).
            # animalChoices must be predefined since the water-beast-adding code uses it:
            animalChoices = []
            if data.climate == "IceCap":
                animalChoices = coldClimateBeasts
                fruitChoices = None
                vegChoices = None
                cropChoices = None
                spiceChoices = None
                alchemyChoices = None
                hasTimber = False
            if data.climate == "Tundra":
                animalChoices = coldClimateBeasts
                fruitChoices = subarcticFruits
                vegChoices = None
                cropChoices = tundraCrops
                spiceChoices = None
                alchemyChoices = None
                hasTimber = False
            if data.climate == "Taiga":
                animalChoices = livestock + otherBeasts + coldClimateBeasts
                fruitChoices = subarcticFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = True
            if data.climate == "Desert":
                animalChoices = desertBeasts
                fruitChoices = None
                vegChoices = None
                cropChoices = None
                spiceChoices = None
                alchemyChoices = None
                hasTimber = False
            if data.climate == "Steppe":
                animalChoices = livestock + otherBeasts
                fruitChoices = temperateFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = False
            if data.climate == "HotSummerContinental":
                animalChoices = livestock + otherBeasts
                fruitChoices = temperateFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = True
            if data.climate == "ColdContinental":
                animalChoices = livestock + otherBeasts
                fruitChoices = subarcticFruits + temperateFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = True
            if data.climate == "WetContinental":
                animalChoices = livestock + otherBeasts
                fruitChoices = temperateFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = True
            if data.climate == "Oceanic":
                animalChoices = livestock + otherBeasts
                fruitChoices = temperateFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = True
            if data.climate == "ColdOceanic":
                animalChoices = livestock + otherBeasts
                fruitChoices = subarcticFruits = temperateFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = True
            if data.climate == "Mediterranean":
                animalChoices = livestock + otherBeasts
                fruitChoices = mediterraneanFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = True
            if data.climate == "Savannah":
                animalChoices = savannahOnlyBeasts + tropicalOrSavannahBeasts + [(name,(count-2)) for name,count in livestock] + [("fish",2),("snake",2),("bird",2)]
                fruitChoices = temperateFruits
                vegChoices = vegetables
                cropChoices = crops
                spiceChoices = spices
                alchemyChoices = alchemyPlants
                hasTimber = True
            if data.climate == "Monsoon":
                animalChoices = [(name,(count-2)) for name,count in livestock] + tropicalOrSavannahBeasts
                fruitChoices = tropicalFruits
                vegChoices = tropicalVegetables
                cropChoices = tropicalCrops
                alchemyChoices = alchemyPlants
                spiceChoices = tropicalSpices
                hasTimber = True
            if data.climate == "HumidSubtropical":
                animalChoices = [(name,(count-2)) for name,count in livestock] + tropicalOrSavannahBeasts
                fruitChoices = subtropicalFruits
                vegChoices = tropicalVegetables
                cropChoices = tropicalCrops
                alchemyChoices = alchemyPlants
                spiceChoices = tropicalSpices
                hasTimber = True
            if data.climate == "TropicalRainforest":
                animalChoices = [(name,(count-2)) for name,count in livestock] + tropicalOrSavannahBeasts
                fruitChoices = tropicalFruits
                vegChoices = tropicalVegetables
                cropChoices = tropicalCrops
                alchemyChoices = alchemyPlants
                spiceChoices = tropicalSpices
                hasTimber = True
            
            # todo auto resources: ice for icecap; timber for taiga plus tropicals
            # if there's a non-land neighbor, allow the chance of water beasts in normal animal generation
            actualNeighbors = [b for a,b in data.neighbors.items() if b is not None]
            waterNeighbors = [x for x in actualNeighbors if worldModel[x].isLand is False]
            if waterNeighbors == []:
                pass
            else:
                animalChoices = animalChoices + waterBeasts

            # generate number of resources for this hex
            chances = 3 * [1,2,3] + 4 * [4,5,6] + 3 * [7,8,9,10]
            chanceResourceCount = random.choice(chances)
            # note: random seed is already set in HexResources

            while chanceResourceCount > 0:
                choice = None
                # 1/3 chance to get animal, metal/mineral/stone, or plant
                whichResource = random.randint(1,3)
                if whichResource == 1:
                    choice = weightedChoice(animalChoices)
                elif whichResource == 2:
                    # 50% chance for a metal, 40% for a stone/mineral
                    # 9% for ornamental, 1% for gem
                    picker = random.randint(1,100)
                    if picker <= 50:
                        choice = weightedChoice(metalOres)
                    elif picker <= 90:
                        choice = random.choice(stoneAndMinerals)
                    elif picker <= 99:
                        choice = random.choice(ornamentalGems)
                    else:
                        choice = weightedChoice(preciousGems)
                else:
                    # fruit: 25%, veg: 15%, crops: 25%
                    # spices: 10%, alchemy: 10%, timber: 15%
                    picker = random.randint(1,100)
                    if picker <= 25:
                        if fruitChoices is None:
                            choice = None
                        else:
                            choice = random.choice(fruitChoices)
                    elif picker <= 40:
                        if vegChoices is None:
                            choice = None
                        else:
                            choice = random.choice(vegChoices)
                    elif picker <= 65:
                        if cropChoices is None:
                            choice = None
                        else:
                            choice = random.choice(cropChoices)
                    elif picker <= 75:
                        if spiceChoices is None:
                            choice = None
                        else:
                            choice = weightedChoice(spiceChoices)
                    elif picker <= 85:
                        if alchemyChoices is None:
                            choice = None
                        else:
                            choice = weightedChoice(alchemyChoices)
                    else:
                        if hasTimber:
                            choice = "timber"
                        else:
                            choice = None

                if choice is None:
                    chanceResourceCount -= 1
                    continue
                else:
                    # increment value if there, otherwise start at 1
                    if choice in data.resources:
                        data.resources[choice] += 1
                    else:
                        data.resources[choice] = 1
                
                chanceResourceCount -= 1

    # creating cities in some, but not all, hexes, and giving hex resources to them
    marketModel = {}
    for coord,data in worldModel.items():
        resCount = sum(list(data.resources.values()))
        # market chance:
        # base chance of having a market is based on num of resources.
        # a resource is counted multiple times if there are multiple abstract units of it
        chanceOfMarket = sum(list(data.resources.values())) * 5
        x = random.randint(1,100)
        if x <= chanceOfMarket:
            n = makeMarketName()
            # this loop prevents duplication of names
            # it gets slower as you add more cities since they all have to be checked
            while n in marketModel:
                print(n,"is already used.")
                n = makeMarketName()
                print("now it's:",n)
            marketModel[n] = coord
        else:
            pass

    # set up market populations

    return (worldModel,marketModel)

# this is v1
# one day there will be a version which has a different name gen scheme
# for each of several regions defined on the map
def makeMarketName():
    numSoundPairs = random.randint(2,6)
    vowels = ["a","e","i","o","u"]
    cons = ["b","c","d","f","g","h","j","k","l","m","n","p","r","s","t","v","w","z"]
    result = []
    # starting consonant is made uppercase
    result.append(str.upper(random.choice(cons)))
    while numSoundPairs > 0:
        result.append(random.choice(vowels))
        result.append(random.choice(cons))
        numSoundPairs -= 1
    return "".join(result)


# todo: city population assignment, based on resource count: something like "within (2^x)*1000 and (2^(x+1)*1000)-1,
# todo: where x is the num of RANDOM resources at the city" (since we might do the bonus water beast)
def main():
    worldModelReady, marketModelReady = initialize()
    counter = {}
    with open("inputWorldParser.txt", "w") as f:
        for c,d in worldModelReady.items():
            outputString = "Hex Coord " + str(c) + \
                           " Elevation " + str(d.elevation) + \
                           " Temperature " + str(d.temperature) + \
                           " Land " + str(d.isLand) + \
                           " Moisture " + str(d.moisture) + \
                           " Climate " + d.climate + "\n"
            f.write(outputString)
            # count up total generated resources (without considering whether they are available thru a market)
            for x,y in d.resources.items():
                if x in counter:
                    counter[x] += y
                else:
                    counter[x] = 1
    totalWorldCount = sorted([(x,y) for (y,x) in counter.items()])
    print(totalWorldCount)

    print("\n\n\n")
    # count up those resources which ARE available through a market
    accumulator = {}
    for n,c in marketModelReady.items():
        reses = worldModelReady[c].resources
        for r,count in reses.items():
            if r in accumulator:
                accumulator[r] += count
            else:
                accumulator[r] = 1
    totalAccumulator = sorted([(x,y) for (y,x) in accumulator.items()])
    print(totalAccumulator)

    with open("inputMarketParser.txt", "w") as f:
        for n,c in marketModelReady.items():
            outputString = "Market Coord " + str(c) + " Name " + n + "\n"
            f.write(outputString)


if __name__ == "__main__":
    main()
