import random
import itertools
import bisect

# choose randomly from a population built from a list of choices with weights
def weightedChoice(weightedlist):
    choices, weights = zip(*weightedlist)
    cumulativeDist = list(itertools.accumulate(weights))
    x = random.random() * cumulativeDist[-1]
    return choices[bisect.bisect(cumulativeDist,x)]

def stripWeights(weightedList):
    """Helper function to return only the names in a resource list, not its weights."""
    return [member[0] for member in weightedList]

def resourceToServices(res):
    """Returns the appropriate service or services (always in a list, even if just one)
    for a given resource."""
    servs = []
    if res == "honeybee":
        servs.append("beekeeper")
    if res == "silkworm":
        servs.append("weaver")
        servs.append("tailor")
    if res in stripWeights(livestock):
        servs.append("tanner")
        servs.append("leatherworker")
        servs.append("butcher")
    if res in stripWeights(savannahOnlyBeasts + tropicalOrSavannahBeasts + coldClimateBeasts + desertBeasts + otherBeasts):
        for x in ["hunter","tanner","leatherworker","butcher"]:
            servs.append(x)
    if res in stripWeights(waterBeasts):
        servs.append("fishmonger")

    if res in (stripWeights(metalOres) + stoneAndMinerals):
        servs.append("smelter")
        servs.append("blacksmith")
        servs.append("assayer")
    if res in stripWeights(stoneAndMinerals):
        servs.append("stonecarver")
        servs.append("mason")
    if res in (stripWeights(preciousGems) + ornamentalGems):
        servs.append("jeweler")
    if res == "tin ore":
        servs.append("tinsmith")
    if res == "silver ore":
        servs.append("silversmith")
    if res == "gold ore":
        servs.append("goldsmith")

    if res in crops:
        for x in ["miller","brewer","baker"]:
            servs.append(x)
    if res == "tobacco":
        servs.append("tobacconist")
    if res in stripWeights(alchemyPlants):
        servs.append("alchemist")
    if res == "olive":
        servs.append("miller") #olives are pressed at a mill
    if res == "grape":
        servs.append("vintner")
    if res == "timber":
        servs.append("carpenter")

    return servs

# all of the above services are also listed here, for random distribution
serviceList = ["beekeeper","weaver","tailor","tanner","leatherworker","butcher",
               "hunter","fishmonger","smelter","assayer","tinsmith","goldsmith",
               "stonecarver","mason","jeweler","silversmith","blacksmith",
               "miller","brewer","baker","tobacconist","alchemist","vintner",
               "carpenter"]

def getServices(resourceDict):
    """Given a dict mapping from resource names to counts of those resources,
    return the appropriate services (occupational resources, like blacksmith and chandler)
    which are appropriate for those resources."""
    result = {}
    for name,count in resourceDict.items():
        servs = resourceToServices(name)
        for s in servs:
            if s in result:
                result[s] += count
            else:
                result[s] = count
    # then, add some extra services from the list, for more variety
    extraServs = random.randint(1,6)
    while extraServs > 0:
        e = random.choice(serviceList)
        if e in result:
            result[e] += 1
        else:
            result[e] = 1
        extraServs = extraServs - 1
    return result

# these lists define various categories of resource
# when resources are generated for a hex, these lists are consulted,
# with the exact selection of lists determined by climate.
# chance of getting a particular result in a list is determined by its weight

# weights for livestock are high so that rarer beasts can have a low weight which is still an integer
livestock = [("cow",5),("chicken",5),("duck",5),("sheep",5),("pig",5),
             ("goat",5),("horse",5),("cat",5),("dog",5),("donkey",5),
             ("mule",5),("silkworm",3),("turkey",5),("rabbit",5),
             ("honeybee",5)]

savannahOnlyBeasts = [("giraffe",5),("lion",5),("elephant",5),("zebra",5)]

# can be found in the savannah or in any of:
# rainforest, monsoon, humid subtropical
tropicalOrSavannahBeasts = [("tiger",5),("orangutan",5),("panther",5),
                            ("gorilla",5),("hippo",5),("alligator",5),
                            ("crocodile",5)]

# only choices for: tundra, ice cap
coldClimateBeasts = [("fish",5),("mammoth",3),("remorhaz",1)]

# found in most places; some are rare.
otherBeasts = [("fish",5),("deer",5),("snake",5),
               ("frog",5),("wolf",5),("bird",5),("bear",5),
               ("boar",5),("scorpion",5),("owlbear",3),("bulette",1),
               ("griffin",1),("ankheg",3),("roc",1),("pegasus",1)]

desertBeasts = [("scorpion",5),("camel",5),("ankheg",5),("snake",5)]
        
# can found in any hex which has 1+ Water neighbors
waterBeasts = [("crab",5),("eel",5),("lobster",5),("whale",3),
               ("dolphin",3),("squid",5),("octopus",5),("oyster",1)]

metalOres = [("iron ore",14),("manganese ore",13),("nickel ore",12),
             ("zinc ore",11),("copper ore",10),("cobalt ore",9),
             ("lead ore",8),("tin ore",7),("silver ore",6),
             ("cinnabar",5),("platinum ore",4),("gold ore",3),
             ("mithril ore",2),("adamantine ore",1)]
# cinnabar = ore from which mercury is refined

# these are considered to have the same weight, so weights are elided;
# they can be chosen among with the function random.choice
stoneAndMinerals = ["chalk","coal","salt","talc","emery","granite",
                    "marble","slate","flint","obsidian","phosphorus",
                    "witherite","sulfur"] + (["limestone"] * 2)

preciousGems = [("ruby",10),("emerald",10),("topaz",10),("sapphire",10),
                ("diamond",3)]

ornamentalGems = ["agate","azurite","cat's eye","hawk's eye","hematite",
                    "malachite","lapis lazuli","mother-of-pearl","quartz",
                    "tiger eye","turquoise"]

# vegetables are mainly tropical vs. non-tropical
# for vegetables, a "tropical" variable means monsoon, subtropical, or rainforest;
# non-tropical means anything else other than icecap/desert (no veggies there)
vegetables = ["carrot","lettuce","potato","bean","tomato","squash","cucumber",
              "onion","garlic","sweet pepper"]

tropicalVegetables = vegetables + ["luffa"]

crops = ["cereal","cavebloom","hops","flax","poppy"]

tropicalCrops = ["sugarcane","tobacco","coffee bean","tea","cotton","hemp"]

# tundra are a special case, since I allow certain frost-tolerant crops to grow there
tundraCrops = ["cavebloom","flax"]

spices = [("mint",5),("basil",5),("thyme",5),("rosemary",5)]

tropicalSpices = [("black pepper",7),("cacao",5),("vanilla",5),("saffron",1)]

# I don't bother with a trop/non-trop distinction here
alchemyPlants = [("nightshade",5),("wolfsbane",5),("black lotus",1),("hemlock",5)]

# there are fewer fruit categories than climates since some climates share a category
# fruit/climate pairings:
# subarctic fruit: taiga, tundra, cold oceanic
# temperate fruit: steppe, all continental, oceanic
# mediterranean fruit: mediterranean
# subtropical fruit: subtropical
# tropical fruit: rainforest, monsoon
temperateFruits = ["apple","pear","cherry","plum","apricot","peach","nectarine",
                  "blackberry","elderberry","raspberry","strawberry"]

subarcticFruits = ["blueberry","cranberry","huckleberry","lingonberry"]

mediterraneanFruits = ["olive","date","fig","grape","pomegranate"]

subtropicalFruits = ["lemon","lime","grapefruit","avocado","peanut"]

tropicalFruits = ["allspice","banana","coconut","dragonfruit","durian",
                  "grape","grapefruit","guava","kiwifruit","lemon",
                  "lime","mango","oil palm","olive","orange","papaya",
                  "pecan","pineapple","pistachio","watermelon"]

allFruits = temperateFruits + subarcticFruits + mediterraneanFruits + subtropicalFruits + tropicalFruits

