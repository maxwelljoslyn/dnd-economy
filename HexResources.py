import random
import itertools
import bisect
from decimal import Decimal
from TownInfo import towns


# choose randomly from a population built from a list of choices with weights
def weightedChoice(weightedlist):
    choices, weights = zip(*weightedlist)
    cumulativeDist = list(itertools.accumulate(weights))
    x = random.random() * cumulativeDist[-1]
    return choices[bisect.bisect(cumulativeDist,x)]

def stripWeights(weightedList):
    """Helper function to return only the names in a resource list, not its weights."""
    return [member[0] for member in weightedList]

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
                result[s] += Decimal(count)
            else:
                result[s] = Decimal(count)
    # then, add some extra services from the list, for more variety
    extraServs = random.randint(1,6)
    while extraServs > 0:
        e = random.choice(serviceList)
        if e in result:
            result[e] += Decimal(1)
        else:
            result[e] = Decimal(1)
        extraServs = extraServs - 1
    return result

# this dictionary holds world production figures, with both amounts and units thereof,
# for each of the resources listed below.
# these are currently listed at 1/1000 of their full world size
worldProductionMatrix = {}
worldProductionMatrix["flax"] = (10000000,"lb")
worldProductionMatrix["timber"] = (2000000, "cuft")
worldProductionMatrix["arable land"] = (10000000, "acre")
worldProductionMatrix["clay"] = (10000000, "lb")
worldProductionMatrix["cereal"] = (10000000, "lb")
worldProductionMatrix["hops"] = (1000000, "lb")
worldProductionMatrix["sugarcane"] = (1000000, "lb")
worldProductionMatrix["cod"] = (500000, "lb")
worldProductionMatrix["herring"] = (500000, "lb")
worldProductionMatrix["tobacco"] = (1943618, "lb")
worldProductionMatrix["cotton"] = (10000000, "lb")

worldProductionMatrix["iron ore"] = (1000000000,"lb") 
worldProductionMatrix["manganese ore"] = (80000000, "lb")
worldProductionMatrix["nickel ore"] = (60000000, "lb")
worldProductionMatrix["zinc ore"] = (40000000,"lb")
worldProductionMatrix["copper ore"] = (20000000, "lb")
worldProductionMatrix["cobalt ore"] = (5000000, "lb")
worldProductionMatrix["lead ore"] = (2500000, "lb")
worldProductionMatrix["tin ore"] = (1250000, "lb")
worldProductionMatrix["silver ore"]= (625000, "lb")
worldProductionMatrix["cinnabar"] = (312500, "lb")
# cinnabar = ore from which mercury is refined
worldProductionMatrix["platinum ore"] = (156250, "lb")
worldProductionMatrix["gold ore"] = (78125, "lb")
worldProductionMatrix["mithril ore"] = (40000, "lb")
worldProductionMatrix["adamantine ore"] = (20000, "lb")
worldProductionMatrix["lapis lazuli"] = (50000, "lb")

stoneAndMinerals = ["chalk","coal","salt","talc","emery","granite",
                    "marble","slate","flint","obsidian","phosphorus",
                    "witherite","sulfur", "limestone"]

for sm in stoneAndMinerals:
    worldProductionMatrix[sm] = (1000000,"lb")
worldProductionMatrix["limestone"] = (2000000,"lb")


preciousGems = [("ruby",10),("emerald",10),("topaz",10),("sapphire",10),
                ("diamond",3)]

for pg in preciousGems:
    name = pg[0]
    worldProductionMatrix[name] = (1000 * pg[1], "lb")

ornamentalGems = ["agate","azurite","cat's eye","hawk's eye","hematite",
                    "malachite","lapis lazuli","mother-of-pearl","quartz",
                    "tiger eye","turquoise"]

for og in ornamentalGems:
    worldProductionMatrix[og] = (50000,"lb")
