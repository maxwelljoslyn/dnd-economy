from decimal import *

# set up decimal environment
getcontext().prec = 4

# dict of dicts
# outer dict keys = cities
# inner dict keys = resource names
# inner dict vals = resource amounts, in abstract units (Alexis notation: "references")
# like the data in directPaths, resourceMatrix is part of the hard data about the game world
# that is, it's inputted from somewhere else (my determination of which cities have which resources, done elsewhere)
resourceMatrix =\
{"A":
     {"iron_ore": Decimal(1), "coal": Decimal(2), "blacksmith": Decimal(1)},
 "B":
     {"iron_ore": Decimal(1), "fish": Decimal(2)},
 "C":
     {"fish": Decimal(1), "iron_ore": Decimal(1), "puddler": Decimal(1), "blacksmith": Decimal(1)},
 "D":
     {"lumber": Decimal(1), "gold_ore": Decimal(1)},
 "E":
     {"fish": Decimal(1), "mithril_ore": Decimal(1)}
}

# intermediate used to arrive at intermediate, listWithDupes
vals = []
for v in list(resourceMatrix.values()):
    vals.append(list(v.keys()))

# intermediate used to arrive at useful definition, allResourceNames
listWithDupes = []
for v in vals:
    for each in v:
        listWithDupes.append(each)

# remove dupes
allResourceNames = set(listWithDupes)

# total count for each resources in resourceMatrix, adding up from each city
# this will be put to use in e.g. determining what % of the world's references a city has,
# which is turn used to help control pricing
originalGlobalResourceCounts = {}
for res in allResourceNames:
    originalGlobalResourceCounts[res] = 0
    for city,resources in resourceMatrix.items():
        if res not in resources:
            pass
        else:
            originalGlobalResourceCounts[res] += resources[res]

# these are global production numbers for raw materials
# notice that services, such as blacksmith and puddler, are not listed here
# this is because services do not have production values, they are measured only by abstract units ("references")
globalProductionMatrix =\
{
    "iron_ore" :
        (1000000000, "oz"),
    "fish" :
        (50000000, "oz"),
    "coal" :
        (10000000, "oz"),
    "lumber" :
        (2000000, "sqft"),
    "gold_ore" :
        (500000, "oz"),
    "mithril_ore" :
        (400000, "oz")
}

allRawMatNames = list(globalProductionMatrix.keys())

# this divides the numbers in globalProductionMatrix according to the global count of each raw material,
# as listed in originalGlobalResourceCounts
# this gives us a production figure for each raw material in concrete units (kg, sq meters, etc),
# instead of abstract ones ("counts" or "references"),
# but divided up so that we know exactly how much production each reference represents
productionPerReference = {}
for rawMat,productionDetails in globalProductionMatrix.items():
    amount = productionDetails[0] # getting the first thing out of the tuple
    productionPerReference[rawMat] = (amount / originalGlobalResourceCounts[rawMat], productionDetails[1])

# ounces of gold_ore are the baseline for pricing
goldOzToCP = 8 * 100 # 1 gold oz equals 8 gold pieces; 1 gold piece equals 100 copper coins
# then, we want to convert to the smallest unit of money for the easiest calculations, the copper piece
oneRefGoldOzInCP = productionPerReference["gold_ore"][0] * goldOzToCP # sets value of 1 ref of gold ore in GP
# this is considered to be the value in gold pieces of one reference of any other good
# because an axiom of the system is that references are a measure of importance i.e. value


# see resourceCountCalculator for the final steps in the pricing code, stored in pricesPerProductionUnit
