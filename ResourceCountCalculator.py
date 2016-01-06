from ShortestPaths import shortestPathMatrix
from Resources import resourceMatrix, allResourceNames,\
    originalGlobalResourceCounts, productionPerReference, oneRefGoldOzInCP, allRawMatNames
from decimal import *
import copy

#set up the Decimal environment
getcontext().prec = 4

# other information for later
allCityNames = list(resourceMatrix.keys())

#TODO: assert that allCityNames, as built from resourceMatrix.keys(), is the same as shortestPathMatrix.keys()
# this is because they are both "sources of truth" about what cities exist and what their names are

def zeroDictDict(dictMatrix,aList):
    """
    Given a list aList and a dict of dicts dictMatrix, go through each inner dictionary, adding keys
    with value 0 for each entry in aList that the dict lacks as a key
    """
    for city,resList in dictMatrix.items():
        localHas = list(resList.keys())
        for a in aList:
            if a in localHas:
                pass
            else:
                dictMatrix[city][a] = 0

# use zeroDictDict to, for each city, add an key for each resource it doesn't originally have, with value zero
zeroDictDict(resourceMatrix,allResourceNames)

# this is the matrix which will have the numbers updated to reflect importing resources from other cities
# the total count for a given resource after imports, as stored in this matrix,
# will be larger than that originally contained in resourceMatrix
# this is intentional and can be thought of as the "velocity of goods"

# it needs to be a separate matrix since it will hold separate numbers from the resourceMatrix,
# and the original numbers of resourceMatrix must be left untouched
# the original resourceMatrix numbers are still required for import calculation
pricingMatrix = copy.deepcopy(resourceMatrix)

def quantityToImport(source, destination, sourceAmount):
    """
    Calculates the fraction of the sourceAmount which gets imported to destination.
    :param source: city from which resource is being imported
    :param destination: city which is receiving the import
    :param sourceAmount: the number of abstract units of some resource
    :return: the fraction of sourceAmount which will be added to the amount at destination
    """
    distance = shortestPathMatrix[source][destination]
    quantity = Decimal(sourceAmount) / (distance + 1)
    # 1 is added to distance so that when a destination is 1 away from its source, it does not receive exactly as much
    # as exists at the source
    return quantity

#this set of nested loops adds fractional amounts of the count of each resource at each city to the counts
# at each other city, relative to the distance between the two cities
for source, sourceResources in resourceMatrix.items():
    for destination in list(pricingMatrix.keys()):
        if source == destination:
            pass
        else:
            for aResource, count in sourceResources.items():
                q = quantityToImport(source,destination,count)
                #then, the all-important update line
                pricingMatrix[destination][aResource] += q


# finally, we need a price per production UNIT
# e.g. if global iron production is 10,000 tons, we want to find out what it costs for 1 ton
# this is done in two steps
# Step 1
# the baseline price per single unit is determined:
# for each raw material resource, we divide oneRefGoldOzInCP by productionPerReference
# this gets us the CP price for e.g. 1 ton of iron, not the whole 10,000 tons in a whole reference
# Step 2
# the adjusted price is determined:
# the prices produced in step 1 end up very high, so we apply the correction Alexis uses
# the arbitrary baseline is that a city whose pricing references equal 5% of the world's global references
# has no adjustment made to its price
# if it's below that, the price does get higher
# if it's above that, which most are, then the price gets lower
# for example, suppose that City A's pricing references for iron are 25% of the original global total
# (remember that after imports, total pricing refs can exceed original global total -- that's OK)
# then, to find adjusted price, we divide baseline lump at City A by (.25/.05), i.e. divide by 5

pricesPerProductionUnit = {}
for city,resources in pricingMatrix.items():
    pricesPerProductionUnit[city] = {}
    for rawMat,details in productionPerReference.items():
        referenceSize = details[0]
        baselinePrice = oneRefGoldOzInCP / referenceSize
        worldPercentage = resources[rawMat] / originalGlobalResourceCounts[rawMat]
        divisionFactor = worldPercentage / Decimal(0.05)
        adjustedPrice = baselinePrice / divisionFactor
        productionUnitPrice = adjustedPrice / referenceSize
        pricesPerProductionUnit[city][rawMat] = (productionUnitPrice, details[1])

# packaging everything up to serve as input to the recipe programs
# the key point is that if it's a service, we want the value in pricingMatrix at the given city;
# however, if it's a raw material, we only want the price in pricesPerProductionUnit (again, at the given city)
# alias to match naming convention for recipeCalculator
rawMatStorage = pricesPerProductionUnit
# its counterpart, which stores services
serviceStorage = {}
# what we want to store in serviceStorage: a transformation of pricingMatrix, which drops all non-services
# so we want to keep every subkey (and its value) from pricingMatrix as long as that subkey isn't in allRawMatNames
# do that like this:
# loop over pricingMatrix by-city
# then for items in each subdictionary, add the item to serviceStorage only if its name isn't in allRawMatList
for city, cityResources in pricingMatrix.items():
    serviceStorage[city] = {}
    for name, pricingRef in cityResources.items():
        if name in allRawMatNames:
            pass
        else:
            serviceStorage[city][name] = pricingRef
