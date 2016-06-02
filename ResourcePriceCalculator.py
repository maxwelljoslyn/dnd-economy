from decimal import *
import copy
from WorldGenerator import towns, shortestPathMatrix
from HexResources import referenceProductionMatrix

#set up the Decimal environment
getcontext().prec = 4

# get names of all resources available in the economy
# get names of all services available in the economy
allResourceNames = []
allServiceNames = []
for t,d in towns.items():
    allResourceNames = allResourceNames + list(d.resources.keys())
    allServiceNames = allServiceNames + list(d.services.keys())
allResourceNames = list(set(allResourceNames))
allServiceNames = list(set(allServiceNames))

# for each town, for each service and resource it doesn't have an entry for,
# add one with a count of 0, so that stuff can be added to it in the import step
# without having to muddy that code with repetitive checking for null values
for t,d in towns.items():
    for r in allResourceNames:
        if r in d.resources:
            pass
        else:
            d.resources[r] = Decimal(0)
    for s in allServiceNames:
        if s in d.services:
            pass
        else:
            d.services[s] = Decimal(0)

# now we create a deep copy of the towns data.
# why? because for importing we're going to be updating resource and service values in place,
# but in order to perform the import calculations,
# we need to be able to refer to the original values.

originalTowns = copy.deepcopy(towns)
# set up the original, pre-import counts of each resource (not service)
# these are used later, when establishing the size of a reference
originalWorldResourceCounts = {}
for ot,d in originalTowns.items():
    for r,val in d.resources.items():
        if r in originalWorldResourceCounts:
            originalWorldResourceCounts[r] += val
        else:
            originalWorldResourceCounts[r] = val

def quantityToImport(source, destination, sourceAmount):
    """
    Calculates the fraction of the sourceAmount which gets imported to destination.
    :param source: city from which resource is being imported
    :param destination: city which is receiving the import
    :param sourceAmount: the number of abstract units of some resource (or service)
    :return: the fraction of sourceAmount which will be added to the amount at destination
    """
    distance = shortestPathMatrix[source][destination]
    quantity = Decimal(sourceAmount) / (distance + 1)
    # 1 is added to distance so that when a destination is 1 away from its source, it does not receive exactly as much
    # as exists at the source
    return quantity

# now: the import step
# we base the quantity of each resource to be imported on the ORIGINAL amount available,
# and add the appropriate fraction of that amount to each importing town
for source, sourceData in originalTowns.items():
    for dest, destData in towns.items():
        if source == dest:
            pass
        else:
            for res, count in sourceData.resources.items():
                q = quantityToImport(source, dest, count)
                destData.resources[res] += q
            for serv, count in sourceData.services.items():
                q = quantityToImport(source, dest, count)
                destData.services[serv] += q

# now: finding the value of a gold-ore reference in copper pieces.
# 1 oz gold ore equals 8 gold pieces
oneOzGoldOreInGP = Decimal(8)
# (it's not actually that much metal, that's just its official value)
# we'll let 1 oz = 28.35 grams
oneOzInGrams = Decimal(28.35)
# thus there are 28.35 grams to 8 gold pieces; thus a price of approx 282.19 gp/kg gold ore
goldOreKGinGP = oneOzGoldOreInGP * 1000 / oneOzInGrams
# 1 gold piece is equivalent to 100 copper pieces
goldOreKGinCP = goldOreKGinGP * 100
# thus we arrive at the copper-piece value of one reference of gold ore,
# which we also take as the CP value of one reference of ANY material resource.

# finally, we need a price per production UNIT
# e.g. if the size of an iron reference is 1,000 tons, we want to find what 1 ton costs
# Step 1
# the baseline price per single unit is determined:
# for each raw material resource, we divide oneRefGoldOzInCP by productionPerReference
# this gets us the CP price for e.g. 1 ton of iron, not the whole 1,000 tons in a whole reference
pricesPerProductionUnit = {}
for t,d in towns.items():
    pricesPerProductionUnit[t] = {}
    for rawMat in allResourceNames:
        referenceSize, unit = referenceProductionMatrix[rawMat]
        baseUnitPrice = goldOreKGinCP / referenceSize
        # local references to resource, divided by world references, times unit price
        localRefs = d.resources[rawMat]
        worldRefs = originalWorldResourceCounts[rawMat]
        ratio = localRefs / worldRefs
        localUnitPrice = baseUnitPrice / ratio
        pricesPerProductionUnit[t][rawMat] = (localUnitPrice, unit)

# and with that, we are ready to move into the final economy step: recipes!
# in which we use these raw material prices together with services
# to arrive at prices for actual items which the players can buy

def main():
    print("\n")
    for t,vals in pricesPerProductionUnit.items():
        print(t)
        for name in allResourceNames:
            price, unit = vals[name]
            print(name,"costs",str(price),"CP per",unit)

if __name__ == "__main__":
    main()
