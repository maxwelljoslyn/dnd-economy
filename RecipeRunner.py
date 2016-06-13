from decimal import *
from math import ceil
from RecipeDefinitions import *
from ResourcePriceCalculator import towns, pricesPerProductionUnit

#set up the Decimal environment
getcontext().prec = 6

def findCost(city, name):
    rawMatSum = 0
    recipe = recipeStorage[name]
    for r,count in recipe.subRaws:
        base = pricesPerProductionUnit[city][r][0]
        multiplied = base * Decimal(count)
        rawMatSum += multiplied
    subRecipeSum = 0
    for r,count in recipe.subRecipes:
        # recursion! this can eventually be made more efficient by memoization,
        # i.e. by storing the result of each call to findCost in a dictionary,
        # then wrapping findCost in a method which, when given a recipe name,
        # first looks in that dictionary to see if the recipe cost has already been calculated
        base = findCost(city, r)
        # recursion
        multiplied = base * Decimal(count)
        subRecipeSum += multiplied
    componentCost = rawMatSum + subRecipeSum
    serviceNum = towns[city].services[recipe.service]
    serviceModifier = (1 / serviceNum)
    serviceCost = componentCost * serviceModifier * Decimal(recipe.difficulty)
    finalCost = componentCost+ serviceCost
    return finalCost

def getDisplayPrice(priceInCP):
    """Given an exact price in decimal coppers, return the rounded price,
    formatted for display in appropriate amounts of copper/silver/gold."""
    # first, round up: the difference represents merchant and tradesman profit
    rounded = ceil(priceInCP)
    # now we break it into GP, SP, and CP
    numGold = rounded // 100
    remaining = rounded - (numGold * 100)
    numSilv = remaining // 10
    remaining = remaining - (numSilv * 10)
    numCopp = remaining
    # quick test
    if (numGold * 100 + numSilv * 10 + numCopp != rounded):
        raise ValueError("RecipeRunner: unequal calculation inside getDisplayPrice.")
    else:
        pass
    # format result for returning
    resultChunks = []
    if numGold == 0:
        pass
    else:
        resultChunks.append(str(numGold) + " GP")
    if numSilv == 0:
        pass
    else:
        resultChunks.append(str(numSilv) + " SP")
    if numCopp == 0:
        pass
    else:
        resultChunks.append(str(numCopp) + " CP")
    result = ", ".join(resultChunks)
    return result

def display(city, name):
    """Show the price of a recipe 'name' at 'city'."""
    recipe = recipeStorage[name]
    basePrice = findCost(city, name)
    displayPrice = getDisplayPrice(basePrice)
    return "{0:20}: {1:>16}|{2:>6} {3:6}|{4}".format(name,displayPrice,str(recipe.unit[0]),recipe.unit[1],recipe.description)

# TODO: parameterize to cities named on the command line (any number of)
def main():
    # testing with town Veder Vek
    t = "Veder Vek"
    print("At",t + ":")
    for n in recipeStorage:
        if n in semiGoods:
            pass
        else:
            print(display(t,n))

if __name__ == "__main__":
    main()
