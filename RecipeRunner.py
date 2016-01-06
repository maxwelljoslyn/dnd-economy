from decimal import *
from ResourceCountCalculator import rawMatStorage, serviceStorage
from RecipeDefinitions import *

#set up the Decimal environment
getcontext().prec = 6

def findCost(city, recipe):
    rawMatSum = 0
    for r,c in recipe.subRaws:
        base = rawMatStorage[city][r][0]
        multiplied = base * c
        rawMatSum += multiplied
    subRecipeSum = 0
    for r,c in recipe.subRecipes:
        base = findCost(city, r)
        # recursion
        multiplied = base * c
        subRecipeSum += multiplied
    componentCost = rawMatSum + subRecipeSum
    serviceNum = serviceStorage[city][recipe.service]
    serviceModifier = (1 / serviceNum)
    serviceCost = componentCost * serviceModifier * recipe.difficulty
    finalCost = componentCost + serviceCost
    return finalCost

def printCost(city, name):
    """Print the price of a recipe 'name' at 'city'."""
    arg = recipeStorage[name]
    price = findCost(city, arg)
    return (name + ": " + str(price) + " CP\nUnit: " + str(arg.unit[0]) + " " + arg.unit[1])

# TODO: parameterize to cities named on the command line (any number of)
def main():
    for n in recipeStorage:
        if n in semiGoods:
            pass
        else:
            print(printCost("A", n))
            # currently just testing with City A

if __name__ == "__main__":
    main()