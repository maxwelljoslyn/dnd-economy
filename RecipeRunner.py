from decimal import *
from math import ceil
from RecipeDefinitions import *
from ResourcePriceCalculator import towns, pricesPerProductionUnit, pseudoAverageRefPercent

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
    serviceCost = (componentCost * serviceModifier * Decimal(recipe.difficulty)) * pseudoAverageRefPercent
    finalCost = componentCost + serviceCost
    return finalCost

def getDisplayPrice(priceInCP):
    """Given an exact price in Decimal coppers, return the rounded price,
    formatted for display in appropriate amounts of copper/silver/gold."""
    # first, round up: the difference represents merchant and tradesman profit
    rounded = ceil(priceInCP)
    if rounded < 100:
        # present in coppers
        return (str(rounded) + " CP")
    elif rounded < 1000:
        # present in silvers
        return (str(ceil(rounded / 10)) + " SP")
    else:
        # in gold
        return (str(ceil(rounded / 100)) + " GP")

def display(city, name):
    """Show the price of a recipe 'name' at 'city'."""
    recipe = recipeStorage[name]
    basePrice = findCost(city, name)
    displayPrice = getDisplayPrice(basePrice)
    displayWeight = str(Decimal(recipe.weight[0]).quantize(Decimal('0.01')))
    displayUnitCount = str(Decimal(recipe.unit[0]).quantize(Decimal('0.01')))
    displayUnitName = recipe.unit[1]
    if recipe.unit == recipe.weight:
        displayUnitCount = "<--"
        displayUnitName = ""
    return "{0:30}| {1:>10}|{2:>8} {3:>2}|{4:>8} {5:6}|{6}".format(name,displayPrice,displayWeight,recipe.weight[1],displayUnitCount,displayUnitName,recipe.description)


# TODO: parameterize to cities named on the command line (any number of)
def main():
    town = "Veder Vek"
    print("At",town + ":")
    names = list(recipeStorage.keys())
    names.sort()
    print("{0:30}  {1:>10} {2:>8} {3:>2} {4:>8} {5:6} {6}".format("Name","Price","Weight","","Units","","Description"))
    for n in names:
        if n in semiGoods:
            pass
        else:
            print(display(town,n))
    print("Number of recipes:",len(names))

if __name__ == "__main__":
    main()
