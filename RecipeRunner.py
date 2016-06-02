from decimal import *
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
    serviceCost = componentCost * serviceModifier * recipe.difficulty
    finalCost = componentCost + serviceCost
    return finalCost

def showCost(city, name):
    """Show the price of a recipe 'name' at 'city'."""
    price = findCost(city, name)
    return (name + ": " + str(price) + " CP\nUnit: " + str(arg.unit[0]) + " " + arg.unit[1])

# TODO: parameterize to cities named on the command line (any number of)
def main():
    for n in recipeStorage:
        if n in semiGoods:
            pass
        else:
            for t in towns:
                print("At",t," price of",n,":",showCost(t,n))
                

if __name__ == "__main__":
    main()
