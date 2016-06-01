class Recipe:
    """This class holds the structure of a product's recipe, including which resources or other recipes are needed
    to make it, and how much of those amounts are needed, the service needed to create this recipe, and the difficulty
    of doing so."""
    def __init__(self, service, unit, difficulty=1):
        self.service = service
        self.difficulty = difficulty
        self.subRaws = []
        self.subRecipes = []
        self.unit = unit
        self.description = ""
        # description of item, including dimensions, weight, properties
        # there may be some subclasses of Recipe specific to particular item types, such as Weapon and Armor,
        # since those have special details (AC, damage dice, break chance, etc)

# keys in this dict are recipe names, used in printing
# values are Recipe objects
# when defining a Recipe, it is added to this dict
recipeStorage = {}

# semiGoods are those items whose prices need to be calculated to produce more complex items,
# but which themselves are not sold at market.
# the quintessential example is pig iron, which is brittle and mostly useless, but is the first step in making steel.
# if a key is stored in semiGoods, then it will not be printed by recipeRunner.main()
semiGoods = []


recipeStorage["pig iron"] = Recipe("smelter",(1, "kg"))
recipeStorage["pig iron"].subRaws = [("iron ore",1),
                                     ("coal",5),
                                     ("limestone",0.1)]
semiGoods.append("pig iron")
