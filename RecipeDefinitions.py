class Recipe:
    """This class holds the structure of a product's recipe, including which resources or other recipes are needed
    to make it, and how much of those amounts are needed, the service needed to create this recipe, and the difficulty
    of doing so."""
    def __init__(self, service, unit, subRaws, subRecipes=[],difficulty=1):
        self.service = service
        self.difficulty = difficulty
        self.subRaws = subRaws
        self.subRecipes = subRecipes
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

recipeStorage["pig iron"] = Recipe("smelter",(1, "lb"),
                                   [("iron ore",1),("coal",0.5),("limestone",0.25)])
semiGoods.append("pig iron")

recipeStorage["cast iron"] = Recipe("smelter",(1, "lb"),
                                    # first the components which go into cast iron
                                       [("manganese ore",0.06),
                                        ("nickel ore",0.01),
                                    # then the components to do heating and thus smelting
                                        ("coal",0.5),
                                        ("limestone",0.25)],
                                       [("pig iron",0.93)])

recipeStorage["wrought iron"] = Recipe("smelter",(1,"lb"),
                                       [("coal",0.5),("limestone",0.25)],
                                       [("pig iron",1)])

recipeStorage["steel"] = Recipe("smelter",(1,"lb"),
                                       [("coal",0.25),("limestone",0.25)],
                                # steel requires half as much coal as other iron stuff
                                # b/c howstuffworks says it only needs to get half as hot
                                       [("pig iron",1)],
                                difficulty = 1.25)

# next: add the various pieces that come together to make a sword

