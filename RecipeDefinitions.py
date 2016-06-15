class Recipe:
    """This class holds the structure of a product's recipe, including which resources or other recipes are needed
    to make it, and how much of those amounts are needed, the service needed to create this recipe, and the difficulty
    of doing so."""
    def __init__(self, service, unit, subRaws, subRecipes=[],difficulty=1, description=""):
        self.service = service
        self.difficulty = difficulty
        self.subRaws = subRaws
        self.subRecipes = subRecipes
        self.unit = unit
        self.description = description
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

# cast iron: ~454.8 lb/cuft
recipeStorage["cast iron"] = Recipe("smelter",(1, "lb"),
                                    # first the components which go into cast iron
                                       [("manganese ore",0.06),
                                        ("nickel ore",0.01),
                                    # then the components to do heating and thus smelting
                                        ("coal",0.5),
                                        ("limestone",0.25)],
                                       [("pig iron",0.93)],
                                    description="ingot, 1x1x3.8 in.")

# wrought iron: ~483 lb/cu. ft
recipeStorage["wrought iron"] = Recipe("smelter",(4.5,"lb"),
                                       [("coal",2.25),("limestone",1.125)],
                                       [("pig iron",1)],
                                       description="ingot, 2x2x4 in.")

# steel: ~489 lb/cuft
recipeStorage["steel"] = Recipe("smelter",(1,"lb"),
                                       [("coal",0.25),("limestone",0.25)],
                                # steel requires half as much coal as other iron stuff
                                # b/c howstuffworks says it only needs to get half as hot
                                       [("pig iron",1)],
                                difficulty = 1.1,
                                description="ingot, 1x1x3.5 in.")

# carved from ash, beech, or elm
recipeStorage["blade hilt"] = Recipe("carpenter",(0.5,"lb"),
                                     [("timber",0.01)],
                                     description="wood tube, carved from 2x2x5 in. block")

recipeStorage["pommel"] = Recipe("blacksmith",(0.25,"lb"),
                                 [],
                                 [("steel",0.25)],
                                 difficulty=1.1,
                                 description="metal knob which holds hilt and blade together")
# semiGoods.append("pommel")


recipeStorage["blade"] = Recipe("blacksmith",(1.2,"lb"),
                                [],
                                [("steel",1.2)],
                                difficulty=1.5,
                                description="price for a one-foot steel blade")
# density of steel: 490 lbs/cubic foot
# a 1-foot (unit) blade is 2 inches wide, 1/6 inch thick, 1 foot long
# thus, the unit blade is (2/12) * (1/6/12) * 1 = approx 0.002 cubic feet, thus weighing ~1.134 lb
# let's round to 1.2 for ease

recipeStorage["dagger"] = Recipe("blacksmith",(1.95,"lb"),
                                 [],
                                 [("blade",1),("pommel",1),("blade hilt",1)],
                                 description="1d4 damage")

recipeStorage["shortsword"] = Recipe("blacksmith",(3.15,"lb"),
                                 [],
                                 [("blade",2),("pommel",1),("blade hilt",1)],
                                 description="1d6 damage")

recipeStorage["longsword"] = Recipe("blacksmith",(4.95,"lb"),
                                 [],
                                 [("blade",3.5),("pommel",1),("blade hilt",1)],
                                 description="1d8 damage")



# let's do some foods, plus cattle feed so we do cow prices and all the stuff coming from that

recipeStorage["husked cereal"] = Recipe("miller",(1,"lb"),
                                         [("cereal",1)],
                                         [])
semiGoods.append("husked cereal")

recipeStorage["flour"] = Recipe("miller",(1,"lb"),
                                [],
                                [("husked cereal",1)],
                                difficulty=1.4,
                                description="Flour ground from cereals.")

recipeStorage["cattle feed"] = Recipe("miller",(1,"lb"),
                                [],
                                [("husked cereal",1)],
                                difficulty=1.2,
                                description="Feed coarsely ground from cereals.")

recipeStorage["cow, beef"] = Recipe("farmer",(1,"head"),
                              [("arable land",10.67)],
                              [("cattle feed",424)],
                              description="suitable for slaughtering")


recipeStorage["bread, coarse"] = Recipe("baker",(1,"lb"),
                                       [("salt",0.05)],
                                       [("flour",0.7)],
                                       description="small round loaf")

recipeStorage["bread, good"] = Recipe("baker",(1,"lb"),
                                     [("salt",0.05)],
                                     [("flour",0.7)],
                                     difficulty=2,
                                     description="small round loaf")

recipeStorage["bread, excellent"] = Recipe("baker",(1,"lb"),
                                     [("salt",0.05)],
                                     [("flour",0.7)],
                                     difficulty=4,
                                     description="small round loaf")

