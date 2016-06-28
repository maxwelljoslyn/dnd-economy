from decimal import *
from math import pi

#set up the Decimal environment
getcontext().prec = 4

class Recipe:
    """This class holds the structure of a product's recipe, including which resources or other recipes are needed
    to make it, and how much of those amounts are needed, the service needed to create this recipe, and the difficulty
    of doing so."""
    def __init__(self, service, unit, subRaws, subRecipes=[],difficulty=1, description=""):
        self.service = service
        self.difficulty = difficulty
        self.subRaws = subRaws
        self.subRecipes = subRecipes
        self.unit = (Decimal(unit[0]),unit[1])
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

# measurements of this and that kind
densityCastIron = Decimal(454.8)
densityWroughtIron = 483 # lb/cu. ft
densitySteel = 489 # lbs/cu ft
# giving a density for generic timber until diversified, neither too soft nor too hard
densityTimber = 40 # lb/cu ft
densityMilk = Decimal(64.48808) # 1033 kg/cu meter, converted to lb/cu ft
cuFtPerGallonLiquid = 1 / Decimal(7.48052)
milkGallonWeight = densityMilk * cuFtPerGallonLiquid
densityMolasses = Decimal(88.1233091)
molassesGallonWeight = densityMolasses * cuFtPerGallonLiquid


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
                                       [("pig iron",0.93)],
                                    description="ingot, 1x1x3.8 in.")

recipeStorage["wrought iron"] = Recipe("smelter",(4.5,"lb"),
                                       [("coal",2.25),("limestone",1.125)],
                                       [("pig iron",1)],
                                       description="ingot, 2x2x4 in.")

recipeStorage["steel"] = Recipe("smelter",(1,"lb"),
                                       [("coal",0.25),("limestone",0.25)],
                                # steel requires half as much coal as other iron stuff
                                # b/c howstuffworks says it only needs to get half as hot
                                       [("pig iron",1)],
                                difficulty = 1.5,
                                description="ingot, 1x1x3.5 in.")

hiltCuFt = ((Decimal(1) / Decimal(6)) ** 2) * (Decimal(5) / Decimal(12))
hiltWeight = densityTimber * hiltCuFt
recipeStorage["blade hilt"] = Recipe("carpenter",(hiltWeight,"lb"),
                                     [("timber",hiltCuFt)],
                                     description="wood tube, carved from 2x2x5 in. block")

recipeStorage["pommel"] = Recipe("blacksmith",(0.25,"lb"),
                                 [],
                                 [("steel",0.25)],
                                 difficulty=1.5,
                                 description="metal knob which holds hilt and blade together")
# semiGoods.append("pommel")


# a 1-foot (unit) blade is 2 inches wide, 1/6 inch thick, 1 foot long
# thus, the unit blade is (2/12) * (1/6/12) * 1 = approx 0.002 cubic feet, thus weighing ~1.134 lb
# let's round to 1.2 for ease
recipeStorage["blade"] = Recipe("blacksmith",(1.2,"lb"),
                                [],
                                [("steel",1.2)],
                                difficulty=1.5,
                                description="price for a one-foot steel blade")

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
                                difficulty=3,
                                description="Flour ground from cereals.")

recipeStorage["cattle feed"] = Recipe("miller",(1,"lb"),
                                [],
                                [("husked cereal",1)],
                                description="coarsely ground from cereals")

recipeStorage["horse feed"] = Recipe("miller",(1,"lb"),
                                [],
                                [("husked cereal",1)],
                                difficulty=2,
                                description="ground from cereals")

recipeStorage["cow"] = Recipe("farmer",(1,"head"),
                              [("arable land",10.67)],
                              [("cattle feed",424)],
                              description="two years old, suitable for slaughtering")

# http://www.personal.utulsa.edu/~marc-carlson/history/cattle.html
# this gives an average milk production of 3.5 gallons per day
dailyMilkGallons = Decimal(3.5)
# from some research, cows can give milk 300/365 days of the year (so 5/6 of the year)
# on the other hand, the production tapers off as this period goes on
# let's be ad-hoc and say that that the effective number of days of milk production is 250
# thus yearly milk production is 3.5 gallons * 250 days
avgMilkingDays = 250
yearlyMilkGallons = dailyMilkGallons * avgMilkingDays
# then divide cow price by that, to get price of milk per gallon

recipeStorage["cow milk"] = Recipe("farmer",(milkGallonWeight,"lb"),
                                   [],
                                   [("cow",Decimal(1/yearlyMilkGallons))],
                                   description = "1 gallon")

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

recipeStorage["quicklime"] = Recipe("potter",(1,"lb"),
                                    [("limestone",1),("coal",0.5)],
                                    [],
                                    description="used in tanning and to make mortar")

recipeStorage["mortar"] = Recipe("potter",(1,"lb"),
                                 [("clay",0.75)],
                                 [("quicklime",0.25)],
                                 description="in powdered form")

# I assume a cow for slaughter weights 1300 pounds
# taking the carcass weight to be 2/3 of that and the useable meat, in turn, to be 2/3 of carcass weight,
# the remaining meat is 577.7 lbs.
# thus to get a price for 1 lb, we divide the price of the cow by 577.7.
# this approach treats all beef as generic: in reality, a given cow produces different amounts
# of each cut of beef. for now we'll treat them all as the same.
recipeStorage["beef"] = Recipe("butcher",(1,"lb"),
                               [],
                               [("cow",(1/Decimal(577.7)))])

# a raw cowhide is about 50 square feet
# this includes the irregularly-shaped edge portions,
# so a nice big single square piece would only be about 40 square feet at most.
recipeStorage["raw cowhide"] = Recipe("butcher",(60,"lb"),
                                      [],
                                      [("cow",1)],
                                      description="50 square feet")
semiGoods.append("raw cowhide")

recipeStorage["defleshed cowhide"] = Recipe("tanner",(15,"lb"),
                                            [],
                                            [("raw cowhide",1)],
                                            difficulty=2,
                                            description="cowhide cleaned of flesh and/or hair")
semiGoods.append("defleshed cowhide")

# density of quicklime is 209.1337 lb/cu ft.
# this site:
# http://boar.org.uk/aaiwxw3MusprattL6Preparation.htm
# says that three to four cubic feet measure of "freshly burned fat lime" (aka quicklime)
# is used for 100 average hides
# taking 3.5 cubic feet as our measure, that means 731.96795 lbs of quicklime per 100 hides
# AKA 0.732 lbs of quicklime per hide.
recipeStorage["tanned cowhide"] = Recipe("tanner",(15,"lb"),
                                         [],
                                         [("quicklime",0.732),("defleshed cowhide",1)],
                                         description="50 sq ft of cowhide, ready for leatherwork")

recipeStorage["holy symbol, wooden, simple"] = Recipe("carpenter",(1,"lb"),
                                                      [("timber",0.02)],
                                                      [],
                                                      difficulty=2)

recipeStorage["holy symbol, iron, simple"] = Recipe("blacksmith",(1,"lb"),
                                                      [],
                                                      [("wrought iron",1/4.5)],
                                                    difficulty=2)

# note that value for the weight of the wrought iron
# it's the weight of the final object divided by the weight of one ingot of wrought iron, 4.5 lbs

recipeStorage["holy symbol, iron, ornate"] = Recipe("blacksmith",(1,"lb"),
                                                      [],
                                                      [("wrought iron",1/4.5)],
                                                    difficulty=8)

# first step in making red and yellow (ochre) dyes
# high difficulty because multiple steps in the process
# here: https://en.wikipedia.org/wiki/Ochre#Modern_history
# wiki says that clay would be about 90% junk and 10% ochre
recipeStorage["separated ochre clay"] = Recipe("potter",(1,"lb"),
                                       [("clay",10)],
                                       [],
                                       difficulty=3)
semiGoods.append("separated ochre clay")

# primary componenent of paints and of dyes
recipeStorage["pigment, red/yellow"] = Recipe("dyer",(1,"lb"),
                                              [],
                                              [("separated ochre clay",1)])

# component of blue dye
recipeStorage["ground lapis lazuli"] = Recipe("potter",(1,"lb"),
                                              [("lapis lazuli",1)])

semiGoods.append("ground lapis lazuli")

recipeStorage["pigment, ultramarine"] = Recipe("dyer",(1,"lb"),
                                               [],
                                               [("ground lapis lazuli",1)])

recipeStorage["malted grain"] = Recipe("brewer",(1,"lb"),
                               [("cereal",1)],
                               description="germinated and dried; used in brewing")

recipeStorage["roasted malt"] = Recipe("brewer",(1,"lb"),
                                       [],
                                       [("malted grain",1)])

# for volume considerations,I've approximated a 30-gallon barrel as a cylinder with radius = 0.67 ft and height = 2 and 11/12 ft.
# were it a right cylinder, this would give a volume of 4.12 cubic feet, or approx. 30.8 US gallons.
# the actual barrel is not a perfect cyllinder, but has sloping sides,
# thus it holds a bit less than 4.12 cubic feet, so I feel OK rounding off to 30 gallons.
# then there's a few more considerations going into the design of the barrel:
# the heads (top and bottom) of the barrel
# the thickness of wood used for all the wood
# and the wrought-iron hoops, of which I use 6 (two fat at the heads, four thin around the body)

# let's say the actual radius of the top/bot of the barrel is 6.5 inches
# and let's say the thickness of the heads is 2/3 inch and the staves are 1.125 inch thick
# let's say the fat hoops are 1.25 inch wide, and the thin ones are 1 inch. hoop breadth is 0.065 inch (16 gauge).

barrelHeadThickness = Decimal(2/3) / Decimal(12)
barrelHeadRadius = Decimal(6.5)/Decimal(12)
barrelHeadCircleArea = Decimal(pi) * (barrelHeadRadius ** 2)
barrelCuFt = barrelHeadCircleArea * barrelHeadThickness
barrelHeadWeight = densityTimber * barrelCuFt

recipeStorage["barrel head"] = Recipe("cooper",(barrelHeadWeight,"lb"),
                                      [("timber",barrelCuFt)],
                                      [])

barrelHeadCircumference = barrelHeadRadius * 2 * Decimal(pi)
fatHoopCuFt = barrelHeadCircumference * (Decimal(1.25) / Decimal(12)) * (Decimal(0.065) / Decimal(12))
fatHoopWeight = densityWroughtIron * fatHoopCuFt

recipeStorage["barrel hoop, fat"] = Recipe("blacksmith",(fatHoopWeight, "lb"),
                                                         [],
                                                         [("wrought iron",(fatHoopWeight / Decimal(4.5)))])

# the thin hoops are actually 1/3 and 2/3 the distance to the middle of the barrel,
# so with the sloping of the barrel, we'd use a measure less than the barrel's max circumference
# to get their length.
# but I'll just go ahead and approximate them as being the same as its radius. no need for total accuracy.
barrelBodyCircumference = Decimal(2) / Decimal(3)
thinHoopCuFt = barrelBodyCircumference * (Decimal(1) / Decimal(12)) * (Decimal(0.065) / Decimal(12))
thinHoopWeight = densityWroughtIron * thinHoopCuFt

recipeStorage["barrel hoop, thin"] = Recipe("blacksmith",(thinHoopWeight, "lb"),
                                                         [],
                                                         [("wrought iron",(thinHoopWeight / Decimal(4.5)))])

# these are quite rough measurements. so it goes.
numStaves = 12
staveWidth = barrelBodyCircumference / Decimal(numStaves)
staveThickness = (Decimal(1.125)/Decimal(12))
staveHeight = Decimal(35) / Decimal(12) # 2 and 11/12 feet high, i.e. 35 inches
staveCuFt = staveWidth * staveHeight * staveThickness
staveWeight = staveCuFt * densityTimber
recipeStorage["barrel stave"] = Recipe("cooper",(staveWeight,"lb"),
                                       [("timber",staveCuFt)],
                                       [])

# bringing it all together
barrelWeight = (numStaves * staveWeight) + (4 * thinHoopWeight) + (2 * fatHoopWeight) + (2 * barrelHeadWeight)
recipeStorage["barrel"] = Recipe("cooper",(barrelWeight, "lb"),
                                 [],
                                 [("barrel stave",numStaves),("barrel hoop, fat",2),("barrel hoop, thin",4),
                                  ("barrel head",2)],
                                 difficulty=2,
                                 description="30-gallon barrel; 0.67-ft widest radius; 2 ft 11 in. tall")

# based on the Clare household strong ale recipe from:
# https://www.cs.cmu.edu/~pwp/tofi/medieval_english_ale.html
# 8 lbs., Hugh Baird brand English Pale malt
# I use cereal for oats
# judging by how much ale is created, 2 gall (8 qts) of water is boiled off in the main batch;
# that will be useful once I start accounting for water prices

# here is the original:
# 1 1/3 lbs., (Baird) Pale malt, roasted. 
# For darker ale, roast to amber: 30 mins. at 225 F. followed by 30 mins. at 300 F. For lighter, roast an hour at 225 F.
# around 3 lbs., oats (rolled)
# 14 to 16 qts., water (main batch) 
# 14 will produce 1 1/2 gallons of ale; 16 will produce 2 gallons
# 6 to 8 qts., water (second runnings)
# 1 pkt, Danstar brand Nottingham ale yeast
# 1 pkt, Danstar brand Windsor ale yeast

def calculateABV(poundsCereal, poundsMalt, gallonsInBatch):
    # for calculating ABV:
    # the source at http://brewery.org/library/PeriodRen.html says:
    # wheat (here, cereal) is 75% fermentable sugar,
    # malt is 80%.
    # specific gravity of sugar solution is (1 + 0.039 * sugar in grains) / water used) [or just final volume of the beverage]
    cerealSugar = Decimal(0.75) * poundsCereal
    maltSugar = Decimal(0.8) * poundsMalt
    originalGravity = 1 + Decimal(0.039) * ((cerealSugar + maltSugar) / gallonsInBatch)
    # then we use the formula given in the brewery.org source for ABV to find a theoretical maximum strength for this alcohol, and then we'll take 80% of that as our final number to represent realities of brewing creeping in
    theoreticalMaxABV = (originalGravity - 1) * Decimal(135)
    realABV = Decimal(0.8) * theoreticalMaxABV
    return realABV

weightWater = Decimal(8.345404)

aleCerealAmt = Decimal(45)
aleMaltAmt = Decimal(120)
aleRoastedMaltAmt = Decimal(19.95)
aleBatchGallons = 30
# I calculated an original gravity of 1.190
# also, for both beer and ale I just use the weight of water to determine the weight. good enough for me.
recipeStorage["ale"] = Recipe("brewer",((aleBatchGallons * weightWater)+barrelWeight,"lb"),
                                      [("cereal",aleCerealAmt)],
                                       [("barrel",1),("malted grain",aleMaltAmt),("roasted malt",aleRoastedMaltAmt)],
                                       description="30-gallon barrel; " + str(calculateABV(aleCerealAmt,(aleMaltAmt + aleRoastedMaltAmt),aleBatchGallons)) + "% alcohol")

# "To brewe beer; 10 quarters malt. 2 quarters wheat, 2 quarters oats, 40 lbs hops. To make 60 barrels of single beer."
# this is one of the recipes taken from http://brewery.org/library/PeriodRen.html
# a "quarter" equals 256 lbs of grain (b/c it's 64 gallons of dry volume and 1 gallon of grain ~= 4 lbs)
# a medieval barrel of beer was 36 gallons
# thus we have 2560 lbs malt, 512 lbs + 512 lbs = 1024 lbs cereal, 40 lbs hops = 2160 gallons.
# divide all amounts by 72 to arrive at a 30-gallon recipe for a 30-gallon barrel, same as ale above.
# that's what we have here.
# to be consistent, I'll do the ABV calc here myself, even though the source lists a number
beerCereal = Decimal(14.22)
beerMalt = Decimal(35.55)
beerGallons = 30
recipeStorage["beer"] = Recipe("brewer",((beerGallons*weightWater)+barrelWeight,"lb"),
                               [("cereal",14.22),("hops",0.55)],
                               [("barrel",1),("malted grain",35.55)],
                               description="30-gallon barrel; " + str(calculateABV(beerCereal, beerMalt, beerGallons)) + "% alcohol")

recipeStorage["mature ewe"] = Recipe("farmer",(1,"head"),
                                    [("arable land",Decimal(0.394))],
                                    [],
                                    description="eight months old, suitable for milking or shearing")

recipeStorage["mutton sheep"] = Recipe("farmer",(1,"head"),
                                    [("arable land",Decimal(1.11375))],
                                    [],
                                    description="one year four months, suitable for slaughter")

# one mature ewe produces ~ 200 lbs of milk, once a year during lambing
# thus the division by 200
recipeStorage["sheep milk"] = Recipe("farmer",(milkGallonWeight,"lb"),
                                     [],
                                     [("mature ewe",Decimal(1/200))],
                                     description = "1 gallon")
# sheep for slaughter weighs 120 lbs
# I take the dress percentage to be 55% of that, giving the hanging weight, and the useable meat to be 75% of the hanging weight
sheepUseableMeat = 120 * Decimal(0.55) * Decimal(0.75)
# we divide the cost of a mutton sheep by this number to get a price for 1 lb mutton
recipeStorage["mutton"] = Recipe("butcher",(1,"lb"),
                                     [],
                                     [("mutton sheep",Decimal(1/sheepUseableMeat))])

# production figures for greasy wool vary wildly, so I'll go with one sheep producing 25 lbs of greasy wool, which can be turned into 15 lbs of scoured wool (which must then be pounded)
recipeStorage["greasy wool"] = Recipe("farmer",(25,"lb"),
                                      [],
                                      [("mature ewe",1)])

semiGoods.append("greasy wool")

recipeStorage["fuller's earth"] = Recipe("potter",(1,"lb"),
                                         [("clay",3)],
                                         [],
                                         description = "certain clays used for scouring wool")
semiGoods.append("fuller's earth")

# again, I've decided that one 25-lb unit of greasy wool becomes 15 lbs of scoured
# wool is cleaned by scouring (washing) and using clay
recipeStorage["scoured wool"] = Recipe("fuller",(15,"lb"),
                                       [],
                                       [("fuller's earth",1),("greasy wool",1)])
semiGoods.append("scoured wool")

# final step in cleaning wool is pounding, which is done by mills
recipeStorage["clean wool"] = Recipe("miller",(1,"lb"),
                                     [],
                                     [("scoured wool",Decimal(1/15))])

# brown (or "raw") sugar, which still contains some molasses
# cane can yield 50% of its mass in juice; approximately 20% of that juice is sugar
# that means 10 lbs of sugarcane to get 1 lb of sugar
# as for molasses, recipe is essentially the same as brown sugar, except for the ratio of inputs to product
# the sugar in the sugarcane juice is 20% of the juice mass;
# we can say the remaining 80% of the juice mass turns into molasses; thus 50% * 80% = 4 lbs of molasses per 10 lbs of sugarcane
# its' more than 4 lbs of molasses per gallon -- its about 12. so we divide the molasses per-gallon weight by 4 lbs, to get the number we multiply that 10 lbs of sugarcane by, to get the sugarcane for 1 gallon of molasses
sugarcaneForOneGallonMolasses = 10 * molassesGallonWeight / 4

recipeStorage["sugar"] = Recipe("miller",(1,"lb"),
                                      [("sugarcane",10)],
                                      [],
                                      difficulty=4,
                                      description="brown sugar")

recipeStorage["molasses"] = Recipe("miller",(molassesGallonWeight,"lb"),
                                   [("sugarcane",sugarcaneForOneGallonMolasses)],
                                   [],
                                   difficulty=4,
                                   description="1 gallon")

# 6 month old pig for the slaughter, weighing 150 lbs
recipeStorage["pig"] = Recipe("farmer",(1, "head"),
                              [],
                              [("cattle feed",630)],
                              description="6 months old, 150 lbs, ready for slaughter")

# going off the web, carcass weight is 75% of live weight, and dress weight is 75% of carcass weight
pigDressPercentageOfLiveWeight = Decimal(0.75) * Decimal(0.75)
# times weight of slaughtered pig
pigDressWeight = pigDressPercentageOfLiveWeight * 150
recipeStorage["pork"] = Recipe("butcher",(1,"lb"),
                               [],
                               [("pig",Decimal(1/pigDressWeight))])
