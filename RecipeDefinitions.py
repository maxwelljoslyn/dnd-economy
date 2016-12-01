from decimal import *
from math import pi

#set up the Decimal environment
getcontext().prec = 4

class Recipe:
    """This class holds the structure of a product's recipe, including which resources or other recipes are needed
    to make it, and how much of those amounts are needed, the service needed to create this recipe, and the difficulty
    of doing so."""
    def __init__(self, service, weight, subRaws, subRecipes=[], unit=None, difficulty=1, description=""):
        self.service = service
        self.difficulty = difficulty
        self.subRaws = subRaws
        self.subRecipes = subRecipes
        self.weight = (Decimal(weight[0]),weight[1])
        if unit is None:
            self.unit = self.weight
        else:
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
densityTallow = Decimal(54.09)
densityTin = Decimal(456.3484)
densityCopper = 559
densityClay = 100
densitySilver = Decimal(655.4934)
percentageClayInGlass = Decimal(0.8)

def cylinderCuFt(height,radius):
    height = Decimal(height)
    radius = Decimal(radius)
    val = Decimal(pi) * height * (radius ** 2)
    return val

def sphereCuFt(radius):
    val = Decimal(pi) * Decimal(4/3) * (Decimal(radius) ** 3)
    return val

def coneCuFt(height, radius):
    """Volume of a cone."""
    return Decimal(pi) * (radius ** 2) * (height / Decimal(3.0))

def squarePyramidCuFt(edge,height):
    """Volume of a right square pyramid."""
    return ((Decimal(edge) ** 2) * (height)) / Decimal(3)
def triangularPrismCuFt(base,height,thickness):
    """The volume of a prism having two triangular faces and three quadrilateral ones.
    Like two 2D triangles with the area between them filled in."""
    base = Decimal(base)
    height = Decimal(height)
    thickness = Decimal(thickness)
    val = ((base * height) / 2) * thickness
    return val

def getUnitSize(name):
    """Convenience method to get size of unit."""
    return recipeStorage[name].unit[0]

def getWeight(name):
    """Convenience method to get weight in pounds."""
    return recipeStorage[name].weight[0]


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
pewterTinProportion = Decimal(0.85)
pewterCopperProportion = Decimal(0.15)
# this volume calculation works because we're making a 1 lb ingot, so the number of pounds = the proportion
volumePewterIngot = (pewterTinProportion / densityTin) + (pewterCopperProportion / densityCopper)
densityPewter = 1 / volumePewterIngot # because it's a 1 lb ingot over whatever the volume turned out to be
# comes out to 469.3 lbs/cubic ft
# checked it for sanity and the density looks good for an alloy with this much copper.
recipeStorage["pewter"] = Recipe("smelter",(1,"lb"),
                                 [("tin ore",pewterTinProportion),("copper ore",pewterCopperProportion),
                                  ("coal",Decimal(0.5)),("limestone",Decimal(0.5))],
                                 [],
                                 description="ingot, 1x1x3.65 in.")


recipeStorage["silver"] = Recipe("smelter",(1,"lb"),
                                 [("silver ore",1),("coal",Decimal(0.5)),("limestone",Decimal(0.5))],
                                 [],
                                 description="ingot, 1.5x1.5x1.175 in.")


bronzeCopperProportion = Decimal(0.88)
bronzeTinProportion = Decimal(0.12)
volumeBronzeIngot = (bronzeTinProportion / densityTin) + (bronzeCopperProportion / densityCopper)
densityBronze = 1 / volumeBronzeIngot
# comes out to 544.4 lb/cuft
recipeStorage["bronze"] = Recipe("smelter",(1,"lb"),
                                 [("tin ore",bronzeTinProportion),("copper ore",bronzeCopperProportion),
                                  ("coal",Decimal(0.5)),("limestone",Decimal(0.5))],
                                 [],
                                 description="ingot, 1.125x1.675x1.675 in.")

bellmetalCopperProportion = Decimal(0.78)
bellmetalTinProportion = Decimal(0.22)
volumeBellmetalIngot = (bellmetalTinProportion / densityTin) + (bellmetalCopperProportion / densityCopper)
densityBellmetal = 1 / volumeBellmetalIngot
# comes out to 532.8 lb/cuft, almost the same as bronze but not quite
recipeStorage["bell metal"] = Recipe("smelter",(1,"lb"),
                                 [("tin ore",bellmetalTinProportion),("copper ore",bellmetalCopperProportion),
                                  ("coal",Decimal(0.5)),("limestone",Decimal(0.5))],
                                 [],
                                 description="ingot, 1.2x1.35x2 in.")

recipeStorage["wrought iron"] = Recipe("smelter",(1,"lb"),
                                       [("coal",Decimal(0.5)),("limestone",Decimal(0.25))],
                                       [("pig iron",1)],
                                       description="ingot, 1x1x3.57 in.")

recipeStorage["steel"] = Recipe("smelter",(1,"lb"),
                                       [("coal",0.25),("limestone",0.25)],
                                # steel requires half as much coal as other iron stuff
                                # b/c howstuffworks says it only needs to get half as hot
                                       [("pig iron",1)],
                                description="ingot, 1x1x3.5 in.")

hiltCuFt = ((Decimal(1) / Decimal(6)) ** 2) * (Decimal(5) / Decimal(12))
hiltWeight = densityTimber * hiltCuFt
recipeStorage["blade hilt"] = Recipe("carpenter",(hiltWeight,"lb"),
                                     [("timber",hiltCuFt)],
                                     description="wood tube, carved from 2x2x5 in. block")
semiGoods.append("blade hilt")

recipeStorage["pommel"] = Recipe("blacksmith",(0.25,"lb"),
                                 [],
                                 [("steel",0.25)],
                                 description="metal knob which holds hilt and blade together")
semiGoods.append("pommel")


# a 1-foot (unit) blade is 2 inches wide, 1/6 inch thick, 1 foot long
unitBladeCuFt = Decimal(2/12) * Decimal(Decimal(1/6)/12) * 1
unitBladeWeight = unitBladeCuFt * densitySteel
recipeStorage["blade"] = Recipe("blacksmith",(unitBladeWeight,"lb"),
                                [],
                                [("steel",unitBladeWeight)],
                                description="price for a one-foot steel blade")
semiGoods.append("blade")

daggerWeight = getWeight("pommel") + getWeight("blade hilt") + getWeight("blade")
recipeStorage["dagger"] = Recipe("blacksmith",(daggerWeight,"lb"),
                                 [],
                                 [("blade",1),("pommel",1),("blade hilt",1)],
                                 description="1d4 damage, melee or thrown 2/3/4; 1-foot blade")

shortswordWeight = getWeight("pommel") + getWeight("blade hilt") + (Decimal(2) * getWeight("blade"))
recipeStorage["shortsword"] = Recipe("blacksmith",(shortswordWeight,"lb"),
                                 [],
                                 [("blade",2),("pommel",1),("blade hilt",1)],
                                 description="1d6 damage; 2-foot blade")

longswordWeight = getWeight("pommel") + getWeight("blade hilt") + (Decimal(3.5) * getWeight("blade"))
recipeStorage["longsword"] = Recipe("blacksmith",(longswordWeight,"lb"),
                                 [],
                                 [("blade",3.5),("pommel",1),("blade hilt",1)],
                                 description="1d8 damage; 3.5-foot blade")

greatswordWeight = getWeight("pommel") + getWeight("blade hilt") + (Decimal(4.5) * getWeight("blade"))
recipeStorage["greatsword"] = Recipe("blacksmith",(greatswordWeight,"lb"),
                                 [],
                                 [("blade",4.5),("pommel",1),("blade hilt",1)],
                                 description="1d10 damage; 4.5-foot blade")


# let's do some foods, plus cattle feed so we do cow prices and all the stuff coming from that

recipeStorage["husked cereal"] = Recipe("miller",(1,"lb"),
                                         [("cereal",1)],
                                         [])
semiGoods.append("husked cereal")

recipeStorage["flour"] = Recipe("miller",(1,"lb"),
                                [],
                                [("husked cereal",1)],
                                description="Flour ground from cereals.")

recipeStorage["cattle feed"] = Recipe("miller",(1,"lb"),
                                [],
                                [("husked cereal",1)],
                                description="coarsely ground from cereals")

recipeStorage["horse feed"] = Recipe("miller",(1,"lb"),
                                [],
                                [("husked cereal",1)],
                                description="ground from cereals")

recipeStorage["bread"] = Recipe("baker",(1,"lb"),
                                       [("salt",0.05)],
                                       [("flour",0.7)],
                                       description="round loaf")

recipeStorage["quicklime"] = Recipe("potter",(1,"lb"),
                                    [("limestone",1),("coal",0.5)],
                                    [],
                                    description="used in tanning and to make mortar")

recipeStorage["mortar"] = Recipe("potter",(1,"lb"),
                                 [("clay",0.75)],
                                 [("quicklime",0.25)],
                                 description="in powdered form")

recipeStorage["mature ewe"] = Recipe("farmer",(90,"lb"),
                                     [("arable land",Decimal(0.315))],
                                     [],
                                     unit = (1,"head"),
                                     description="eight months old, suitable for milking or shearing")

# a lamb which has been grain finished for slaughter
recipeStorage["mutton sheep"] = Recipe("farmer",(130,"lb"),
                                    [],
                                    [("mature ewe",1),("horse feed",345)],
                                    unit=(1,"head"),
                                    description="one year old, suitable for slaughter")

# one mature ewe produces ~ 200 lbs of milk, once a year during lambing
# thus the division by 200
recipeStorage["sheep milk"] = Recipe("farmer",(milkGallonWeight,"lb"),
                                     [],
                                     [("mature ewe",Decimal(1/200))],
                                     unit=(1,"gallon"))
# sheep for slaughter weighs 120 lbs
# I take the dress percentage to be 55% of that, giving the hanging/carcass weight, and the useable meat to be 75% of the hanging weight
sheepCarcassWeight = 120 * Decimal(0.55)
sheepMeatWeight = sheepCarcassWeight * Decimal(0.75)
# we divide the cost of a mutton sheep by this number to get a price for 1 lb mutton
recipeStorage["mutton"] = Recipe("butcher",(1,"lb"),
                                     [],
                                     [("mutton sheep",Decimal(1/sheepMeatWeight))])

cowSlaughterWeight = 800
recipeStorage["cow"] = Recipe("farmer",(cowSlaughterWeight,"lb"),
                              [("arable land",5.28)],
                              [("cattle feed",246)],
                              unit=(1,"head"),
                              description="1 year and 4 months old, suitable for slaughtering")

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
                                   unit=(1,"gallon"))

# I assume a cow for slaughter weighs 1300 pounds
# taking the carcass weight to be 2/3 of that and the useable meat, in turn, to be 2/3 of carcass weight,
# the remaining meat is 577.7 lbs.
# thus to get a price for 1 lb, we divide the price of the cow by 577.7.
# this approach treats all beef as generic: in reality, a given cow produces different amounts
# of each cut of beef. for now we'll treat them all as the same.
cowCarcassWeight = Decimal(cowSlaughterWeight*2) / Decimal(3)
cowMeatWeight = Decimal(cowCarcassWeight*2) / Decimal(3)
recipeStorage["beef"] = Recipe("butcher",(1,"lb"),
                               [],
                               [("cow",(1/cowMeatWeight))])

# above I said that 2/3 of the cow's carcass weight was meat
# let's assume that 2/3 of the other 1/3 of the carcass weight (i.e. 2/9) is fat, and the other 1/9 is innards, bones, horn, hoof, etc
cowFatWeight = Decimal(2/9) * cowCarcassWeight
# furthermore, since we are working toward a price for suet, which is not ALL fat but just certain portions,
# we'll divide the above weight in half below (or equivalently, treat it as requiring 2 lbs of cow per lb of suet)
recipeStorage["suet"] = Recipe("butcher",(1,"lb"),
                               [],
                               [("cow",(2/cowFatWeight))],
                               description="beef fat for cooking, or for manufacture of tallow")

# using 2 lbs of the CARCASS weight, not just the fat weight, is b/c this is a rendering process;
# it uses scraps of meat and fat (like bones etc)
recipeStorage["tallow"] = Recipe("chandler",(1,"lb"),
                                 [],
                                 [("cow",2/cowCarcassWeight)])

# 1% yield, number given from some research
poundsTimberPerPoundAshes = 100
ashesRequiredTimberCuFt = poundsTimberPerPoundAshes / densityTimber
recipeStorage["ashes"] = Recipe("chandler",(1,"lb"),
                                [("timber",ashesRequiredTimberCuFt)],
                                [])

# made by leaching ashes in water
recipeStorage["lye"] = Recipe("chandler",(1,"lb"),
                              [],
                              [("ashes",1)])

# http://www.millennium-ark.net/News_Files/Soap/Lye_Fat_Table.html
# (above page copyright AL Durtschi)
# using the 5% excess fat column given here, the required lye content for soap is 0.133 times the tallow content
lyeTallowRatio = Decimal(0.133)
# finally, we add a little bit of salt to get hard soap instead of soft
# http://www.motherearthnews.com/homesteading-and-livestock/how-to-make-soap-from-ashes-zmaz72jfzfre.aspx
# this article suggests 2.5 pints (3.22 lb) salt for 5 gallons (36.16 lb) of tallow
saltTallowRatio = Decimal(3.22) / Decimal(36.16)
# let's put it all together:
# to find the amount of tallow needed for 1 pound of soap, we solve for x in this equation: 1 = x + (x*lye-to-tallow ratio) + (x*salt-to-tallow ratio)
tallowForOneLbSoap = 1 / (1 + saltTallowRatio + lyeTallowRatio)
# finally, using the density of tallow as a proxy, we find the weight of a bar of soap
barSoapInCuFt = Decimal(3/12) * Decimal(2/12) * Decimal(6/12)
weightOneBarSoap = densityTallow * barSoapInCuFt
# my calculation for how many times it will wash one person:
# one bar of my soap is ~9 cubic inches and lasts about a month i.e. 30 washes
# the soap here is 36 cubic inches, thus it should last about 4 months or 120 washes
# but I'm going to cut that in half because adventurers get much dirtier than I ever would
recipeStorage["soap, hard"] = Recipe("chandler",(weightOneBarSoap,"lb"),
                                    [("salt",saltTallowRatio * weightOneBarSoap)],
                                    [("lye",lyeTallowRatio * weightOneBarSoap),("tallow",tallowForOneLbSoap * weightOneBarSoap)],
                                    description="will wash 1 person 60 times; 3x2x6 in.")


# a raw cowhide is about 50 square feet
# this includes the irregularly-shaped edge portions,
# so a nice big single square piece would only be about 40 square feet at most.
recipeStorage["raw cowhide"] = Recipe("butcher",(60,"lb"),
                                      [],
                                      [("cow",1)],
                                      unit=(50,"sq ft"))
semiGoods.append("raw cowhide")

recipeStorage["defleshed cowhide"] = Recipe("tanner",(15,"lb"),
                                            [],
                                            [("raw cowhide",1)],
                                            unit=recipeStorage["raw cowhide"].unit,
                                            description="cowhide cleaned of flesh and/or hair")
semiGoods.append("defleshed cowhide")

# density of quicklime is 209.1337 lb/cu ft.
# this site:
# http://boar.org.uk/aaiwxw3MusprattL6Preparation.htm
# says that three to four cubic feet measure of "freshly burned fat lime" (aka quicklime)
# is used for 100 average hides
# taking 3.5 cubic feet as our measure, that means 731.96795 lbs of quicklime per 100 hides
# AKA 0.732 lbs of quicklime per hide.
weightCowhideInOz = Decimal(15 * 16)
cowhideDensityInOzPerSqFt = Decimal(weightCowhideInOz / 50)
recipeStorage["tanned cowhide"] = Recipe("tanner",(15,"lb"),
                                         [],
                                         [("quicklime",0.732),("defleshed cowhide",1)],
                                         unit=recipeStorage["raw cowhide"].unit,
                                         description= str(cowhideDensityInOzPerSqFt) + " oz/sq. ft, thus a 1-ft square is " + str(Decimal(cowhideDensityInOzPerSqFt/64)) + " in. thick")

recipeStorage["holy symbol, plain, wooden"] = Recipe("carpenter",(1,"lb"),
                                                      [("timber",0.02)],
                                                      [])

recipeStorage["holy symbol, plain, iron"] = Recipe("blacksmith",(1,"lb"),
                                                      [],
                                                      [("wrought iron",1)])

# first step in making red and yellow (ochre) dyes
# high difficulty because multiple steps in the process
# here: https://en.wikipedia.org/wiki/Ochre#Modern_history
# wiki says that clay would be about 90% junk and 10% ochre
recipeStorage["separated ochre clay"] = Recipe("potter",(1,"lb"),
                                       [("clay",10)],
                                       [])
semiGoods.append("separated ochre clay")

# primary componenent of paints and of dyes
recipeStorage["pigment, red/yellow"] = Recipe("dyer",(1,"lb"),
                                              [],
                                              [("separated ochre clay",1)])
semiGoods.append("pigment, red/yellow")

# component of blue dye
recipeStorage["lapis lazuli, ground"] = Recipe("potter",(1,"lb"),
                                              [("lapis lazuli",1)])
semiGoods.append("lapis lazuli, ground")

recipeStorage["lapis lazuli, cleaned"] = Recipe("potter",(1,"lb"),
                                               [],
                                               [("lapis lazuli, ground",1),("ashes",0.2),("tallow",0.2)])
semiGoods.append("lapis lazuli, cleaned")

recipeStorage["pigment, ultramarine"] = Recipe("dyer",(1,"lb"),
                                               [],
                                               [("lapis lazuli, cleaned",1)])
semiGoods.append("pigment, ultramarine")

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
# let's say the fat hoops are 1.25 inch wide, and the thin ones are 1 inch. hoop breadth is 16 gauge (1/16 of an inch)

gauge16WireThicknessInches = Decimal(0.05082)

barrelHeadThickness = Decimal(2/3) / Decimal(12)
barrelHeadRadius = Decimal(6.5)/Decimal(12)
barrelHeadCircleArea = Decimal(pi) * (barrelHeadRadius ** 2)
barrelCuFt = barrelHeadCircleArea * barrelHeadThickness
barrelHeadWeight = densityTimber * barrelCuFt

recipeStorage["barrel head"] = Recipe("cooper",(barrelHeadWeight,"lb"),
                                      [("timber",barrelCuFt)],
                                      [])
semiGoods.append("barrel head")

barrelHeadCircumference = barrelHeadRadius * 2 * Decimal(pi)
fatHoopCuFt = barrelHeadCircumference * (Decimal(1.25) / Decimal(12)) * (gauge16WireThicknessInches / Decimal(12))
fatHoopWeight = densityWroughtIron * fatHoopCuFt

recipeStorage["barrel hoop, fat"] = Recipe("blacksmith",(fatHoopWeight, "lb"),
                                                         [],
                                                         [("wrought iron",fatHoopWeight)])
semiGoods.append("barrel hoop, fat")

# the thin hoops are actually 1/3 and 2/3 the distance to the middle of the barrel,
# so with the sloping of the barrel, we'd use a measure less than the barrel's max circumference
# to get their length.
# but I'll just go ahead and approximate them as being the same as its radius. no need for total accuracy.
barrelBodyCircumference = Decimal(2) / Decimal(3)
thinHoopCuFt = barrelBodyCircumference * (Decimal(1) / Decimal(12)) * (gauge16WireThicknessInches / Decimal(12))
thinHoopWeight = densityWroughtIron * thinHoopCuFt

recipeStorage["barrel hoop, thin"] = Recipe("blacksmith",(thinHoopWeight, "lb"),
                                                         [],
                                                         [("wrought iron",thinHoopWeight)])
semiGoods.append("barrel hoop, thin")

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
semiGoods.append("barrel stave")

# bringing it all together
barrelWeight = (numStaves * staveWeight) + (4 * thinHoopWeight) + (2 * fatHoopWeight) + (2 * barrelHeadWeight)
recipeStorage["barrel"] = Recipe("cooper",(barrelWeight, "lb"),
                                 [],
                                 [("barrel stave",numStaves),("barrel hoop, fat",2),("barrel hoop, thin",4),
                                  ("barrel head",2)],
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

weightWaterOneGal = Decimal(8.345404)

gallonsPerPint = Decimal(1)/Decimal(8)
waterWeightOnePint = weightWaterOneGal * gallonsPerPint

aleCerealAmt = Decimal(45)
aleMaltAmt = Decimal(120)
aleRoastedMaltAmt = Decimal(19.95)
aleBatchGallons = 30
# I calculated an original gravity of 1.190
# also, for both beer and ale I just use the weight of water to determine the weight. good enough for me.
recipeStorage["ale"] = Recipe("brewer",((aleBatchGallons * weightWaterOneGal),"lb"),
                              [("cereal",aleCerealAmt)],
                              [("malted grain",aleMaltAmt),("roasted malt",aleRoastedMaltAmt)],
                              unit=(30,"gallon"),
                              description="buyer supplies barrel; " + str(calculateABV(aleCerealAmt,(aleMaltAmt + aleRoastedMaltAmt),aleBatchGallons)) + " percent alcohol")

aleCerealOnePint = aleCerealAmt/240 # 8 pints per gallon; 30 gallons in the above batch
aleMaltOnePint = aleMaltAmt/240
aleRoastedMaltOnePint = aleRoastedMaltAmt/240
recipeStorage["ale, one pint"] = Recipe("brewer",(waterWeightOnePint,"lb"),
                                         [("cereal",aleCerealOnePint)],
                                         [("malted grain",aleMaltOnePint),("roasted malt",aleRoastedMaltOnePint)],
                                         unit=(1,"pint"),
                                         description = str(calculateABV(aleCerealAmt,(aleMaltAmt + aleRoastedMaltAmt),aleBatchGallons)) + " percent alcohol")

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
beerHops = Decimal(0.55)
beerGallons = 30
recipeStorage["beer"] = Recipe("brewer",((beerGallons*weightWaterOneGal),"lb"),
                               [("cereal",beerCereal),("hops",beerHops)],
                               [("malted grain",beerMalt)],
                               unit=(30,"gallon"),
                               description="buyer supplies barrel; " + str(calculateABV(beerCereal, beerMalt, beerGallons)) + " percent alcohol")

beerCerealOnePint = beerCereal/240 # 8 pints per gallon; 30 gallons in the above batch
beerMaltOnePint = beerMalt/240
beerHopsOnePint = beerHops/240
recipeStorage["beer, one pint"] = Recipe("brewer",(waterWeightOnePint,"lb"),
                                         [("cereal",beerCerealOnePint),("hops",beerHopsOnePint)],
                                         [("malted grain",beerMaltOnePint)],
                                         unit=(1,"pint"),
                                         description= str(calculateABV(beerCereal, beerMalt, beerGallons)) + " percent alcohol")


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
# ranges in color from brownish to whitish
recipeStorage["clean wool"] = Recipe("miller",(1,"lb"),
                                     [],
                                     [("scoured wool",Decimal(1/getWeight("scoured wool")))],
                                     description="either brownish or whitish in color")
semiGoods.append("clean wool")

# similar to the processes for cleaning wool, but for cotton instead
# no need to scour it, just to clean it (carding, picking, combing, etc)
# the ratio of raw cotton to clean cotton is b/c approx 60% of the weight of raw cotton is in the boll,
# which is discarded
recipeStorage["clean cotton"] = Recipe("miller",(1,"lb"),
                                         [("cotton",2.5)],
                                         [])
semiGoods.append("clean cotton")

recipeStorage["thin yarn, cotton"] = Recipe("spinner",(1,"lb"),
                                            [],
                                            [("clean cotton",1)],
                                            unit=(2000,"feet"),
			                    description="must be spun to be useful")
semiGoods.append("thin yarn, cotton")

recipeStorage["yarn, cotton"] = Recipe("spinner",(1,"lb"),
                                       [],
                                       [("thin yarn, cotton",1)],
                                       unit=(getUnitSize("thin yarn, cotton"),"feet"),
                                       description="useable as string and in stitching, ropemaking, etc.")

# warning: mostly-bullshit calculations ahead. I just need a figure here.
# if yarn is 1/16 inch thick, then it requires 16*12=192 feet of yarn to cover a square foot;
# if we need two such layers (i.e. two threads) to produce an actual square of cloth, then yarn per square foot is 384 feet
yarnFtPerCottonClothSqFt = 384
# the weight calculation below works because the table lists 1 lb of yarn.
cottonClothWeight = (yarnFtPerCottonClothSqFt/getUnitSize("yarn, cotton")) * getWeight("yarn, cotton")
recipeStorage["cotton cloth"] = Recipe("weaver",(cottonClothWeight,"lb"),
                                     [],
                                     [("yarn, cotton",yarnFtPerCottonClothSqFt / getUnitSize("yarn, cotton"))],
                                     unit=(1,"sq ft"))

# brown (or "raw") sugar, which still contains some molasses
# cane can yield 50% of its mass in juice; approximately 20% of that juice is sugar
# that means 10 lbs of sugarcane to get 1 lb of sugar
# as for molasses, recipe is essentially the same as brown sugar, except for the ratio of inputs to product
# the sugar in the sugarcane juice is 20% of the juice mass;
# we can say the remaining 80% of the juice mass turns into molasses; thus 50% * 80% = 4 lbs of molasses per 10 lbs of sugarcane
# its' more than 4 lbs of molasses per gallon -- its about 12. so we divide the molasses per-gallon weight by 4 lbs, to get the number we multiply that 10 lbs of sugarcane by, to get the sugarcane for 1 gallon of molasses
sugarcaneForOneGallonMolasses = 10 * molassesGallonWeight / 4

recipeStorage["brown sugar"] = Recipe("miller",(1,"lb"),
                                      [("sugarcane",10)],
                                      [],
                                      difficulty=2)

recipeStorage["molasses"] = Recipe("miller",(molassesGallonWeight,"lb"),
                                   [("sugarcane",sugarcaneForOneGallonMolasses)],
                                   [],
                                   unit=(1,"gallon"))

# 6 month old pig for the slaughter, weighing 150 lbs
recipeStorage["pig"] = Recipe("farmer",(150, "lb"),
                              [],
                              [("cattle feed",630)],
                              description="6 months old, ready for slaughter")

# going off the web, carcass weight is 75% of live weight, and dress weight is 75% of carcass weight
pigDressPercentageOfLiveWeight = Decimal(0.75) * Decimal(0.75)
# times weight of slaughtered pig
pigDressWeight = pigDressPercentageOfLiveWeight * 150
recipeStorage["pork"] = Recipe("butcher",(1,"lb"),
                               [],
                               [("pig",Decimal(1/pigDressWeight))])

# working from a recipe in the London Art of Cookery, an early 1700s cookbook,
# as shown here: https://savoringthepast.net/2013/01/08/salted-meat-for-a-journey-at-sea/
# for each pound of meat, it works out to 4 oz salt + a mix of 1/3 lb salt and 1/6 lb brown sugar,
# (for a total of 9.33 oz salt and 2.67 oz brown sugar)
# numbers immediately below are calculated in lbs
saltForSaltBeef = Decimal(1/4) + Decimal(1/3)
brownSugarForSaltBeef = Decimal(1/6)
recipeStorage["beef jerky"] = Recipe("butcher",(1,"lb"),
                                    [("salt",saltForSaltBeef)],
                                    [("brown sugar",brownSugarForSaltBeef)],
                                    description="will keep for a year if seldom exposed to air")

# salt pork, when compared to salt beef, is said to require more salt and less sugar (same source as salt beef)
saltForSaltPork = Decimal(1/4) + Decimal(2.5/6)
brownSugarForSaltPork = Decimal(0.5/6)
recipeStorage["salt pork"] = Recipe("butcher",(1,"lb"),
                                    [("salt",saltForSaltPork)],
                                    [("brown sugar",brownSugarForSaltPork)],
                                    description="will keep for a year if seldom exposed to air")

recipeStorage["spearhead"] = Recipe("blacksmith",(1.5,"lb"),
                                    [],
                                    [("wrought iron",Decimal(0.9)),("steel",Decimal(0.6))])
semiGoods.append("spearhead")

spearHaftCuFt = cylinderCuFt(5,Decimal(0.5/12))
recipeStorage["spear haft"] = Recipe("carpenter",(spearHaftCuFt * densityTimber,"lb"),
                                                  [("timber",spearHaftCuFt)],
                                                  [])
semiGoods.append("spear haft")

spearWeight = recipeStorage["spearhead"].weight[0] + recipeStorage["spear haft"].weight[0]
recipeStorage["spear"] = Recipe("blacksmith",(spearWeight, "lb"),
                                [],
                                [("spearhead",1),("spear haft",1)],
                                description="1d6+1 damage, one-handed, melee; haft 5 ft. long")

maceShaftCrossSectionRadius = Decimal(0.5) # inches
maceHaftCuFt = cylinderCuFt(2,maceShaftCrossSectionRadius/12)
maceHaftMetalProportion = Decimal(0.25)
maceHaftWoodProportion = 1 - maceHaftMetalProportion
maceHaftMetalWeight = maceHaftMetalProportion * maceHaftCuFt * densityWroughtIron
maceHaftWoodWeight = maceHaftWoodProportion * maceHaftCuFt * densityTimber
recipeStorage["mace haft"] = Recipe("blacksmith",(maceHaftMetalWeight + maceHaftWoodWeight,"lb"),
                                    [("timber",maceHaftCuFt)],
                                    [("wrought iron",maceHaftMetalWeight)],
                                    description="wood reinforced with metal bands")
semiGoods.append("mace haft")

maceFlangeCuFt = triangularPrismCuFt(Decimal(1/12),Decimal(0.25),Decimal(0.1))
maceFlangeWeight = maceFlangeCuFt * densityWroughtIron
recipeStorage["mace flange"] = Recipe("blacksmith",(maceFlangeWeight,"lb"),
                                      [],
                                      [("wrought iron",maceFlangeWeight)])
semiGoods.append("mace flange")

recipeStorage["mace"] = Recipe("blacksmith",(getWeight("mace haft") + (6 * maceFlangeWeight),"lb"),
                               [],
                               [("mace haft",1),("mace flange",6)],
                               description="1d8 damage, one-handed, melee; haft is 2 ft.")

wroughtIronIngotCuFt = Decimal(1/12) * Decimal(1/12) * Decimal(3.57/12)
oneFootWireCuFt = cylinderCuFt(1,(gauge16WireThicknessInches/2/12))
feetOfWire = wroughtIronIngotCuFt / oneFootWireCuFt
# can be used for fastening, or turned into rings for mail
# weight of wire is the same as the 1 lb ingot of wrought iron; it's just turned into a different shape
recipeStorage["wire"] = Recipe("blacksmith",(1,"lb"),
                               [],
                               [("wrought iron",1)],
                               difficulty=3, # lots of hammering and then lots and lots of pulling
                               unit=(feetOfWire,"feet"),
                               description="thickness 16 gauge, i.e. 0.05082 in. diameter")

# this is in feet b/c division by 12
mailRingRadius = Decimal(0.2) / 12
# this circumference is also the feet of wire needed for one ring
mailRingCircumference = 2 * Decimal(pi) * mailRingRadius
unitsWirePerRing = mailRingCircumference / getUnitSize("wire")
recipeStorage["mail ring"] = Recipe("blacksmith",(unitsWirePerRing,"lb"),
                                    [],
                                    [("wire",unitsWirePerRing)])
semiGoods.append("mail ring")

# rings overlap, which would mean more per linear foot, but we'll ignore that since mail can stretch a little too
ringsToReachOneFootLength = 1 / mailRingRadius
ringsInSquareFootMail = ringsToReachOneFootLength ** 2
recipeStorage["mail sqft"] = Recipe("blacksmith",(getWeight("mail ring") * ringsInSquareFootMail,"lb"),
                                    [],
                                    [("mail ring",ringsInSquareFootMail)],
                                    unit=(1,"sq ft"))
semiGoods.append("mail sqft")

# this is an OK-ish estimate, way better than my first one
hauberkSqFt = 12
recipeStorage["mail hauberk"] = Recipe("blacksmith",(hauberkSqFt * getWeight("mail sqft"),"lb"),
                                       [],
                                       [("mail sqft",hauberkSqFt)],
                                       description="AC 6; has full sleeves, and covers torso to the knees")

# The amount of feet of yarn per pound of wool which I give here is probably a vast under- or overshoot,
# but it's a highly variable amount dependent on thickness of resultant yarn, type of sheep, and
# other factors, so I'll just go ahead and soldier on. Can always fix it later.
recipeStorage["thin yarn, wool"] = Recipe("spinner",(1,"lb"),
                                    [],
                                    [("clean wool",1)],
                                    unit=(2000,"feet"),
			       description="must be spun into thread or yarn to be useful")
semiGoods.append("thin yarn, wool")

recipeStorage["yarn, wool"] = Recipe("spinner",(1,"lb"),
                               [],
                               [("thin yarn, wool",1)],
                               unit=(getUnitSize("thin yarn, wool"),"feet"),
                               description="useable as string and in stitching, ropemaking, etc.")


recipeStorage["thread"] = Recipe("spinner",(1,"lb"),
                                 [],
                                 [("thin yarn, wool",1)],
                                 unit=(getUnitSize("thin yarn, wool")*2,"feet"),
                                 description="useable for stitching cloth and textiles")


# the leather pattern must be worked by a leatherworker with thread, to make the bag;
# he sews the sides together, and adds the string for tying closed, with its holes
# 4 feet of thread are used to sew the sides of the bag together;
# another 4 feet are used to make the top flap extra secure

# I approximate the pattern of a leather backpack holding one cubic foot
# as the pattern which, when folded up, makes a cube, each side 1 foot long.
# Thus the pattern requires 6 square feet of leather (roughly the shape of a Christian cross).
# Then we add another 1 square foot of leather for the straps.
# A tanned cowhide is 50 square feet, so we divide our total of 7 by 50.
backpackPortionOfCowhide = Decimal(7 / 50)
recipeStorage["backpack"] = Recipe("leatherworker",(backpackPortionOfCowhide * getWeight("tanned cowhide"),"lb"),
                                                [],
                                                [("thread",Decimal(8) / getUnitSize("thread")),
                                                 ("tanned cowhide",backpackPortionOfCowhide),
                                                 ("yarn, wool",Decimal(1) / getUnitSize("yarn, wool"))],
                                                description="with string clasp; holds 30 lbs")

# belt pouch holds very little;
# the total square footage is (0.15^2)*6
beltpouchSqFt = (Decimal(0.15) ** 2) * 6
beltpouchPortionOfCowhide = Decimal(beltpouchSqFt / 50)
recipeStorage["belt pouch"] = Recipe("leatherworker",(beltpouchPortionOfCowhide * getWeight("tanned cowhide"),"lb"),
                                                [],
                                                [("thread",Decimal(1.2) / getUnitSize("thread")),
                                                 ("tanned cowhide",beltpouchPortionOfCowhide),
                                                 ("yarn, wool",Decimal(1) / getUnitSize("yarn, wool"))],
                                                description="with string clasp; holds 4.5 lbs")

# let's say a belt is 3 feet long and 1 inch wide, and you cut to the appropriate length -- but the leatherworker is gonna charge you for the whole thing.
# you also need maybe 12 feet of thread to make the edges tough
beltSqFt = Decimal(1/12) * 3
beltPortionOfCowhide = Decimal(beltSqFt/50)
recipeStorage["belt"] = Recipe("leatherworker",(beltPortionOfCowhide * getWeight("tanned cowhide"),"lb"),
                                                [],
                                                [("thread",Decimal(12) / getUnitSize("thread")),
                                                 ("tanned cowhide",beltPortionOfCowhide)],
                                                description="leather; can attach up to 3 belt-attachable items: weapon scabbard/loop/hook, belt pouch, quiver")

# approximate
whistleCuFt = cylinderCuFt(Decimal(0.25), Decimal(0.75/12)) 
whistleWeight = whistleCuFt * densityTimber
recipeStorage["whistle"] = Recipe("carpenter",(whistleWeight,"lb"),
                                  [("timber",whistleCuFt)],
                                  [],
                                  description="non-musical; 3 inches long")

# structurally it's a little hollow block
fippleCuFt = Decimal(1/12) * Decimal(1/12) * Decimal(2/12)
fippleWeight = fippleCuFt * densityTimber
recipeStorage["fipple"] = Recipe("carpenter",(fippleWeight,"lb"),
                                 [("timber", fippleCuFt)],
                                 [],
                                 description="whistle mouthpiece for certain instruments")
semiGoods.append("fipple")

recorderBodyCuFt = cylinderCuFt(Decimal(1.25), Decimal(0.75/12))
recorderBodyWeight = recorderBodyCuFt * densityTimber
recipeStorage["recorder"] = Recipe("carpenter",(recorderBodyWeight + fippleWeight,"lb"),
                                   [("timber",recorderBodyCuFt)],
                                   [("fipple",1)],
                                   description="type of wooden flute; 15 inches long")

# four strands of yarn are twisted into a strand, turning one direction;
# four strands are twisted into a rope, turning the other direction.
# thus 16 feet of yarn makes 1 foot of rope, and a foot of rope weighs 16 times as much as a foot of yarn
recipeStorage["rope strand"] = Recipe("ropewalker",(4,"lb"),
                                      [],
                                      [("yarn, wool",1)],
                                      unit=(getUnitSize("yarn, wool")/4,"feet"))
semiGoods.append("rope strand")

recipeStorage["rope"] = Recipe("ropewalker",(16,"lb"),
                               [],
                               [("rope strand",1)],
                               unit=(getUnitSize("rope strand")/4,"feet"),
                               description="1 inch thick; can suspend up to 1000 lbs")

# warning: mostly-bullshit calculations ahead. I just need a figure here.
# if yarn is 1/16 inch thick, then it requires 16*12=192 feet of yarn to cover a square foot;
# if we need two such layers to produce an actual square of cloth, then yarn per square foot is 384 feet
yarnFtPerWoolClothSqFt = 384
# the weight calculation below works because the table lists 1 lb of yarn.
woolClothWeight = (yarnFtPerWoolClothSqFt/getUnitSize("yarn, wool")) * getWeight("yarn, wool")
recipeStorage["wool cloth"] = Recipe("weaver",(woolClothWeight,"lb"),
                                     [],
                                     [("yarn, wool",yarnFtPerWoolClothSqFt / getUnitSize("yarn, wool"))],
                                     unit=(1,"sq ft"))

# felt is produced by squishing layers of clean wool together and addig lye
# 1/8 of an inch thick
feltThicknessRelativeToWoolCloth = 3
poundsCleanWoolPerSqFtFelt = feltThicknessRelativeToWoolCloth * woolClothWeight
recipeStorage["felt"] = Recipe("fuller",(poundsCleanWoolPerSqFtFelt,"lb"),
                               [],
                               [("lye",Decimal(0.1)),("clean wool",poundsCleanWoolPerSqFtFelt)],
                               unit=(1,"sq ft"),
                               description="soft and thick wool textile")

# gambesons are quite thick, with lots of layers
gambesonLayers = 10
# we'll need  feet of thread per layer, to go around the edges of each layer twice, attaching them to the others into a big stack
gambesonThread = gambesonLayers * 8
gambesonSqFt = 12 * gambesonLayers
recipeStorage["quilted gambeson"] = Recipe("weaver",(gambesonSqFt * getWeight("wool cloth"),"lb"),
                                           [],
                                           [("wool cloth",gambesonSqFt),("thread",(gambesonThread/getUnitSize("thread")))],
                                           description="AC 9; padded cloth armor")

recipeStorage["gemshorn"] = Recipe("carpenter",(2,"lb"),
                                   [],
                                   [("fipple",1),("cow",Decimal(0.5))],
                                   description="ocarina-type instrument made from bull horn; 8 inches long")

# this is really a small instrument, and it's not a solid block either, so these dimensions are about right
# I approximate its dimensions as a thin cylinder, which would then be bent into shape,
# i.e. curve into a sort of horseshoe shape,
# and the reed and tongue would be added (small pieces of scrap metal)
# the height (length) of the cylinder is given,
# but because it's bent more or less in half, the instrument length is half that.
jawHarpCuFt = cylinderCuFt(Decimal(1),Decimal(0.1/12))
jawHarpWeight = jawHarpCuFt * densityWroughtIron
recipeStorage["jaw harp"] = Recipe("blacksmith",(jawHarpWeight,"lb"),
                                   [],
                                   [("wrought iron",jawHarpWeight)],
                                   description="6 inches long")

handbellHandleSqFt = Decimal(0.5/12) * Decimal(4/12)
handbellHandleLeatherProportion = handbellHandleSqFt / 50 # sqft of 1 cowhide
handbellHandleWeight = handbellHandleLeatherProportion * getWeight("tanned cowhide")
recipeStorage["handbell handle"] = Recipe("leatherworker",(handbellHandleWeight,"lb"),
                                          [],
                                          [("tanned cowhide",handbellHandleLeatherProportion)])
semiGoods.append("handbell handle")

# modeled as a tiny block of bell metal which is pounded into shape
handbellClapperCuFt = Decimal(0.25/12) * Decimal(0.25/12) * Decimal(0.5/12)
handbellClapperWeight = handbellClapperCuFt * densityBellmetal
recipeStorage["handbell clapper"] = Recipe("blacksmith",(handbellClapperWeight,"lb"),
                                           [],
                                           [("bell metal",handbellClapperWeight)])
semiGoods.append("handbell clapper")

# a thin sheet of bell metal, pounded into shape
handbellBodyCuFt = Decimal(0.1/12) * Decimal(1.5/12) * Decimal(3.5/12)
handbellBodyWeight = handbellBodyCuFt * densityBellmetal
recipeStorage["handbell body"] = Recipe("blacksmith",(handbellBodyWeight,"lb"),
                                        [],
                                        [("bell metal",handbellBodyWeight)])
semiGoods.append("handbell body")

recipeStorage["handbell"] = Recipe("blacksmith",(handbellHandleWeight + handbellClapperWeight + handbellBodyWeight,"lb"),
                                   [],
                                   [("handbell handle",1),("handbell body",1),("handbell clapper",1)],
                                   description="has leather loop handle; body of bell ~1.5 inches tall") 

# shoulder belt
# 4 feet
# can hold 1 pouch in front and one scabbard (or weapon cord)

# some of the weight of the original clay is water;
# we account for that loss here so that we don't have to do it as part of calculation for weights of
# pottery pieces
recipeStorage["kneaded clay"] = Recipe("potter",(1,"lb"),
                                       [("clay",Decimal(1.15))],
                                       [])
semiGoods.append("kneaded clay")


# next we can do a salt-glazed pottery holder thingy, i.e. a stoneware I guess
# some kind of large stoneware mug.
# some considerations for volume...
cubicFootInPints = Decimal(59.8442)
# (hey that means there's just abou 4 cubic feet of beer in the 30-gallon barrel! woohoo, coincidence!)
# we want this to be a two-pint mug, like the Bavarian mug called Mass
# (a tankard could also be wooden or glass)
# if modeled as a cylinder, with radius 1.25 inches and height 9 inches,
# then the innner volume (amount of liquid contained) would be (44.18/1728)*cubicFootInPints, or 1.53 pints,
# and the tapster just doesn't have to fill it all the way!
# and the whole thing would be like a cylinder with one hollow side, larger than X by maybe 1/4 inch
# this could easily be made of pewter -- no lets leave that a tankard.

clubCuFt = cylinderCuFt(2,1.5/12)
clubWeight = clubCuFt * densityTimber
recipeStorage["club"] = Recipe("carpenter",(clubWeight, "lb"),
                               [("timber",clubCuFt)],
                               [],
                               description="1d4+1 damage; melee one-handed; 2 feet long")

handaxeHaftCuFt = cylinderCuFt(2,0.25/12)
handaxeHaftWeight = handaxeHaftCuFt * densityTimber
recipeStorage["handaxe haft"] = Recipe("carpenter",(handaxeHaftWeight,"lb"),
                                       [("timber",handaxeHaftCuFt)],
                                       [])

semiGoods.append("handaxe haft")

# head made from  metal having approximately these dimensions
handaxeHeadCuFt = Decimal(3/12) * Decimal(4/12) * Decimal(0.5/12)
handaxeHeadWeight = handaxeHeadCuFt * densitySteel
# I'm envisioning this as basically a tomahawk, not a francisca;
# thus it is both a melee and thrown weapon
recipeStorage["handaxe"] = Recipe("blacksmith",(handaxeHeadWeight + handaxeHaftWeight,"lb"),
                                  [],
                                  [("steel",handaxeHeadWeight),("handaxe haft",1)],
                                  description="1d4+1 damage; melee/thrown; range 4/6/8")

quarterstaffCuFt = cylinderCuFt(5,Decimal(0.5/12))
recipeStorage["quarterstaff"] = Recipe("carpenter",(quarterstaffCuFt * densityTimber,"lb"),
                                       [("timber",quarterstaffCuFt)],
                                       [],
                                       description="1d6 damage; melee two-handed; 5 ft long")

ratioJavelinWeightSpearWeight = Decimal(3.5/5)
javelinWeight = recipeStorage["spearhead"].weight[0] + (recipeStorage["spear haft"].weight[0] * ratioJavelinWeightSpearWeight)
recipeStorage["javelin"] = Recipe("blacksmith",(javelinWeight ,"lb"),
                                  [],
                                  [("spear haft", ratioJavelinWeightSpearWeight),("spearhead",1)],
                                  description="1d6 damage; thrown 6/10/14; " + str(ratioJavelinWeightSpearWeight * 5) + " ft long")

# string used for a sling is braided; thus 3 feet of yarn string make 1 foot of braid
# in total a sling will require 4 feet of braid, thus 12 feet of yarn
# the pocket for ammunition is a 2x2 in. leather square; the tab is 1x1 in.
# the question I can't quite answer yet is WHO makes the sling?
# for now I will just call it the leatherworker, although I think that's a pretty crappy choice.
slingYarnFt = 12
slingYarnUnitRatio = slingYarnFt / getUnitSize("yarn, wool")
slingYarnWeight = slingYarnUnitRatio * getWeight("yarn, wool")
slingLeatherSqFt = (Decimal(2/12) ** 2) + (Decimal(1/12) ** 2)
slingLeatherUnitRatio = slingLeatherSqFt / getUnitSize("tanned cowhide")
slingLeatherWeight = slingLeatherUnitRatio * getWeight("tanned cowhide")
recipeStorage["sling"] = Recipe("leatherworker",(slingLeatherWeight + slingYarnWeight,"lb"),
                                [],
                                [("yarn, wool",slingYarnWeight),("tanned cowhide",slingLeatherWeight)],
                                description="1d4 damage (1d4-1, min. 0, with scavenged ammo); missile; range 12/24/36")

musicalBoneCuFt = Decimal(1/12) * Decimal(1/8/12) * Decimal(6/12)
# musical bones come in pairs
musicalBonePairCuFt = musicalBoneCuFt * 2
recipeStorage["musical bones"] = Recipe("carpenter",(musicalBonePairCuFt * densityTimber,"lb"),
                                        [("timber",musicalBonePairCuFt)],
                                        [],
                                        description="pair of 6-inch curved wooden slats; wrist is rotated to create percussive music")

# lamella: a rectangle-like piece of material laced together to form armor
# these will form the basis of leather armor
leatherLamellaWidth = Decimal(4/12)
leatherLamellaHeight = Decimal(2/12)
leatherLamellaSqFt = leatherLamellaHeight * leatherLamellaWidth
leatherLamellaUnitRatio = leatherLamellaSqFt / getUnitSize("tanned cowhide")
leatherLamellaWeight = leatherLamellaUnitRatio * getWeight("tanned cowhide")
recipeStorage["leather lamella"] = Recipe("leatherworker", (leatherLamellaWeight, "lb"),
                                          [],
                                          [("tanned cowhide", leatherLamellaWeight)],
                                          description="2x4 inches; punched with holes for lacing")
semiGoods.append("leather lamella")

# I'm assuming this is enough square feet for good coverage, including arms, down to the waist
# that's probably a bit small, especially since I made the gambeson 12 square feet of material
# oh well
leatherArmorSqFt = 8
numberOfLeatherLamella = leatherArmorSqFt / leatherLamellaSqFt
yarnFtPerLamella = 2
leatherArmorTotalYarn = numberOfLeatherLamella * yarnFtPerLamella
leatherArmorYarnUnitRatio = leatherArmorTotalYarn / getUnitSize("yarn, wool")
leatherArmorYarnWeight = leatherArmorYarnUnitRatio * getWeight("yarn, wool")
leatherArmorWeight = (numberOfLeatherLamella * leatherLamellaWeight) + leatherArmorYarnWeight
recipeStorage["leather armor"] = Recipe("leatherworker", (leatherArmorWeight,"lb"),
                                        [],
                                        [("leather lamella",numberOfLeatherLamella),("yarn, wool", leatherArmorYarnWeight)],
                                        description="AC 8; lamellar construction; covers torso down to waist, plus arms")

shieldWoodThickness = Decimal(0.5)/Decimal(12)

# timber for a round wooden shield, a foot in radius
shieldRadius = Decimal(1) # feet
shieldArea = shieldRadius * Decimal(3.14)
shieldPerimeter = 2 * shieldArea # circumference

shieldTimberCuFt = shieldArea * shieldWoodThickness
shieldTimberWeight = shieldTimberCuFt * densityTimber

# a strip of leather reinforces the edge of the shield
# currently LEFT OUT OF THE RECIPE, to do for when leather prices are more stable
shieldEdgingSqFt = shieldPerimeter * shieldWoodThickness
shieldEdgingUnitRatio = shieldEdgingSqFt / getUnitSize("defleshed cowhide")
shieldEdgingWeight = shieldEdgingUnitRatio * getWeight("defleshed cowhide")

# two of these ropes act as straps to provide protection against dropping if Dex check failed when hit by crit, at cost of taking time to secure them beforehand
shieldRopeStrapLength = Decimal(0.5) # 6 inches long
shieldRopeStrapUnitRatio = shieldRopeStrapLength / getUnitSize("rope")
shieldRopeStrapWeight = shieldRopeStrapUnitRatio * getWeight("rope")

# handle, a u-shaped cylinder of wood attached to the shield's back
shieldHandleCuFt = cylinderCuFt(Decimal(6)/Decimal(12),Decimal(0.25)/Decimal(12))
shieldHandleWeight = shieldHandleCuFt * densityTimber

recipeStorage["shield, round wooden"] = Recipe("carpenter",(shieldTimberWeight + (shieldRopeStrapWeight * 2) + shieldHandleWeight,"lb"),
                                               [("timber",shieldTimberCuFt + shieldHandleCuFt)],
                                               [("rope",shieldRopeStrapWeight * 2)],
                                               description="improves AC by -1; wooden shield 2 feet across, with ropes for securing")


# 2 ounces, or 1/8 lb, of pigment for a gallon of paint
poundsOfPigmentPerGallonPaint = Decimal(1)/Decimal(8)
# but we are doing only 1 quart of paint, so we divide by 4
poundsPigmentQuartPaint = poundsOfPigmentPerGallonPaint / 4

recipeStorage["paint, red/yellow"] = Recipe("potter",(poundsPigmentQuartPaint + milkGallonWeight/Decimal(4),"lb"),
                                                      [],
                                                      [("cow milk",0.25),("pigment, red/yellow",poundsPigmentQuartPaint)],
                                                      unit=(1,"quart"),
                                                      description="in powder form; covers 75 sq ft. Keeps for 6 mo. sealed or 4 days once once opened")

recipeStorage["paint, ultramarine blue"] = Recipe("potter",(poundsPigmentQuartPaint + milkGallonWeight/Decimal(4),"lb"),
                                                      [],
                                                      [("cow milk",0.25),("pigment, ultramarine",poundsPigmentQuartPaint)],
                                                      unit=(1,"quart"),
                                                      description=recipeStorage["paint, red/yellow"].description)

# sling bullet is a 1-inch sphere, pressed into almondish shape
slingBulletCuFt = sphereCuFt(Decimal(1)/Decimal(12))
slingBulletWeight = slingBulletCuFt * densityClay
recipeStorage["sling bullet"] = Recipe("potter",(slingBulletWeight,"lb"),
                                       [("clay",slingBulletWeight)],
                                       [],
                                       unit=(1,"bullet"),
                                       description="made of clay, with almond-like shape")

recipeStorage["sling bullet, inscribed"] = Recipe("potter",(slingBulletWeight,"lb"),
                                                  [("clay",slingBulletWeight)],
                                                  [],
                                                  difficulty=4,
                                                  unit=(1,"bullet"),
                                                  description="inscribed with symbol, or word up to 5 chars")

# http://www.maritime.org/conf/conf-kaye-tar.htm
# this source gives 4,000 lbs of "light wood" per 40-60 gallons of pitch
# I'll go ahead and say it's 50 gallons per 4000 lbs
# thus, for 1 gallon, it's 4000/50 or 80 lbs of wood
lbsTimberPerGallonPitch = 80
timberCuFtPerGallonPitch = lbsTimberPerGallonPitch / densityTimber
recipeStorage["pitch, bulk"] = Recipe("potter",(weightWaterOneGal,"lb"),
                                [("timber",timberCuFtPerGallonPitch)],
                                [],
                                unit=(1,"gallon"),
                                description="viscous fluid derived from trees; customer supplies container")

semiGoods.append("pitch, bulk")
# temporarily renamed and made a semigood until I've sorted out a container for it,
# i.e. done programming to do accurate pricing for container + components scenarios

# the shirt, which was the sole underwear for most people, had full sleeves and fell to the knees
# let's call it 10 square feet
shirtWoolSqFt = 10
shirtNeededWoolYarnProportion = (shirtWoolSqFt * 2) / getUnitSize("yarn, wool")
shirtWoolYarnWeight = shirtNeededWoolYarnProportion * getWeight("yarn, wool")
shirtTotalWeight = shirtWoolYarnWeight + (shirtWoolSqFt * getWeight("wool cloth"))
recipeStorage["shirt"] = Recipe("tailor",(shirtTotalWeight, "lb"),
                                        [],
                                        [("wool cloth",shirtWoolSqFt),("yarn, wool",shirtWoolYarnWeight)],
                                        description="underwear for both sexes; full sleeves, reaches knees")

bicorneFeltSqFt = Decimal(1.75)
bicorneWeight = bicorneFeltSqFt * getWeight("felt")
recipeStorage["hat, bicorne"] = Recipe("hatter",(bicorneWeight,"lb"),
                                       [],
                                       [("felt",bicorneWeight)],
                                       description="hat with both sides of brim turned up")

tricorneFeltSqFt = Decimal(2.25)
tricorneWeight = tricorneFeltSqFt * getWeight("felt")
recipeStorage["hat, tricorne"] = Recipe("hatter",(tricorneWeight,"lb"),
                                       [],
                                       [("felt",tricorneWeight)],
                                       description="hat with brim folded up in a triangular shape")

# model the amount of clay needed for a cup,
# as the volume of a cylinder 4 inches high and 1 inch radius, minus the inner volume,
# a cylinder 3.5 inches high and 0.875 in radius
cupOuterVolume = cylinderCuFt(Decimal(4)/Decimal(12),Decimal(1)/Decimal(12))
cupInnerVolume = cylinderCuFt(Decimal(3.5)/Decimal(12),Decimal(0.875)/Decimal(12))
cupCuFt = cupOuterVolume - cupInnerVolume
cupClayWeight = cupCuFt * densityClay
recipeStorage["cup, earthenware"] = Recipe("potter",(cupClayWeight,"lb"),
                                           [("clay",cupClayWeight)],
                                           [],
                                           description="4 in. high, 2 in. diameter; no handle")

# using clay in place of sand
# 80/20 mix of sand to quicklime
cupGlassWeightClay = cupClayWeight * percentageClayInGlass
cupGlassWeightQl = cupClayWeight - cupGlassWeightClay
recipeStorage["cup, glass, clear"] = Recipe("glassblower",(cupClayWeight,"lb"),
                                     [("clay",cupGlassWeightClay)],
                                     [("quicklime",cupGlassWeightQl)],
                                     description="4 in. high, 2 in. diameter; no handle")

# shape is roughly a square pyramid
# 3-inch edge, 5-inches heigh
flaskInnerVolume = squarePyramidCuFt(Decimal(3)/Decimal(12),Decimal(5)/Decimal(12))
flaskOuterVolume = squarePyramidCuFt(Decimal(3.25)/Decimal(12),Decimal(5.25)/Decimal(12))
flaskCuFt = flaskOuterVolume - flaskInnerVolume
flaskClayWeight = flaskCuFt * densityClay
flaskStopperCuFt = cylinderCuFt(Decimal(0.75)/Decimal(12),Decimal(0.125)/Decimal(12))
recipeStorage["flask, earthenware"] = Recipe("potter",(flaskClayWeight,"lb"),
                                             [("timber",flaskStopperCuFt),("clay",flaskClayWeight)],
                                             [],
                                             description="holds 8 fl oz (1/2 a pint); with wooden stopper; 5 in. high")

flaskGlassWeightClay = flaskClayWeight * percentageClayInGlass
flaskGlassWeightQl = flaskClayWeight - flaskGlassWeightClay
recipeStorage["flask, glass"] = Recipe("glassblower",(flaskClayWeight,"lb"),
                                             [("timber",flaskStopperCuFt),("clay",flaskGlassWeightClay)],
                                             [("quicklime",flaskGlassWeightQl)],
                                             description="holds 8 fl oz (1/2 a pint); with wooden stopper; 5 in. high")

# inches
jarHeight = Decimal(28)
jarRadius = Decimal(2)
jarInnerVolume = cylinderCuFt(jarHeight/Decimal(12),jarRadius/Decimal(12))
jarOuterHeight = jarHeight + Decimal(0.25)
jarOuterRadius = jarRadius + Decimal(0.125)
jarOuterVolume = cylinderCuFt(jarOuterHeight/Decimal(12), jarOuterRadius/Decimal(12))
# sphere part of lid is a little knob for lifting it out
jarLidCuFt = cylinderCuFt(Decimal(0.5)/Decimal(12),jarOuterRadius/Decimal(12)) + sphereCuFt(Decimal(0.125)/Decimal(12))
jarCuFt = jarOuterVolume - jarInnerVolume + jarLidCuFt
jarClayWeight = jarCuFt * densityClay
recipeStorage["jar, earthenware"] = Recipe("potter",(jarClayWeight,"lb"),
                                           [("clay",jarClayWeight)],
                                           [],
                                           description="holds 12 and 1/8 pints; with lid")


# bulk pitch is sold by the gallon
# we want to divide its price (or its weight) by 128 to get the per-oz price,
# then multiply that by 8 since pitch comes in an 8-fl-oz container
pitchConversionFactor = (Decimal(1)/Decimal(128)) * 8
pitchInFlaskWeightPitch = getWeight("pitch, bulk") * pitchConversionFactor
recipeStorage["pitch"] = Recipe("potter",(pitchInFlaskWeightPitch + getWeight("flask, earthenware"),"lb"),
                                [],
                                [("pitch, bulk",pitchConversionFactor),("flask, earthenware",1)],
                                unit=(8,"fl oz"),
                                description="contained in earthenware flask")

# in feet, as always
# one sixteenth of an inch
buttonHeight = (Decimal(1)/Decimal(16))/Decimal(12)
buttonRadius = (Decimal(1)/Decimal(4))/Decimal(12)
buttonCuFt = cylinderCuFt(buttonHeight, buttonRadius)
# the above can apply to all buttons unless otherwise desired

buttonClayWeight = buttonCuFt * densityClay
recipeStorage["button, ceramic"] = Recipe("potter",(buttonClayWeight,"lb"),
                                          [("clay",buttonClayWeight*24)],
                                          [],
                                          unit=(24,"ct"),
                                          description="two dozen buttons")

buttonGlassWeightClay = buttonClayWeight * percentageClayInGlass
buttonGlassWeightQl = buttonClayWeight - buttonGlassWeightClay
recipeStorage["button, glass"] = Recipe("glassblower",(buttonClayWeight,"lb"),
                                          [("clay",buttonGlassWeightClay*24)],
                                          [("quicklime",buttonGlassWeightQl*24)],
                                          unit=(24,"ct"),
                                          description="two dozen buttons")

waistcoatSqFtWool = 4
waistcoatNumButtons = 12
waistcoatThread = 4 * waistcoatSqFtWool
waistcoatThreadUnitRatio = waistcoatThread / getUnitSize("yarn, wool")
waistcoatThreadWeight = waistcoatThreadUnitRatio * getWeight("yarn, wool")
waistcoatWeightWool = waistcoatSqFtWool * getWeight("wool cloth")
recipeStorage["waistcoat"] = Recipe("tailor",(waistcoatWeightWool + waistcoatThreadWeight,"lb"),
                                    [],
                                    [("wool cloth",waistcoatSqFtWool), ("yarn, wool",waistcoatThreadWeight),("button, ceramic",0.5)],
                                    description="men's torso garment, with 12 buttons; worn by all levels of society")

dressSqFtWool = 10
dressThread = 4 * dressSqFtWool
dressThreadUnitRatio = dressThread / getUnitSize("yarn, wool")
dressThreadWeight = dressThreadUnitRatio * getWeight("yarn, wool")
dressWeightWool = dressSqFtWool * getWeight("wool cloth")
recipeStorage["dress"] = Recipe("tailor",(dressWeightWool + dressThreadWeight,"lb"),
                                    [],
                                    [("wool cloth",dressSqFtWool), ("yarn, wool",dressThreadWeight)],
                                    description="women's garment")

skirtSqFtWool = 6
skirtThread = 4 * skirtSqFtWool
skirtThreadUnitRatio = skirtThread / getUnitSize("yarn, wool")
skirtThreadWeight = skirtThreadUnitRatio * getWeight("yarn, wool")
skirtWeightWool = skirtSqFtWool * getWeight("wool cloth")
recipeStorage["skirt"] = Recipe("tailor",(skirtWeightWool + skirtThreadWeight,"lb"),
                                    [],
                                    [("wool cloth",skirtSqFtWool), ("yarn, wool",skirtThreadWeight)],
                                    description="women's garment, worn with bodice or under dress")

bodiceSqFtWool = 4
bodiceThread = (4 * bodiceSqFtWool) + 4 # the normal 4 * wool sqft, plus an additional 4 feet for the lacing
bodiceThreadUnitRatio = bodiceThread / getUnitSize("yarn, wool")
bodiceThreadWeight = bodiceThreadUnitRatio * getWeight("yarn, wool")
bodiceWeightWool = bodiceSqFtWool * getWeight("wool cloth")
recipeStorage["bodice"] = Recipe("tailor",(bodiceWeightWool + bodiceThreadWeight,"lb"),
                                    [],
                                    [("wool cloth",bodiceSqFtWool), ("yarn, wool",bodiceThreadWeight)],
                                    description="women's garment, laced in front; not restrictive like corset")

knitCapSqFtWool = Decimal(1.25)
knitCapWeightWool = knitCapSqFtWool * getWeight("wool cloth")
recipeStorage["knit cap"] = Recipe("tailor",(knitCapWeightWool,"lb"),
                                    [],
                                    [("wool cloth",knitCapSqFtWool)],
                                    description="woolen cap, like a tuque")

stockingSqFtWool = Decimal(1)
stockingWeightWool = stockingSqFtWool * getWeight("wool cloth")
recipeStorage["stocking"] = Recipe("tailor",(stockingWeightWool,"lb"),
                                    [],
                                    [("wool cloth",stockingSqFtWool)],
                                    description="analogue to modern sock, rising to the knee; for men and women alike")

breechesWoolSqFt = Decimal(5)
breechesWoolWeight = breechesWoolSqFt * getWeight("wool cloth")
breechesThreadFeet = Decimal(20)
breechesThreadUnitRatio = breechesThreadFeet / getUnitSize("yarn, wool")
breechesThreadWeight = breechesThreadUnitRatio * getWeight("yarn, wool")
breechesTotalWeight = breechesWoolWeight + breechesThreadWeight
recipeStorage["breeches"] = Recipe("tailor",(breechesWoolWeight,"lb"),
                                   [],
                                   [("wool cloth",breechesWoolSqFt),("yarn, wool",breechesThreadWeight),("button, ceramic",0.75)], #uses 18 buttons
                                   description="tight knee-length pants with no pockets; worn by all social levels")


shoeHeelTimberCuFt = (Decimal(1)/Decimal(12) ** 2) * (Decimal(0.5)/Decimal(12))
shoeHeelTimberWeight = shoeHeelTimberCuFt * densityTimber
shoeLeatherSqFt = 1
shoeLeatherUnitRatio = shoeLeatherSqFt / getUnitSize("tanned cowhide")
shoeLeatherWeight = shoeLeatherUnitRatio * getWeight("tanned cowhide")
recipeStorage["shoe"] = Recipe("cobbler",(shoeLeatherWeight + shoeHeelTimberWeight,"lb"),
                               [("timber",shoeHeelTimberCuFt)],
                               [("tanned cowhide",shoeLeatherUnitRatio)], # NOT the weight of the leather: for cowhide you need to use the square footage for recipe components
                               description="leather and wood construction")

recipeStorage["mace, masterwork"] = Recipe("blacksmith",(getWeight("mace haft") + (6 * maceFlangeWeight),"lb"),
                                           [],
                                           [("mace haft",1),("mace flange",6)],
                                           difficulty=8,
                                           description="1d8 damage, one-handed, melee; haft is 2 ft.")

hoodSqFtWool = Decimal(2)
hoodWeightWool = hoodSqFtWool * getWeight("wool cloth")
recipeStorage["hood"] = Recipe("tailor",(hoodWeightWool,"lb"),
                                    [],
                                    [("wool cloth",hoodSqFtWool)],
                                    description="separate article of clothing")

recipeStorage["aqua fortis"] = Recipe("alchemist",(8,"oz"),
                                      [("copper ore",3)],
                                      [],
                                      difficulty=4, # for cleaning and separating the ore and doing the lab work
                                      description="acid of moderate strength")

# chicken takes ~6 months to reach maturity, and then produces eggs
# alternatively can be slaughtered (in reality it only takes about 4 months until it's slaughterable)
# 6 months of food, 4 weeks per month, and let's say 2 lbs of feed per week -- it's hard to judge
chickenFeedLbs = 6 * 4 * 2
recipeStorage["chicken, mature"] = Recipe("farmer",(5,"lb"),
                                          [], # we say the cost of land for chickens is negligible
                                          [("cattle feed",chickenFeedLbs)],
                                          description="can be slaughtered or kept to lay eggs")

# a single chicken, once mature, will produce eggs
# it will produce approximately 300 eggs in the first year, and less in future years, so let's adjust that first-year figure to 250 for a more average number
# in the meantime it must be fed for a year -- 2 lbs of feed per week
egglayingChickenFeedLbs = 52 * 2 # 52 weeks per year
eggsChickenPerYear = 250
# we will price a single egg
recipeStorage["egg, chicken"] = Recipe("farmer",(Decimal(0.125),"lb"),
                                       [],
                                       [("chicken, mature",1/eggsChickenPerYear),("cattle feed",egglayingChickenFeedLbs/eggsChickenPerYear)])

# base volume of starting material: 6 x 2 x 2 inches
statuetteCuFt = (Decimal(6)/Decimal(12)) * (Decimal(2)/Decimal(12) ** 2)
# let's assume 1/4 of the material is carved away
statuetteWoodWeight = Decimal(0.75) * (statuetteCuFt * densityTimber)
recipeStorage["statuette, wooden, small"] = Recipe("carver",(statuetteWoodWeight,"lb"),
                                            [("timber",statuetteCuFt)],
                                            [],
                                            description="6x2x2 in.")

# life size, thus starting from a block of wood six feet tall, and 1 foot on a side
statueCuFt = 6 * 1 * 1
# let's assume 1/4 of the material is carved away
statueWoodWeight = Decimal(0.75) * (statueCuFt * densityTimber)
recipeStorage["statue, wooden, life-size"] = Recipe("carver",(statueWoodWeight,"lb"),
                                                    [("timber",statueCuFt)],
                                                    [],
                                                    difficulty=4,
                                                    description="purchaser must wait 1d4 weeks before completion (ask DM about waiting dice); subject of statue must pose for 3 days of that time")

gongSmallRadius = 1
gongSmallThickness = Decimal(0.5)/12
gongSmallBodyCuFt = cylinderCuFt(gongSmallRadius,gongSmallThickness)
# the 'button' in the middle of the gong, modeled as a sphere's worth of metal,
# with one half on each side of the gong
gongSmallButtonRadius = 1/12
gongSmallButtonCuFt = sphereCuFt(gongSmallButtonRadius)
# the total cubic feet is the body, plus the button,
# MINUS the chunk of the middle where the body and button volumes would intersect
# (if they were actually separate pieces, although they're not)
# that intersection volume is a cylinder as wide as the button,
# and as high as the body.
# in addition, we must also subtract away the volumes of two 1 inch holes,
# through which ropes are passed to hold the gong in its frame
gongSmallIntersectionCuFt = cylinderCuFt(gongSmallButtonRadius, gongSmallThickness)
gongHoleCuFt = cylinderCuFt(0.5/12, gongSmallThickness)
gongSmallTotalCuFt = gongSmallBodyCuFt + gongSmallButtonCuFt - gongSmallIntersectionCuFt - (2*gongHoleCuFt)
gongSmallWeight = gongSmallTotalCuFt * densityBronze
recipeStorage["gong, small"] = Recipe("blacksmith",(gongSmallWeight,"lb"),
                                      [],
                                      [("bronze",gongSmallWeight)],
                                      description="bronze button gong; " + str(2*gongSmallRadius) + " feet across; with 2 holes for hanging")

# gong frame for small gong
# made from 6 chunks of wood, all being 1x1 inches wide and high
# the 4 longer pieces form a square frame around the gong,
# the 2 shorter pieces are attached perpendicular to the frame, forming the base
gongSmallFrameCommonElement = Decimal(1)/Decimal(12) ** 2
gongSmallFrameLongPiece = gongSmallFrameCommonElement * Decimal(1.5 * (2 * gongSmallRadius))
gongSmallFrameShortPiece = gongSmallFrameCommonElement * gongSmallRadius
gongSmallFrameTotalCuFt = (4 * gongSmallFrameLongPiece) + (2 * gongSmallFrameShortPiece)
gongSmallFrameWeight = gongSmallFrameTotalCuFt * densityTimber
recipeStorage["gong frame, small, plain"] = Recipe("carpenter",(gongSmallFrameWeight,"lb"),
                                                   [("timber",gongSmallFrameTotalCuFt)],
                                                   [],
                                                   description="square wooden frame to hold gong; requires 2 6-inch ropes")

torchStakeRadius = Decimal(0.5)/Decimal(12)
torchStakeLength = 3 # feet
torchStakeCuFt = cylinderCuFt(torchStakeLength, torchStakeRadius)
torchStakeWeight = torchStakeCuFt * densityTimber
recipeStorage["torch stake"] = Recipe("carver",(torchStakeWeight,"lb"),
                                      [("timber",torchStakeCuFt)],
                                      [],
                                      description="reusable; " + str(torchStakeLength) + " feet long")

# 4 inches by 2 feet
torchFuelWoolSqFt = 2 * Decimal(4)/Decimal(12)
torchFuelWoolWeight = torchFuelWoolSqFt * getWeight("wool cloth")
recipeStorage["torch fuel"] = Recipe("carver",(torchFuelWoolWeight + pitchInFlaskWeightPitch,"lb"),
                                     [],
                                     [("wool cloth",torchFuelWoolSqFt),("pitch",1)],
                                     description="illuminates 5-hex radius for 30 minutes. Wool, soaked in pitch")

spikeLength = 1
spikeRadius = 0.75
spikeCuFt = coneCuFt(spikeLength,Decimal(spikeRadius)/Decimal(12))
spikeWeight = spikeCuFt * densityWroughtIron
recipeStorage["spike"] = Recipe("blacksmith",(spikeWeight,"lb"),
                                [],
                                [("wrought iron",spikeWeight)],
                                description="for affixing ropes and chains; " + str(spikeLength) + " ft long, " + str(2 * spikeRadius) + " in. thick")

cakeSpongeNumEggs = 3
cakeSpongeWeightEggs = cakeSpongeNumEggs * getWeight("egg, chicken")
# the recipe I found said to use the same weight of sugar, flour, and fat (suet) as the eggs
recipeStorage["cake, sponge"] = Recipe("baker",(cakeSpongeWeightEggs*4,("lb")),
                                       [],
                                       [("egg, chicken",3),("brown sugar",cakeSpongeWeightEggs),("flour",cakeSpongeWeightEggs),("suet",cakeSpongeWeightEggs)],
                                       description="light and fluffy cake")

