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
densityTallow = Decimal(54.09)
densityTin = Decimal(456.3484)
densityCopper = 559

def cylinderCuFt(height,radius):
    height = Decimal(height)
    radius = Decimal(radius)
    val = Decimal(pi) * height * (radius ** 2)
    return val

def sphereCuFt(radius):
    val = Decimal(pi) * Decimal(4/3) * (Decimal(radius) ** 3)
    return val

def triangularPrismCuFt(base,height,thickness):
    """The volume of a prism having two triangular faces and three quadrilateral ones.
    Like two 2D triangles with the area between them filled in."""
    base = Decimal(base)
    height = Decimal(height)
    thickness = Decimal(thickness)
    val = ((base * height) / 2) * thickness
    return val

def getUnitSize(name):
    """Convenience method to get size of unit -- mostly useful when calculating weights from components."""
    return recipeStorage[name].unit[0]


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
densityPewter = 1 / volumePewterIngot
# comes out to 469.3 lbs/cubic ft
# checked it for sanity and the density looks good for an alloy with this much copper.
recipeStorage["pewter"] = Recipe("smelter",(1,"lb"),
                                 [("tin ore",pewterTinProportion),("copper ore",pewterCopperProportion),
                                  ("coal",Decimal(0.5)),("limestone",Decimal(0.5))],
                                 [],
                                 description="ingot, 1x1x3.65 in.")

recipeStorage["wrought iron"] = Recipe("smelter",(1,"lb"),
                                       [("coal",Decimal(2.25)*Decimal(2/9)),("limestone",Decimal(1.125)*Decimal(2/9))],
                                       [("pig iron",1)],
                                       description="ingot, 1x1x3.57 in.")

recipeStorage["steel"] = Recipe("smelter",(1,"lb"),
                                       [("coal",0.25),("limestone",0.25)],
                                # steel requires half as much coal as other iron stuff
                                # b/c howstuffworks says it only needs to get half as hot
                                       [("pig iron",1)],
                                difficulty = 3,
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
                                 difficulty=1.5,
                                 description="metal knob which holds hilt and blade together")
semiGoods.append("pommel")


# a 1-foot (unit) blade is 2 inches wide, 1/6 inch thick, 1 foot long
unitBladeCuFt = Decimal(2/12) * Decimal(Decimal(1/6)/12) * 1
unitBladeWeight = unitBladeCuFt * densitySteel
recipeStorage["blade"] = Recipe("blacksmith",(unitBladeWeight,"lb"),
                                [],
                                [("steel",unitBladeWeight)],
                                difficulty=6,
                                description="price for a one-foot steel blade")
semiGoods.append("blade")

daggerWeight = getUnitSize("pommel") + getUnitSize("blade hilt") + getUnitSize("blade")
recipeStorage["dagger"] = Recipe("blacksmith",(daggerWeight,"lb"),
                                 [],
                                 [("blade",1),("pommel",1),("blade hilt",1)],
                                 description="1d4 damage, melee or thrown 2/3/4; 1-foot blade")

shortswordWeight = getUnitSize("pommel") + getUnitSize("blade hilt") + (Decimal(2) * getUnitSize("blade"))
recipeStorage["shortsword"] = Recipe("blacksmith",(shortswordWeight,"lb"),
                                 [],
                                 [("blade",2),("pommel",1),("blade hilt",1)],
                                 description="1d6 damage; 2-foot blade")

longswordWeight = getUnitSize("pommel") + getUnitSize("blade hilt") + (Decimal(3.5) * getUnitSize("blade"))
recipeStorage["longsword"] = Recipe("blacksmith",(longswordWeight,"lb"),
                                 [],
                                 [("blade",3.5),("pommel",1),("blade hilt",1)],
                                 description="1d8 damage; 3.5-foot blade")

greatswordWeight = getUnitSize("pommel") + getUnitSize("blade hilt") + (Decimal(4.5) * getUnitSize("blade"))
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

recipeStorage["bread, coarse"] = Recipe("baker",(1,"lb"),
                                       [("salt",0.05)],
                                       [("flour",0.7)],
                                       description="round loaf")

recipeStorage["bread, good"] = Recipe("baker",(1,"lb"),
                                     [("salt",0.05)],
                                     [("flour",0.7)],
                                     difficulty=2,
                                     description="round loaf")

recipeStorage["bread, excellent"] = Recipe("baker",(1,"lb"),
                                     [("salt",0.05)],
                                     [("flour",0.7)],
                                     difficulty=4,
                                     description="round loaf")

recipeStorage["quicklime"] = Recipe("potter",(1,"lb"),
                                    [("limestone",1),("coal",0.5)],
                                    [],
                                    description="used in tanning and to make mortar")

recipeStorage["mortar"] = Recipe("potter",(1,"lb"),
                                 [("clay",0.75)],
                                 [("quicklime",0.25)],
                                 description="in powdered form")

recipeStorage["mature ewe"] = Recipe("farmer",(1,"head"),
                                    [("arable land",Decimal(0.315))],
                                    [],
                                    description="eight months old, 90 lbs, suitable for milking or shearing")

# a lamb which has been grain finished for slaughter
recipeStorage["mutton sheep"] = Recipe("farmer",(1,"head"),
                                    [],
                                    [("mature ewe",1),("horse feed",345)],
                                    description="one year old, 130 lbs, suitable for slaughter")

# one mature ewe produces ~ 200 lbs of milk, once a year during lambing
# thus the division by 200
recipeStorage["sheep milk"] = Recipe("farmer",(milkGallonWeight,"lb"),
                                     [],
                                     [("mature ewe",Decimal(1/200))],
                                     description = "1 gallon")
# sheep for slaughter weighs 120 lbs
# I take the dress percentage to be 55% of that, giving the hanging/carcass weight, and the useable meat to be 75% of the hanging weight
sheepCarcassWeight = 120 * Decimal(0.55)
sheepMeatWeight = sheepCarcassWeight * Decimal(0.75)
# we divide the cost of a mutton sheep by this number to get a price for 1 lb mutton
recipeStorage["mutton"] = Recipe("butcher",(1,"lb"),
                                     [],
                                     [("mutton sheep",Decimal(1/sheepMeatWeight))])

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

# I assume a cow for slaughter weighs 1300 pounds
# taking the carcass weight to be 2/3 of that and the useable meat, in turn, to be 2/3 of carcass weight,
# the remaining meat is 577.7 lbs.
# thus to get a price for 1 lb, we divide the price of the cow by 577.7.
# this approach treats all beef as generic: in reality, a given cow produces different amounts
# of each cut of beef. for now we'll treat them all as the same.
cowSlaughterWeight = 1300
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
                                 [("cow",2/cowCarcassWeight)],
                                 difficulty=3)

# made by leaching ashes in water; we'll use timber for ashes
recipeStorage["lye"] = Recipe("chandler",(1,"lb"),
                              [("timber",1)],
                              [])

# http://www.millennium-ark.net/News_Files/Soap/Lye_Fat_Table.html
# (above page copyright AL Durtschi)
# using the 5% excess fat column given here, the required lye content for soap is 0.133 times the tallow content
lyeTallowRatio = Decimal(0.133)
# finally, we add a little bit of salt to get hard soap instead of soft
# http://www.motherearthnews.com/homesteading-and-livestock/how-to-make-soap-from-ashes-zmaz72jfzfre.aspx
# this article suggests 2.5 pints (3.22 lb) salt for 5 gallons (36.16 lb) of tallow
saltTallowRatio = Decimal(3.22) / Decimal(36.16)
# let's put it all together:
# to find the amount of tallow needed for 1 pound of soap, we solve for x in this equation: 1 = x + (x*lye-tallow ratio) + (x*salt-tallow ratio)
tallowForOneLbSoap = 1 / (1 + saltTallowRatio + lyeTallowRatio)
# finally, using the density of tallow as a proxy, we find the weight of a bar of soap
barSoapInCuFt = Decimal(3/12) * Decimal(2/12) * Decimal(6/12)
weightOneBarSoap = densityTallow * barSoapInCuFt
recipeStorage["soap, hard"] = Recipe("chandler",(weightOneBarSoap,"lb"),
                                    [("salt",saltTallowRatio * weightOneBarSoap)],
                                    [("lye",lyeTallowRatio * weightOneBarSoap),("tallow",tallowForOneLbSoap * weightOneBarSoap)],
                                    description="dimensions: 3x2x6 in.")


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
weightCowhideInOz = Decimal(15 * 16)
cowhideDensityInOzPerSqFt = Decimal(weightCowhideInOz / 50)
recipeStorage["tanned cowhide"] = Recipe("tanner",(15,"lb"),
                                         [],
                                         [("quicklime",0.732),("defleshed cowhide",1)],
                                         description="50 sq ft of cowhide; " + str(cowhideDensityInOzPerSqFt) + " oz/sq. ft, thus a 1-ft square is " + str(Decimal(cowhideDensityInOzPerSqFt/64)) + " in. thick")

recipeStorage["holy symbol, wooden, simple"] = Recipe("carpenter",(1,"lb"),
                                                      [("timber",0.02)],
                                                      [])

recipeStorage["holy symbol, iron, simple"] = Recipe("blacksmith",(1,"lb"),
                                                      [],
                                                      [("wrought iron",1)],
                                                    difficulty=2)

recipeStorage["holy symbol, iron, ornate"] = Recipe("blacksmith",(1,"lb"),
                                                      [],
                                                      [("wrought iron",1)],
                                                    difficulty=4)

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
semiGoods.append("pigment, red/yellow")

# component of blue dye
recipeStorage["ground lapis lazuli"] = Recipe("potter",(1,"lb"),
                                              [("lapis lazuli",1)])
semiGoods.append("ground lapis lazuli")


recipeStorage["pigment, ultramarine"] = Recipe("dyer",(1,"lb"),
                                               [],
                                               [("ground lapis lazuli",1)])
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

recipeStorage["brown sugar"] = Recipe("miller",(1,"lb"),
                                      [("sugarcane",10)],
                                      [],
                                      difficulty=4)

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
                                    [("wrought iron",Decimal(0.9)),("steel",Decimal(0.6))],
                                    difficulty=3)
semiGoods.append("spearhead")

spearHaftCuFt = cylinderCuFt(5,Decimal(0.5/12))
recipeStorage["spear haft"] = Recipe("carpenter",(spearHaftCuFt * densityTimber,"lb"),
                                                  [("timber",spearHaftCuFt)],
                                                  [],
                                                  difficulty=3)
semiGoods.append("spear haft")

spearWeight = recipeStorage["spearhead"].unit[0] + recipeStorage["spear haft"].unit[0]
recipeStorage["spear"] = Recipe("blacksmith",(spearWeight, "lb"),
                                [],
                                [("spearhead",1),("spear haft",1)],
                                description="1d6 damage, one-handed, melee; haft 5 ft. long")

maceShaftCrossSectionRadius = Decimal(0.5) # inches
maceHaftCuFt = cylinderCuFt(2,maceShaftCrossSectionRadius/12)
maceHaftMetalProportion = Decimal(0.25)
maceHaftWoodProportion = 1 - maceHaftMetalProportion
maceHaftMetalWeight = maceHaftMetalProportion * maceHaftCuFt * densityWroughtIron
maceHaftWoodWeight = maceHaftWoodProportion * maceHaftCuFt * densityTimber
recipeStorage["mace haft"] = Recipe("blacksmith",(maceHaftMetalWeight + maceHaftWoodWeight,"lb"),
                                    [("timber",maceHaftWoodWeight)],
                                    [("wrought iron",maceHaftMetalWeight)],
                                    difficulty=3,
                                    description="wood reinforced with metal bands")
semiGoods.append("mace haft")

maceFlangeCuFt = triangularPrismCuFt(Decimal(1/12),Decimal(0.25),Decimal(0.1))
maceFlangeWeight = maceFlangeCuFt * densityWroughtIron
recipeStorage["mace flange"] = Recipe("blacksmith",(maceFlangeWeight,"lb"),
                                      [],
                                      [("wrought iron",maceFlangeWeight)],
                                      difficulty=3)
semiGoods.append("mace flange")

recipeStorage["mace"] = Recipe("blacksmith",(getUnitSize("mace haft") + (6 * maceFlangeWeight),"lb"),
                               [],
                               [("mace haft",1),("mace flange",6)],
                               description="1d8 damage, one-handed, melee; haft is 2 ft.")

wroughtIronIngotCuFt = Decimal(1/12) * Decimal(1/12) * Decimal(3.57/12)
oneFootWireCuFt = cylinderCuFt(1,(gauge16WireThicknessInches/2/12))
feetOfWire = wroughtIronIngotCuFt / oneFootWireCuFt
# can be used for fastening, or turned into rings for mail
# weight of wire is the same as the 1 lb ingot of wrought iron; it's just turned into a different shape
recipeStorage["wire"] = Recipe("blacksmith",(feetOfWire,"feet"),
                               [],
                               [("wrought iron",1)],
                               difficulty=6, # lots of hammering and then lots and lots of pulling
                               description="weight 1 lb; thickness 16 gauge, i.e. 0.05082 in. diameter")

# this is in feet b/c division by 12
mailRingRadius = Decimal(0.2) / 12
# this circumference is also the feet of wire needed for one ring
mailRingCircumference = 2 * Decimal(pi) * mailRingRadius
unitsWirePerRing = mailRingCircumference / getUnitSize("wire")
recipeStorage["mail ring"] = Recipe("blacksmith",(unitsWirePerRing,"lb"),
                                    [],
                                    [("wire",unitsWirePerRing)],
                                    difficulty=5)
semiGoods.append("mail ring")

# rings overlap, which would mean more per linear foot, but we'll ignore that since mail can stretch a little too
ringsToReachOneFootLength = 1 / mailRingRadius
ringsInSquareFootMail = ringsToReachOneFootLength ** 2
recipeStorage["mail sqft"] = Recipe("blacksmith",(getUnitSize("mail ring") * ringsInSquareFootMail,"lb"),
                                    [],
                                    [("mail ring",ringsInSquareFootMail)],
                                    description="1 square foot of mail")
semiGoods.append("mail sqft")

# this is an OK-ish estimate, way better than my first one
hauberkSqFt = 12
recipeStorage["mail hauberk"] = Recipe("blacksmith",(hauberkSqFt * getUnitSize("mail sqft"),"lbs"),
                                       [],
                                       [("mail sqft",hauberkSqFt)],
                                       description="AC 6; has full sleeves, and covers torso to the knees")

# The amount of feet of yarn per pound of wool which I give here is probably a vast under- or overshoot,
# but it's a highly variable amount dependent on thickness of resultant yarn, type of sheep, and
# other factors, so I'll just go ahead and soldier on. Can always fix it later.
recipeStorage["thin yarn"] = Recipe("spinner",(1500,"feet"),
                               [],
                               [("clean wool",1)],
			       description="weight 1 lb; must be spun into thread or yarn to be useful")
semiGoods.append("thin yarn")

recipeStorage["yarn"] = Recipe("spinner",(getUnitSize("thin yarn"),"feet"),
                               [],
                               [("thin yarn",1)],
                               description="weight 1 lb; useable as string and in stitching, ropemaking, etc.")

recipeStorage["thread"] = Recipe("spinner",(getUnitSize("thin yarn")*2,"feet"),
                                 [],
                                 [("thin yarn",1)],
                                 description="weight 1 lb; useable for stitching cloth and textiles")


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
recipeStorage["backpack"] = Recipe("leatherworker",(backpackPortionOfCowhide * getUnitSize("tanned cowhide"),"lb"),
                                                [],
                                                [("thread",Decimal(8) / getUnitSize("thread")),
                                                 ("tanned cowhide",backpackPortionOfCowhide),
                                                 ("yarn",Decimal(1) / getUnitSize("yarn"))],
                                                description="with string clasp; holds 1 cubic foot")

# belt pouch holds very little;
# the total square footage is (0.15^2)*6
beltpouchSqFt = (Decimal(0.15) ** 2) * 6
beltpouchPortionOfCowhide = Decimal(beltpouchSqFt / 50)
recipeStorage["belt pouch"] = Recipe("leatherworker",(beltpouchPortionOfCowhide * getUnitSize("tanned cowhide"),"lb"),
                                                [],
                                                [("thread",Decimal(1.2) / getUnitSize("thread")),
                                                 ("tanned cowhide",beltpouchPortionOfCowhide),
                                                 ("yarn",Decimal(1) / getUnitSize("yarn"))],
                                                description="with string clasp; holds 0.15 cubic foot")

# let's say a belt is 3 feet long and 1 inch wide, and you cut to the appropriate length -- but the leatherworker is gonna charge you for the whole thing.
# you also need maybe 12 feet of thread to make the edges tough
beltSqFt = Decimal(1/12) * 3
beltPortionOfCowhide = Decimal(beltSqFt/50)
recipeStorage["belt"] = Recipe("leatherworker",(beltPortionOfCowhide * getUnitSize("tanned cowhide"),"lb"),
                                                [],
                                                [("thread",Decimal(12) / getUnitSize("thread")),
                                                 ("tanned cowhide",beltPortionOfCowhide)],
                                                description="leather; can attach up to 3 pouches, scabbards, etc.")

# approximate
whistleCuFt = cylinderCuFt(Decimal(0.25), Decimal(0.75/12)) 
whistleWeight = whistleCuFt * densityTimber
recipeStorage["whistle"] = Recipe("carver",(whistleWeight,"lb"),
                                  [("timber",whistleWeight)],
                                  [],
                                  difficulty=2,
                                  description="non-musical; 3 inches long")

# structurally it's a little hollow block
fippleCuFt = Decimal(1/12) * Decimal(1/12) * Decimal(2/12)
fippleWeight = fippleCuFt * densityTimber
recipeStorage["fipple"] = Recipe("carver",(fippleWeight,"lb"),
                                 [("timber", fippleWeight * densityTimber)],
                                 [],
                                 difficulty=3,
                                 description="whistle mouthpiece for certain instruments")
semiGoods.append("fipple")

recorderBodyCuFt = cylinderCuFt(Decimal(1.25), Decimal(0.75/12))
recorderBodyWeight = recorderBodyCuFt * densityTimber
recipeStorage["recorder"] = Recipe("carver",(recorderBodyWeight + fippleWeight,"lb"),
                                   [("timber",recorderBodyWeight)],
                                   [("fipple",1)],
                                   difficulty=4,
                                   description="type of wooden flute; 15 inches long")

# four strands of yarn are twisted into a strand, turning one direction;
# four strands are twisted into a rope, turning the other direction.
# thus 16 feet of yarn makes 1 foot of rope, and a foot of rope weights 16 times as much as a foot of yarn
recipeStorage["rope"] = Recipe("ropewalker",(getUnitSize("yarn")/16,"feet"),
                               [],
                               [("yarn",1)],
                               difficulty=2,
                               description="weighs 16 lbs")

# warning: mostly-bullshit calculations ahead. I just need a figure here.
# if yarn is 1/8 inch thick, then it requires 96 feet of yarn to make a foot square cloth.
# let's round to 100.
yarnFtPerWoolClothSqFt = 100
# the weight calculation below works because the table lists 1 lb of yarn.
recipeStorage["wool cloth"] = Recipe("weaver",(yarnFtPerWoolClothSqFt/getUnitSize("yarn"),"lb"),
                                          [],
                                          [("yarn",yarnFtPerWoolClothSqFt/getUnitSize("yarn"))],
                                     description="1 square foot")
semiGoods.append("wool cloth")

# gambesons are quite thick, with lots of layers
gambesonLayers = 10
# we'll need  feet of thread per layer, to go around the edges of each layer twice, attaching them to the others into a big stack
gambesonThread = gambesonLayers * 8
gambesonSqFt = 12 * gambesonLayers
recipeStorage["quilted gambeson"] = Recipe("weaver",(gambesonSqFt * getUnitSize("wool cloth"),"lb"),
                                           [],
                                           [("wool cloth",gambesonSqFt),("thread",(gambesonThread/getUnitSize("thread")))],
                                           difficulty=2,
                                           description="AC 9; padded cloth armor")

recipeStorage["gemshorn"] = Recipe("carver",(2,"lb"),
                                   [],
                                   [("fipple",1),("cow",Decimal(0.5))],
                                   description="ocarina-type instrument made from bull horn; 8 inches long")
