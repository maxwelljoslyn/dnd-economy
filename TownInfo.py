class Town():
    """Stores data associated with a given town."""
    def __init__(self,coord,resources,services):
        self.coord = coord
        self.population = 0
        self.resources = resources
        self.services = services
        self.blackMarket = False

towns = {}
connections = {}

def addConnection(town_a,town_b):
    # check against not-yet-inserted
    if town_a not in connections:
        connections[town_a] = []
    connections[town_a].append(town_b)
    if town_b not in connections:
        connections[town_b] = []
    connections[town_b].append(town_a)

# the meat of this file: the actual data on each town,
# and setting up the connections between them
# note that the distance of the roads connecting towns
# is not determined by hand!
# the computer will do that for me,
# finding both the path of the road and the distance along it
    
towns["Veder Vek"] = Town((44,-62,18),
                          {"coal":2, "iron ore":3, "gold ore":1},
                          {"blacksmith":2})
towns["Ekodo"] = Town((44,-65,21),
                      {"clay":1, "nickel ore":1,"silver ore":1, "gold ore":2},
                      {"silversmith":1, "goldsmith":2})
towns["Gootonu"] = Town((43,-66,23),
                        {"limestone":1, "timber":1, "gold ore":1},
                        {"smelter":2, "baker":1, "brewer":1, "tanner":1})
towns["Torkada"] = Town((42,-67,25),
                        {"arable land":1, "clay":1, "cereal":2},
                        {"butcher":1,"potter":1, "dyer":1, "miller":1})
towns["Serdabach"] = Town((45,-68,23),
                          {"salt":1, "arable land":1, "timber":2},
                          {"smelter":1, "farmer":1, "carpenter":1, "brewer":1})
towns["Berdorl"] = Town((43,-69,26),
                        {"salt":1,"coal":1, "iron ore":1, "limestone":1},
                        {"potter":1, "tanner":1, "blacksmith":1})
towns["Kededal"] = Town((43,-60,17),
                        {"arable land":2, "hops":1},
                        {"vintner":1})
towns["Yog"] = Town((45,-60,15),
                    {"arable land":1, "limestone":1},
                    {"farmer":1, "tanner":1, "brewer":1, "cooper":1, "leatherworker":1})
towns["Dreot"] = Town((47,-67,20),
                      {"arable land":2, "timber":1, "cereal":1},
                      {"miller":1, "carpenter":1, "cooper":1})
towns["Guroff"]=Town((47,-63,16),
                     {"clay":1, "gold ore":2, "silver ore":1, "nickel ore":1},
                     {"goldsmith":1, "silversmith":1, "smelter":1})
towns["Nender"] = Town((48,-66,18),
                       {"lapis lazuli":1,"manganese ore":1},
                       {"potter":1})
towns["Pwodd"] = Town((49,-65,16),
                      {"arable land":1,"cereal":1, "hops":1},
                      {"tailor":1, "baker":1})
towns["Widder"] = Town((49,-63,14),
                       {"arable land":2},
                       {"tailor":1,"weaver":1,"butcher":1})

addConnection("Yog","Guroff")
addConnection("Pwodd","Widder")
addConnection("Widder","Guroff")
addConnection("Nender","Pwodd")
addConnection("Dreot","Nender")
addConnection("Dreot","Ekodo")
addConnection("Yog","Kededal")
addConnection("Kededal","Veder Vek")
addConnection("Gootonu","Serdabach")
addConnection("Torkada","Berdorl")
addConnection("Torkada", "Gootonu")
addConnection("Ekodo","Gootonu")
addConnection("Veder Vek","Ekodo")
