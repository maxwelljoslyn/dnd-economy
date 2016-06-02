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
    
towns["Veder Vek"] = Town((44,-62,18),{},{})
towns["Ekodo"] = Town((44,-65,21),{},{})
towns["Goot Ronu"] = Town((43,-66,23),{},{})
towns["Goot Torkada"] = Town((42,-67,25),{},{})
towns["Serdabach"] = Town((45,-68,23),{},{})
towns["Otalo Vek"] = Town((43,-69,26),{},{})

addConnection("Goot Ronu","Serdabach")
addConnection("Goot Torkada","Otalo Vek")
addConnection("Goot Torkada", "Goot Ronu")
addConnection("Ekodo","Goot Ronu")
addConnection("Veder Vek","Ekodo")
