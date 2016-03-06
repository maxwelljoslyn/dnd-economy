import random
from Direction import Direction

numRegions = 20
dirNames = [n for n in Direction.__members__.keys()]

def getRegionCoords(worldModel):
    """Assigns a region (can be a country, a culture, whatever) to each HexData
    stored as a value in the worldModel argument."""
    # return value
    # storage for coordinates belonging to each region
    regionCoords = {x:[] for x in range(1,numRegions+1)}
    unassignedCoords = list(worldModel.keys())
    print("FOOOFOOFOO", unassignedCoords, "DOODOODOO")
    counter = 1
    # assign starting coordinates for each region
    while counter <= numRegions:
        x = random.choice(unassignedCoords)
        regionCoords[counter].append(x)
        # maintain the property that unassignedCoords only contains coords which are
        # as yet unassigned to a region
        unassignedCoords.remove(x)
        counter+=1
    # assign all remaining coords by branching out from each start point
    while unassignedCoords != []:
        for i in range(1,numRegions+1):
            # select a coord in those already belonging to region i
            # to serve as a "launch pad" from which to grab a neighboring coord
            launchpad = random.choice(regionCoords[i])
            # get its neighbors;
            # we'll add as many as we can to the region
            launchNeighs = worldModel[launchpad].neighbors.values()
            # remove any neighbors which are invalid
            # i.e. some region has already claimed them
            validLaunchNeighs = [c for c in launchNeighs if c in unassignedCoords]
            # no valid launch neighs? oh well, I say. better luck next loop
            for v in validLaunchNeighs:
                regionCoords[i].append(v)
                unassignedCoords.remove(v)
    # having gone through all valid coords, we're almost done
    # first, though, we want to take the region label OFF water hexes
    # regions are supposed to be confined to land only
    for r,vals in regionCoords.items():
        for v in vals:
            if not worldModel[v].isLand:
                vals.remove(v)
    # all done, regions are nice and dry.
    # now return the coords split up by region. voila!
    return regionCoords
