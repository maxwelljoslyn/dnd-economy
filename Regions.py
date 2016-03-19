import random
from Direction import Direction

numRegions = 20

def getRegionCoords(worldModel):
    """Assigns a region (can be a country, a culture, whatever) to each HexData
    stored as a value in the worldModel argument."""
    # return value
    # storage for coordinates belonging to each region
    regionCoords = {x:[] for x in range(1,numRegions+1)}
    unassignedCoords = list(worldModel.keys())
    counter = 1
    # assign starting coordinates for each region
    while counter <= numRegions:
        x = random.choice(unassignedCoords)
        regionCoords[counter].append(x)
        # maintain the property that unassignedCoords only contains coords which are
        # as yet unassigned to a region
        unassignedCoords.remove(x)
        counter+=1
    # assign remaining coords by spreading out from each start point
    remainingRegions = list(range(1,numRegions+1))
    # listify this because we want to be able to remove stuff from it later
    # it's defined outside the while loop because
    # eventually we want runs thru the while loop
    # (i.e. each set of runs thru the inner for loop)
    # to "shrink" to reflect the fact that some regions are "filled up"
    # and cannot expand further
    while unassignedCoords != []:
        for r in remainingRegions:
            # get a list of neighbor lists
            preNeighs = [list(worldModel[x].neighbors.values()) for x in regionCoords[r]]
            # flatten down into one list
            regionalNeighs = []
            for p in preNeighs:
                for c in p:
                    regionalNeighs.append(c)
            unassignedNeighs = [r for r in regionalNeighs if r in unassignedCoords]
            # remove duplicates, since some hexes share neighbors,
            # and we can't remove a given hex from unassignedCoords twice
            unassignedNeighs = list(set(unassignedNeighs))
            if unassignedNeighs == []:
                remainingRegions.remove(r)
            else:
                for u in unassignedNeighs:
                    regionCoords[r].append(u)
                    unassignedCoords.remove(u)

    # now return the coords split up by region. voila!
    # note: sea hexes are not appropriately assigned to region 0 in this method
    # because for some reason that's just not working right
    # so instead that happens after calling this method,
    # inside WorldGenerator
    return regionCoords
