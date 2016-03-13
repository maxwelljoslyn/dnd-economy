# modified from tutorial at:
# http://www.redblobgames.com/pathfinding/a-star/implementation.html
import heapq
from decimal import *

# set up the Decimal environment
getcontext().prec = 6

class PriorityQueue:
    def __init__(self):
        self.elements = []

    def empty(self):
        return len(self.elements) == 0

    def put(self, item, priority):
        heapq.heappush(self.elements, (priority, item))

    def get(self):
        return heapq.heappop(self.elements)[1]

def elevAwareDistance(a,b,worldModel):
    """Returns distance from coord a to coord b (in days of travel) in the worldModel.
    These two must be neighbors, so an error is given if they aren't."""
    if b not in list(worldModel[a].neighbors.values()):
        raise ValueError("args to elevAwareDistance must be neighbors:",a,b)
    aElev = worldModel[a].elevation
    bElev = worldModel[b].elevation
    distance = 1 # basic distance between two hexes
    # for each full increment of 0.05 of difference in elevation, add one to distance
    diff = abs(aElev - bElev)
    numIncrements = int(diff / Decimal(0.05))
    # call to int is to round down incrementss to whole number
    distance += numIncrements
    return distance

def reconstructPath(came_from, start, goal):
    """Processes path result of AStarSearch into useable list of coords."""
    current = goal
    path = [current]
    while current != start:
        current = came_from[current]
        path.append(current)
    path.reverse()
    return path

# heuristic for A-Star
def cubeDistance(a,b):
    """Distance from a to b on a hexagonal grid,
    where a and b are cube coordinates."""
    return((abs(a[0] - b[0]) + abs(a[1] - b[1]) + abs(a[2] - b[2])) / 2)

def AStarSearch(worldModel, start, goal):
    frontier = PriorityQueue()
    frontier.put(start, 0)
    came_from = {}
    cost_so_far = {}
    came_from[start] = None
    cost_so_far[start] = 0

    while not frontier.empty():
        current = frontier.get()

        # early exit if the goal is reached
        if current == goal:
            break

        eligibleNeighbors = [h for h in list(worldModel[current].neighbors.values())
                             if h != None]
        for next in eligibleNeighbors:
            new_cost = cost_so_far[current] + elevAwareDistance(current,next,worldModel)
            # if either of these are met...
            if next not in cost_so_far or new_cost < cost_so_far[next]:
                # ...then the best way to get to to next from current has changed
                cost_so_far[next] = new_cost
                priority = new_cost + cubeDistance(goal, next)
                frontier.put(next, priority)
                came_from[next] = current
    # return only the relevant information
    return reconstructPath(came_from,start,goal), cost_so_far[goal]
