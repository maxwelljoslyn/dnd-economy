directPathMatrix = \
    {
        "A":
            {"B": 3},
        "B":
            {"A": 3, "C": 4},
        "C":
            {"B": 4, "D": 2, "E": 4},
        "D":
            {"C": 3, "E": 6},
        "E":
            {"C": 1, "D": 2}
    }

# directPathMatrix is part of the hard data about the game world.
# it records, for each city, all paths which lead from it to another city,
# and, most importantly, records the distances of each of those direct paths

#these distances are determined by my manual calculation of distance on the game map