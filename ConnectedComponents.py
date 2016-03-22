def DFS_forComponents(source, graph, num, components, visited):
    """Depth-first search as helper function for getConnectedComponents."""
    for dest in graph[source]:
        if visited[dest] == False:
            visited[dest] = True
            components[num].append(dest)
            DFS_forComponents(dest, graph, num, components, visited)

def getConnectedComponents(graph):
    """Argument is an nested-dict 'adjacency-list' representation of a graph.
    Returns dictionary. Keys are integers; values are lists.
    Each value-list contains the nodes of the graph which belong to that component."""
    components = {}
    numComponents = 0
    visited = {g:False for g in graph}
    for g in graph:
        if visited[g] == False:
            numComponents += 1
            components[numComponents] = []
            components[numComponents].append(g)
            DFS_forComponents(g, graph, numComponents, components, visited)
    for c,d in components.items():
        d = list(set(d))
        components[c] = d # don't know why just assigning to d isn't working, but oh well!
    return(components)

# sample graph
graph = {'shrek': {'xyzzy': 1}, 'bar': {'baz': 1, 'foo': 1}, 'baz': {'bar': 1, 'foo': 1}, 'quux': {}, 'foo': {'bar': 1, 'baz': 1}, 'xyzzy': {'shrek': 1}}
