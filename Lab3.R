# Lab 3
# Authors: Henrik Karlsson, Milda Poceviciute

# 1.1 Write the R code
# 1.1.1 euclidean()

euclidean <- function(a, b) {
        stopifnot(is.numeric(a), is.numeric(b))
        while (b != 0) {
                t <- b
                b <- a %% b
                a <- t
                return(a)
                
        }
}

# 1.1.2 dijkstra()
dijkstra <- function(graph, init_node) {
        stopifnot(is.data.frame(graph), 
                  is.numeric(init_node) & length(init_node) == 1)
        q <- c()
        # for each vertex v in Graph:             // Initialization
        # dist[v] ← INFINITY                  // Unknown distance from init node to v
        # prev[v] ← UNDEFINED                 // Previous node in optimal path from init node
        # add v to Q                          // All nodes initially in Q (unvisited nodes)
        #
        # dist[init node] ← 0                        // Distance from init node to init node
        #
        # while Q is not empty:
        #   u ← vertex in Q with min dist[u]    // Node with the least distance will be selected first
        # remove u from Q
        #
        # for each neighbor v of u:           // where v is still in Q.
        # alt ← dist[u] + length(u, v)
        # if alt < dist[v]:               // A shorter path to v has been found
        # dist[v] ← alt
        # prev[v] ← u
        #
        # return dist[], prev[]
}



wiki_graph <-
        data.frame(v1 = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                   v2 = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                   w = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
