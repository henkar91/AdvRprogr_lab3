# Lab 3

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
Dijkstra <- function(graph, init_node){
  stopifnot(is.data.frame(graph), length(init_node)==1)
  Q <- c()
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


# Authors: Henrik Karlsson, Milda Poceviciute

# 1.1 Write R-code
# 1.1.1 euclidean()
euclidean <- function(a, b){
        stopifnot(is.numeric(a) & is.numeric(b))
        
        i <- 0
        while(b != 0){
                i <- i + 1
                
                t <- b
                b <- a %% b
                a <- t
        }
        return(a)
}

euclidean(100, 1000)

# 1.1.2* dijkstra()
dijkstra <- function(graph, init_node){
        stopifnot(is.numeric(init_node) & init_node == length(1))
        
        
}


