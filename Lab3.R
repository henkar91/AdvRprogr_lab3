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
# Solution from Wikipedia: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
# Acknowledgement to Simon Jönsson
wiki_graph <-
        data.frame(
                v1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6),
                v2 = c(2, 3, 6, 1, 3, 4, 1, 2, 4, 6, 2, 3, 5, 4, 6, 1, 3, 5),
                w = c(7, 9, 14, 7, 10, 15, 9, 10, 11, 2, 15, 11, 6, 6, 9, 14, 2, 9))


dijkstra <- function(graph, init_node) {
        stopifnot(is.data.frame(graph),
                  is.atomic(init_node) & length(init_node) == 1)
        
        q <- c()
        dist <- c()
        prev <- c()
        
        for(v in unique(c(graph$v1, graph$v2))){
                dist[v] <- Inf
                prev[v] <- NA
                q[v] <- v
        }
        
        dist[init_node] <- 0
        while(length(q) != 0){
                u <- q[which.min(dist[q])]
                q <- q[q != u]
                
                for(v in graph$v2[graph$v1 == u]){
                        w <- graph[ ,3][graph$v1 == u & graph$v2 == v]
                        alt <- dist[u] + w
                        
                        if(alt < dist[v]){
                                dist[v] <- alt
                                prev[v] <- u
                        }
                }
        } 
        return(dist)
}
dijkstra(wiki_graph, 3)


