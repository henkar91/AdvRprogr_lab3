# Lab 3
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


