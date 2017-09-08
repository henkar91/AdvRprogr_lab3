Dijkstra <-
function(graph, init_node){
  stopifnot(is.data.frame(graph), length(init_node)==1)
  v1 <- numeric()
  v2 <- numeric()
  w <- numeric()
  mat <- matrix(999,max(graph[1]),max(graph[1]))
  for (i in 1:length(graph[1])){
    v1 <- graph[i,1]
    v2 <- graph[i,2]
    w <- graph[i,3]
   mat[v1,v2] <- w
  }
  return (mat)
}
