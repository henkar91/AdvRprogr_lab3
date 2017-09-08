dijkstra <-
function(graph, init_node){
  stopifnot(is.data.frame(graph), length(init_node)==1)
  v1 <- numeric()
  v2 <- numeric()
  w <- numeric()
  mat <- matrix(999,max(graph[1]),max(graph[1]))
  print(length(graph[,1]))
  # setting up the distance matrix wtih known values so far. The indirect links are set to 999
  for (i in 1:length(graph[,1])){
    v1 <- graph[i,1]
    v2 <- graph[i,2]
    w <- graph[i,3]
   mat[v1,v2] <- w
  }
  
  n=length(mat)
  dest=n #destination node
  #create empty variables to store data
  dest = numeric(n)
  v = init_node
  flag = numeric(n)
  prev = numeric(n)
  # for every node in the network
  for(i in 1:n){
    prev[i] = -1
    dest[i] = mat[v,i] #= distance from start node v to every other node i in the network
  }
  
  #initialise counter which keeps track of number of steps through network
  count=2
  
  # until we have reached our destination node n
  while(count <= n){
    min=999
    
    # loop over each node
    for(w in 1:n){
      #if the new path is less long than the existing smallest one and flag[w] is equal to zero (aka we've not already incuded that node in route)
      if(dest[w] < min && !flag[w]){
        # overwrite the minimum with the new shortest path and update counter
        min=dest[w]
        u=w
      }
    }
    flag[u] = 1 #indicate that we go to this site
    count = count+1
    
    # loop over each node again keeping in mind where we have already been
    for(w in 1:n){
      #if the new route is shorter than the previous route
      if((dest[u]+mat[u,w] < dest[w]) && !flag[w]){
        dest[w]=dest[u]+mat[u,w] #update the distance to destination
        prev[w]=u #keep track of the node visited
      }
    }
  }
  return(prev)
}
