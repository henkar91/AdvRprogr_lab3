\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    stopifnot(is.data.frame(graph), length(init_node) == 1)
    v1 <- numeric()
    v2 <- numeric()
    w <- numeric()
    mat <- matrix(999, max(graph[1]), max(graph[1]))
    print(length(graph[, 1]))
    for (i in 1:length(graph[, 1])) {
        v1 <- graph[i, 1]
        v2 <- graph[i, 2]
        w <- graph[i, 3]
        mat[v1, v2] <- w
    }
    n = length(mat)
    dest = n
    dest = numeric(n)
    v = init_node
    flag = numeric(n)
    prev = numeric(n)
    for (i in 1:n) {
        prev[i] = -1
        dest[i] = mat[v, i]
    }
    count = 2
    while (count <= n) {
        min = 999
        for (w in 1:n) {
            if (dest[w] < min && !flag[w]) {
                min = dest[w]
                u = w
            }
        }
        flag[u] = 1
        count = count + 1
        for (w in 1:n) {
            if ((dest[u] + mat[u, w] < dest[w]) && !flag[w]) {
                dest[w] = dest[u] + mat[u, w]
                prev[w] = u
            }
        }
    }
    return(prev)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
