## Installing packages
install.packages("igraph")
install.packages("ergm")
install.packages("netrankr")
install.packages(("magrittr"))

library(igraph)
library(netrankr)
library(magrittr)

vertices()

flomarriage$val


eeg$oci = 1
fm <- flomarriage
plot(flomarriage)
flomarriage
m1 = ergm(fm ~ edges + nodefactor("wealth"))
summary(m1)

## Plotting 
data("florentine_m")
# Delete Pucci family (isolated)
florentine_m <- delete_vertices(florentine_m, which(degree(florentine_m) == 0))
florentine_m

# plot the graph (label size proportional to wealth)
set.seed(111)
plot(florentine_m,
  vertex.label.cex = V(florentine_m)$wealth * 0.01,
  vertex.label.color = "black",
  vertex.color = "white",
  vertex.frame.color = "gray"
)


cent.df <- data.frame(
  degree = degree(florentine_m),
  betweenness = betweenness(florentine_m),
  closeness = closeness(florentine_m),
  eigenvector = eigen_centrality(florentine_m)$vector,
  subgraph = subgraph_centrality(florentine_m)
)

P <- neighborhood_inclusion(florentine_m)

