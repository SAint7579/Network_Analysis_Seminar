install.packages("network")
install.packages("ergm")
library(network)
library(ergm)


edges_df_open <- read.csv("C:/Users/vishw/OneDrive/Desktop/Projects/Network_Analysis_Seminar/Dataset/Graph/PD3565.csv", header = TRUE)

# Read node attributes CSV
nodes_df_open <- read.csv("C:/Users/vishw/OneDrive/Desktop/Projects/Network_Analysis_Seminar/Dataset/Energy/PD3565.csv")

node_names_open <- unique(c(edges_df_open$start_node, edges_df_open$end_node))
num_nodes_open <- length(node_names_open)

# Create network object
g_open <- network.initialize(num_nodes_open, directed = TRUE)
network.vertex.names(g_open) <- node_names_open


# Add edges to the network
add.edges(g_open, tail = match(edges_df_open$start_node, node_names_open), head = match(edges_df_open$end_node, node_names_open))


# Add attributes to the nodes
set.vertex.attribute(g_open, "delta", nodes_df_open$delta[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "theta", nodes_df_open$theta[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "alpha", nodes_df_open$alpha[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "gamma", nodes_df_open$gamma[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "beta", nodes_df_open$beta[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "frontal", nodes_df_open$Frontal.Lobe[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "temporal", nodes_df_open$Temporal.Lobe[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "parietal", nodes_df_open$Parietal.Lobe[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "occipital", nodes_df_open$Occipital.Lobe[match(node_names_open, nodes_df_open$Channel)])
set.vertex.attribute(g_open, "central", nodes_df_open$Central.Region[match(node_names_open, nodes_df_open$Channel)])
print(g_open)


ergm_model_open <- ergm(g_open ~ edges + nodecov("delta") + nodecov("theta") + nodecov("alpha") + nodecov("gamma")  + nodecov("beta") +
                    nodefactor("frontal") + nodefactor("temporal") + nodefactor("parietal") + nodefactor("occipital") +
                    nodefactor("central") + nodematch("frontal") + nodematch("temporal") + nodematch("parietal") + nodematch("occipital") + nodematch("central"),
                    control = control.ergm(
                      MCMC.burnin = 1000,
                      MCMC.interval = 100
                    ))


summary(ergm_model_open)
