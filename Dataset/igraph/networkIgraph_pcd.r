install.packages("network")
install.packages("ergm")
library(network)
library(ergm)


#eyes closed
# Read edge list CSV
edges_df_closed <- read.csv("C:/Users/shubh/Downloads/network seminar/Network_Analysis_Seminar/Dataset/closed_eye_graph_pcd.csv", header = TRUE)

# Read node attributes CSV
nodes_df_closed <- read.csv("C:/Users/shubh/Downloads/network seminar/Network_Analysis_Seminar/Dataset/closed_eyes_energy.csv")

node_names_closed <- unique(c(edges_df$start_node, edges_df$end_node))
num_nodes_closed <- length(node_names)

# Create network object
g_closed <- network.initialize(num_nodes_closed, directed = TRUE)
network.vertex.names(g_closed) <- node_names_closed


# Add edges to the network
add.edges(g_closed, tail = match(edges_df_closed$start_node, node_names), head = match(edges_df_closed$end_node, node_names))


# Add attributes to the nodes
set.vertex.attribute(g_closed, "delta", nodes_df_closed$delta[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "theta", nodes_df_closed$theta[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "alpha", nodes_df_closed$alpha[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "gamma", nodes_df_closed$gamma[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "beta", nodes_df_closed$beta[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "frontal", nodes_df_closed$Frontal.Lobe[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "temporal", nodes_df_closed$Temporal.Lobe[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "parietal", nodes_df_closed$Parietal.Lobe[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "occipital", nodes_df_closed$Occipital.Lobe[match(node_names_closed, nodes_df_closed$Channel)])
set.vertex.attribute(g_closed, "central", nodes_df_closed$Central.Region[match(node_names_closed, nodes_df_closed$Channel)])

print(g_closed)


ergm_model_closed <- ergm(g_closed ~ edges + nodecov("delta") + nodecov("theta") + nodecov("alpha") + nodecov("gamma") + nodecov("beta")
                    + nodefactor("frontal") + nodefactor("temporal") + nodefactor("parietal") + nodefactor("occipital") + 
                    nodefactor("central") + nodematch("frontal") + nodematch("temporal") + nodematch("parietal") + nodematch("occipital") + nodematch("central"),
                    control = control.ergm(
                      MCMC.burnin = 1000,
                      MCMC.interval = 100
                    ))


summary(ergm_model_closed)

#nodematch for eyes closed
ergm_model_match_closed <- ergm(g_closed ~ edges + nodematch("frontal") + nodematch("temporal") + nodematch("parietal") + nodematch("occipital") + nodematch("central"),
                    control = control.ergm(
                      MCMC.burnin = 1000,
                      MCMC.interval = 100
                    ))

summary(ergm_model_match_closed)




#open eyes
# Read edge list CSV
edges_df_open <- read.csv("C:/Users/shubh/Downloads/network seminar/Network_Analysis_Seminar/Dataset/open_eye_graph.csv", header = TRUE)

# Read node attributes CSV
nodes_df_open <- read.csv("C:/Users/shubh/Downloads/network seminar/Network_Analysis_Seminar/Dataset/open_eyes_energy.csv")

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
#nodematch for eyes open
ergm_model_match_open <- ergm(g_open ~ edges + nodematch("frontal") + nodematch("temporal") + nodematch("parietal") + nodematch("occipital") + nodematch("central"),
                    control = control.ergm(
                      MCMC.burnin = 1000,
                      MCMC.interval = 100
                    ))

summary(ergm_model_match_open)
