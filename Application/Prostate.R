library(dineR)
library(igraph)
library(spls)

data(prostate)

data <- as.data.frame(prostate)

data_cleaned <- subset(data, select=-c(y))

normal <- data_cleaned[prostate$y == 0,]
tumor <- data_cleaned[prostate$y == 1,]

results <- estimation(normal, tumor, nlambda = 10, lambda_min_ratio = 0.25, max_iter = 300,
                      stop_tol = 0.001, tuning = "EBIC")

# We end up with 549 edges in the best estimate! So we have reduced the focus from 6033 - 549.

diff_net <- results$path[results$loss_index][[1]]

diff_net_mat <- as.matrix(diff_net)

non_zero_relationship <- colnames(data_cleaned)[col(diff_net_mat)[which(!diff_net_mat == 0)]]

network <- graph_from_adjacency_matrix(diff_net, mode='undirected', weighted = "correlation")

iso <- V(network)[degree(network)==0]
network_joined <- delete.vertices(network, iso)

# There are 549 interactions, but these interactions all only occur within 125 of the variables
# Thus allowing a significant ability to focus in on those. 

cl <- clusters(network_joined)

pdf("Prostate_Diff_Net.pdf",12,12)
plot(network_joined, vertex.color=cl$membership+1L, vertex.label = NA, 
     edge.width = 2, edge.color = "black") 
dev.off()


