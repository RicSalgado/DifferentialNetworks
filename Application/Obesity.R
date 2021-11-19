library(dineR)
library(igraph)

data <- read.csv("us_states_misc_stats.csv")

data_cleaned <- data[,3:ncol(data)]

data_cleaned <- subset(data_cleaned, select=-c(obesity_rate))

max_obesity <- max(data$obesity_rate)
min_obesity <- min(data$obesity_rate)

diff_obesity <- max_obesity - min_obesity

fatter_states <- data[data$obesity_rate > (diff_obesity/2 + min_obesity),]
thinner_states <- data[data$obesity_rate < (diff_obesity/2 + min_obesity),]

fatter_npn <- npn(fatter_states)
thinner_npn <- npn(thinner_states)

results <- estimation(fatter_npn, thinner_npn, nlambda = 50, tuning = "EBIC")

diff_net <- results$path[results$loss_index][[1]]

diff_net_mat <- as.matrix(diff_net)

non_zero_relationship <- colnames(data_cleaned)[col(diff_net_mat)[which(!diff_net_mat == 0)]]

network <- graph_from_adjacency_matrix(diff_net, mode='undirected', weighted = "correlation")
cl <- clusters(network)
deg <- degree(network, mode="all") + 5

pdf("Obesity_Diff_Net.pdf",10,10)
plot(network, vertex.color=cl$membership+1L, vertex.size = deg*3.5, vertex.label.cex = 1.5, 
     edge.width = 3, edge.color = "black") 
#legend("bottomright",
#       legend = c("9 - Income per Capita", "12 - Population per square mile"),
#       pt.bg  = c("lightblue", "steelblue"),
#       pch    = 21,
#       cex    = 1.2,
#       title  = "Variable")
dev.off()
