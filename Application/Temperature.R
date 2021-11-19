# Hotter states vs Colder States

library(dineR)
library(igraph)

data <- read.csv("us_states_misc_stats.csv")

data_cleaned <- data[,3:ncol(data)]

data_cleaned <- subset(data_cleaned, select=-c(mean_temperature))

max_temp <- max(data$mean_temperature)
min_temp <- min(data$mean_temperature)

diff_temp <- max_temp - min_temp

hotter_states <- data_cleaned[data$mean_temperature > (diff_temp/2 + min_temp),]
colder_states <- data_cleaned[data$mean_temperature < (diff_temp/2 + min_temp),]

hotter_npn <- npn(hotter_states)
colder_npn <- npn(colder_states)

results <- estimation(hotter_npn, colder_npn, nlambda = 50, tuning = "EBIC")

diff_net <- results$path[results$loss_index][[1]]

diff_net_mat <- as.matrix(diff_net)

non_zero_relationship <- colnames(data_cleaned)[col(diff_net_mat)[which(!diff_net_mat == 0)]]

network <- graph_from_adjacency_matrix(diff_net, mode='undirected', weighted = "correlation")
cl <- clusters(network)
deg <- degree(network, mode="all") + 5

pdf("Temperature_Diff_Net.pdf",10,10)
plot(network, vertex.color=cl$membership+1L, vertex.size = deg*3, vertex.label.cex = 1.5, 
     edge.width = 3, edge.color = "black") 
legend("bottomright",
       legend = c("1 - Deaths per 100 000", "13 - Population per square mile"),
       pt.bg  = c("lightblue", "steelblue"),
       pch    = 21,
       cex    = 1.2,
       title  = "Variable")
dev.off()
