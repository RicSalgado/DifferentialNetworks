# Democratic states vs Republican States

library(dineR)
library(igraph)

data <- read.csv("us_states_misc_stats.csv")

data_cleaned <- data[,3:ncol(data)]

data_cleaned <- subset(data_cleaned, select=-c(dem_margin_2020))
  
democrats <- data_cleaned[data$dem_margin_2020 > 0,]
republicans <- data_cleaned[data$dem_margin_2020 < 0,]

democrats_NPN <- npn(democrats)
republicans_NPN <- npn(republicans)

results <- estimation(democrats_NPN, republicans_NPN, nlambda = 50, tuning = "EBIC")

diff_net <- results$path[results$loss_index][[1]]

diff_net_mat <- as.matrix(diff_net)

non_zero_relationship <- colnames(data_cleaned)[col(diff_net_mat)[which(!diff_net_mat == 0)]]

network <- graph_from_adjacency_matrix(diff_net, mode='undirected', weighted = "correlation")
cl <- clusters(network)
deg <- degree(network, mode="all") + 5


pdf("Politics_Diff_Net.pdf",10,10)
plot(network, vertex.color=cl$membership+1L, vertex.size = deg*4.5, vertex.label.cex = 1.5, 
     edge.width = 3, edge.color = "black") 
legend("bottomright",
        legend = c("9 - Income per Capita", "12 - Population per square mile"),
        pt.bg  = c("lightblue", "steelblue"),
        pch    = 21,
        cex    = 1.2,
        title  = "Variable")
dev.off()
