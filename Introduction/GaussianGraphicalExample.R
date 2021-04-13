# Gaussian Graphical Model Example

#Since we want to deal with Gaussian Data, the best way to guarantee the 
# data is truly normal is through simulation. 

# Import the package to generate the normal observations

library(mixtools)
library(reshape2)
library(ggplot2)
library(igraph)

# Set the seed so the result is reproducible

set.seed(123)

# Specify the number of variables

p <- 10

# Randomly generate the mean and covariance matrices

mu <- runif(p, min=-100, max=100) #Generates 25 random values between -100 and 100

cov_mat <- cov(matrix(rnorm(p*30), ncol=p))

# So now we have a covariance matrix, and we would like for some of the variables to be independent with one another

for(i in 1:p){
  
  for(j in 1:p){
    
    if(abs(cov_mat[i,j]) < 0.15 ){
      
      cov_mat[i,j] = 0
      
    }
    
  }
  
}
  
# So we now have the covariance matrix, we need to get the correlation matrix

Standard_Deviations <- sqrt(diag(cov_mat))
Inverse_Standard_Deviations <- 1/Standard_Deviations
corr_mat <- Inverse_Standard_Deviations * cov_mat * Inverse_Standard_Deviations

# Round the correlation matrix

cormat <- round(corr_mat,2)

# Get the upper triagular of the correlation matrix

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Reorder the correlation matrix so that the variables with the highest correlations are closest

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create the heatmap plot

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  coord_fixed() +  
  scale_x_continuous(limits = c(0.5, 10.5), breaks = 1:10) + 
  scale_y_continuous(limits = c(0.5, 10.5), breaks = 1:10)

png(file="Heatmap.png", res = 100)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.8),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()

#ggsave(filename = "GaussianHeatmap.png", height = 8, width = 10, dpi = 50, device='tiff')

# Graphical Model

diag(corr_mat) <- 0 # We set the diagonal to zero so we do not have joins between nodes and themselves

cor_g <- graph_from_adjacency_matrix(corr_mat, mode='undirected', weighted = 'correlation') # We extract the network
cl <- clusters(cor_g) # We find the clusters in the data

deg <- degree(cor_g, mode="all") + 5 # We find the degree of "betweenness" of nodes

#png(file="GaussianGraph.png", width = 1080, height = 1080)

pdf("GaussianGraph.pdf",10,10)
igraph.options(plot.layout=layout.graphopt)
plot(cor_g , vertex.color=cl$membership+1L, vertex.size = deg*3) 
dev.off()

#plot(cor_g , vertex.color=cl$membership+1L, vertex.size = deg*3) # We plot the graph

#dev.off()
