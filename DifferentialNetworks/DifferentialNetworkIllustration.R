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

p <- 5

# Randomly generate the covariance matrices

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

# Graphical Model

diag(corr_mat) <- 0 # We set the diagonal to zero so we do not have joins between nodes and themselves

cor_g <- graph_from_adjacency_matrix(corr_mat, mode='undirected', weighted = 'correlation') # We extract the network
cl <- clusters(cor_g) # We find the clusters in the data

deg <- degree(cor_g, mode="all") + 5 # We find the degree of "betweenness" of nodes

pdf("Network_One.pdf",10,10)
igraph.options(plot.layout=layout.graphopt)
plot(cor_g , vertex.color=cl$membership+1L, vertex.size = deg*5, vertex.label.cex = 2) 
dev.off()

mat_1 <- corr_mat

# Randomly generate the covariance matrices

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

# Graphical Model

diag(corr_mat) <- 0 # We set the diagonal to zero so we do not have joins between nodes and themselves

cor_g <- graph_from_adjacency_matrix(corr_mat, mode='undirected', weighted = 'correlation') # We extract the network
cl <- clusters(cor_g) # We find the clusters in the data

deg <- degree(cor_g, mode="all") + 5 # We find the degree of "betweenness" of nodes

pdf("Network_Two.pdf",10,10)
igraph.options(plot.layout=layout.graphopt)
plot(cor_g , vertex.color=cl$membership+1L, vertex.size = deg*5, vertex.label.cex = 2) 
dev.off()

mat_2 <- corr_mat

diff <- mat_1 - mat_2

cor_g <- graph_from_adjacency_matrix(diff, mode='undirected', weighted = 'correlation') # We extract the network
cl <- clusters(cor_g) # We find the clusters in the data

deg <- degree(cor_g, mode="all") + 5 # We find the degree of "betweenness" of nodes

pdf("Network_Diff.pdf",10,10)
igraph.options(plot.layout=layout.graphopt)
plot(cor_g , vertex.color=cl$membership+1L, vertex.size = deg*5, vertex.label.cex = 2) 
dev.off()

