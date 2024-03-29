# Install BiocManager, which allows us to access gene data and plotting specific to biological sciences

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")

# We can now install "components" of Bioconductor

BiocManager::install(c("affy", "estrogen", "vsn", "genefilter"))

# We now make use of the installed packages to be able to plot our gene data as a graphical model

library(affy)
library(estrogen)
library(vsn)
library(genefilter)

datadir <- system.file("extdata", package="estrogen")
dir(datadir)
setwd(datadir)

# Read in phenotype data and the raw CEL files
pd <- read.AnnotatedDataFrame("estrogen.txt", header=TRUE, sep="", row.names=1)
a <- ReadAffy(filenames=rownames(pData(pd)), phenoData=pd, verbose=TRUE)

# Normalise the data
x <- expresso(
  a,
  bgcorrect.method="rma",
  normalize.method="constant",
  pmcorrect.method="pmonly",
  summary.method="avgdiff")

# Remove control probes
controlProbeIdx <- grep("^AFFX", rownames(x))
x <- x[-controlProbeIdx,]

# Identify genes of significant effect
lm.coef <- function(y) lm(y ~ estrogen * time.h)$coefficients
eff <- esApply(x, 1, lm.coef)
effectUp <- names(sort(eff[2,], decreasing=TRUE)[1:25])
effectDown <- names(sort(eff[2,], decreasing=FALSE)[1:25])
main.effects <- c(effectUp, effectDown)

# Filter the expression set object to include only genes of significant effect
estrogenMainEffects <- exprs(x)[main.effects,]

head(estrogenMainEffects)



library(igraph)
library(ggplot2)

# Create a graph adjacency based on correlation distances between genes in  pairwise fashion.
g <- graph.adjacency(
  as.matrix(as.dist(cor(t(estrogenMainEffects), method="pearson"))),
  mode="undirected",
  weighted=TRUE,
  diag=FALSE
)


# Simplfy the adjacency object
g <- simplify(g, remove.multiple=TRUE, remove.loops=TRUE)

# Colour negative correlation edges as blue
E(g)[which(E(g)$weight<0)]$color <- "darkblue"

# Colour positive correlation edges as red
E(g)[which(E(g)$weight>0)]$color <- "darkred"

# Convert edge weights to absolute values
E(g)$weight <- abs(E(g)$weight)

# Change arrow size
# For directed graphs only
#E(g)$arrow.size <- 1.0

# Remove edges below absolute Pearson correlation 0.8
g <- delete_edges(g, E(g)[which(E(g)$weight<0.8)])

# Remove any vertices remaining that have no edges
g <- delete.vertices(g, degree(g)==0)

# Assign names to the graph vertices (optional)
V(g)$name <- V(g)$name

# Change shape of graph vertices
V(g)$shape <- "sphere"

# Change colour of graph vertices
V(g)$color <- "skyblue"

# Change colour of vertex frames
V(g)$vertex.frame.color <- "white"

# Scale the size of the vertices to be proportional to the level of expression of each gene represented by each vertex
# Multiply scaled vales by a factor of 10
scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
vSizes <- (scale01(apply(estrogenMainEffects, 1, mean)) + 1.0) * 10

# Amplify or decrease the width of the edges
edgeweights <- E(g)$weight * 2.0

# Convert the graph adjacency object into a minimum spanning tree based on Prim's algorithm
mst <- mst(g, algorithm="prim")

# Plot the tree object

png("plot_initial.png", units = "cm", width = 18, height = 11, res = 1000) 

plot(
  mst,
  layout=layout.fruchterman.reingold,
  edge.curved=TRUE,
  vertex.size=vSizes,
  vertex.label.dist=-0.5,
  vertex.label.color="black",
  asp=FALSE,
  vertex.label.cex=0.6,
  edge.width=edgeweights,
  edge.arrow.mode=0)

dev.off() 



mst.communities <- edge.betweenness.community(mst, weights=NULL, directed=FALSE)
mst.clustering <- make_clusters(mst, membership=mst.communities$membership)
V(mst)$color <- mst.communities$membership + 1

par(mfrow=c(1,2))

png("plot_second.png", units = "cm", width = 18, height = 11, res = 1000) 

plot(
  mst.clustering, mst,
  layout=layout.fruchterman.reingold,
  edge.curved=TRUE,
  vertex.size=vSizes,
  vertex.label.dist=-0.5,
  vertex.label.color="black",
  asp=FALSE,
  vertex.label.cex=0.6,
  edge.width=edgeweights,
  edge.arrow.mode=0)

dev.off()

svg("plot_third.svg") 

plot(
  mst,
  layout=layout.fruchterman.reingold,
  edge.curved=TRUE,
  vertex.size=vSizes,
  vertex.label.dist=-0.5,
  vertex.label.color="black",
  asp=FALSE,
  vertex.label.cex=0.6,
  edge.width=edgeweights,
  edge.arrow.mode=0)

dev.off()