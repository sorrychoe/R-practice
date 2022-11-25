#data loading
protein <- read.table("~/protein.txt",  sep="\t", header=TRUE)
summary(protein)

#data preparation
vars.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

#hierarchial clustering(Ward's procedure)
d <- dist(pmatrix, method= "euclidean")
pfit <- hclust(d, method="ward.D")
plot(pfit, labels=protein$Country)         

#clustering based on some data
groups <- cutree(pfit, k=5)
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}
print_clusters(groups, 5)

#k-means clustering
pclusters <- kmeans(pmatrix, 5, nstart=100, iter.max=100)
summary(pclusters)

pclusters$centers
pclusters$cluster
pclusters$size
pclusters$withinss
pclusters$betweenss
pclusters$totss

#visualization
library(factoextra)
fviz_cluster(pclusters, pmatrix, stand = T)

##¿¬½À ¹®Á¦
#WSS
WSS <- sum(pclusters$withinss)
WSS

#CH index
CH_index <- (pclusters$betweenss/4) / (WSS/20)
CH_index
