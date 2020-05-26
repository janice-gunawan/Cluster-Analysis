setwd("/Users/janicegunawan/Desktop/Marketing Analytics")
DT = read.csv("Mall_Visits.csv")
View(DT)
head(DT,5)
dim(DT)
summary(DT)
summary(DT$MallVisits)
MeanFun = mean(DT$Fun)
SDFun = sd(DT$Fun)
ScaFun = (DT$Fun - MeanFun) / SDFun 
ScaFun
mean(ScaFun)
sd(ScaFun)
CombineView = cbind(DT$Fun, ScaFun)
CombineView
DT$ScaFun = scale(DT$Fun)
DT$ScaFun
ScaDT = apply(DT[, 2:9], 2, function(x) scale(x))
ScaDT
summary(ScaDT)
EuclideanD <- dist(ScaDT[1:10, 1:6])
EuclideanD = as.matrix(EuclideanD)
View(EuclideanD)
View(round(EuclideanD, 1))
EuclideanD = dist(ScaDT[, 1:6])
Hierarchical_Cluster = hclust(EuclideanD)

plot(Hierarchical_Cluster)
png(file="Dendrogram.png",
    width = 700, height = 500)
plot(Hierarchical_Cluster)
dev.off()

heatmap(as.matrix(ScaDT[, 1:6]))
png(file="HeatMap_ClusterAnalysis.png",
    width = 700, height = 800)
heatmap(as.matrix(ScaDT[, 1:6]))
dev.off()

HC_membership = as.vector(cutree(Hierarchical_Cluster, k = 3))
DT_HC = cbind(DT, HC_membership)
head(DT_HC)
#each group segment size
prop.table(table(DT_HC$HC_membership)) 
#average of all the variables by groups
Mean_by_Group <- function(data, groups) 
  { aggregate(data, list(groups),function(x) mean(as.numeric(x)))} 
Group_Character_HC = Mean_by_Group(DT_HC, DT_HC$HC_membership)

set.seed(888)
kmeans_Cluster <- kmeans(ScaDT[, 1:6],centers = 3, iter.max=2000)
Kmeans_membership = as.vector(kmeans_Cluster$cluster)
DT_Kmeans = cbind(DT, Kmeans_membership)
head(DT_Kmeans)

prop.table(table(DT_Kmeans$Kmeans_membership))
Group_Character_Kmeans = Mean_by_Group(DT_Kmeans, DT_Kmeans$Kmeans_membership)
View(Group_Character_Kmeans)
