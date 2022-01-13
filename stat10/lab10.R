#stat10
setwd("~/stat/stat10")
library(openxlsx)
library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
#zad1

data = read.xlsx("Appendix1.xlsx")
data
#rownames(data) = data$Species
data = data[,-1]
data
data_wide = t(data)
data_wide
data.dist = vegdist(data_wide, method = "euclidean")
data.dist

cluster.single = hclust(data.dist, method = "single")
plot(cluster.single,
     xlab = "complete method",
     ylab = "length",
     main = "Euclidean method",
     sub = "")

cluster.comp = hclust(data.dist, method = "complete")
plot(cluster.comp,
     xlab = "complete method",
     ylab = "length",
     main = "Euclidean method",
     sub = "")

cluster.average = hclust(data.dist, method = "average")
plot(cluster.average,
     xlab = "average method",
     ylab = "length",
     main = "Euclidean method",
     sub = "")

cluster.ward = hclust(data.dist, method = "ward")
plot(cluster.ward,
     xlab = "ward method",
     ylab = "length",
     main = "Euclidean method",
     sub = "")

#zad2

data.dist2 = vegdist(data_wide, method = "bray")
data.dist2

cluster.single2 = hclust(data.dist, 
                         method = "single")
plot(cluster.single2,
     xlab = "single method",
     ylab = "length",
     main = "Bray method",
     sub = "")

cluster.comp2 = hclust(data.dist, method = "complete")
plot(cluster.comp2,
     xlab = "complete method",
     ylab = "length",
     main = "Bray method",
     sub = "")

cluster.average2 = hclust(data.dist, method = "average")
plot(cluster.average2,
     xlab = "average method",
     ylab = "length",
     main = "Bray method",
     sub = "")

cluster.ward2 = hclust(data.dist, method = "ward")
plot(cluster.ward2,
     xlab = "ward method",
     ylab = "length",
     main = "Bray method",
     sub = "")

#zad3

kalusy = read.table("kalusy-PCA.txt", header = T)
kalusy

kalusy.dist = vegdist(kalusy, method = "euclidean")
kalusy.dist2 = vegdist(kalusy, method = "bray")

cluster.single = hclust(kalusy.dist, method = "single")
plot(cluster.single,
     xlab = "complete method",
     ylab = "length",
     main = "Euclidean method",
     sub = "")

cluster.comp = hclust(kalusy.dist, method = "complete")
plot(cluster.comp,
     xlab = "complete method",
     ylab = "length",
     main = "Euclidean method",
     sub = "")

cluster.average = hclust(kalusy.dist, method = "average")
plot(cluster.average,
     xlab = "average method",
     ylab = "length",
     main = "Euclidean method",
     sub = "")

cluster.ward = hclust(kalusy.dist, method = "ward")
plot(cluster.ward,
     xlab = "ward method",
     ylab = "length",
     main = "Euclidean method",
     sub = "")

cluster.single2 = hclust(kalusy.dist2, 
                         method = "single")
plot(cluster.single2,
     xlab = "single method",
     ylab = "length",
     main = "Bray method",
     sub = "")

cluster.comp2 = hclust(kalusy.dist2, method = "complete")
plot(cluster.comp2,
     xlab = "complete method",
     ylab = "length",
     main = "Bray method",
     sub = "")

cluster.average2 = hclust(kalusy.dist2, method = "average")
plot(cluster.average2,
     xlab = "average method",
     ylab = "length",
     main = "Bray method",
     sub = "")

cluster.ward2 = hclust(kalusy.dist2, method = "ward")
plot(cluster.ward2,
     xlab = "ward method",
     ylab = "length",
     main = "Bray method",
     sub = "")

