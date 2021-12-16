library("FactoMineR")
library("factoextra")
setwd("~/stat/stat09/")
library(xlsx)
library(tidyr)
library(dplyr)

#zad1

data = read.xlsx("dziesiecioboj-1.xlsx", 1, header = TRUE)
data
row.names(data) = data$Athlets
data = data[2:ncol(data)]
typeof(data)
data.pca <- PCA(data, graph = FALSE)
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, repel = TRUE)

#cos2-var
fviz_pca_var(data.pca, repel = TRUE, col.var = "cos2", gradient.cols = c("red", "orange", "green"))
fviz_cos2(data.pca, choice = "var")

#contrib-var
fviz_contrib(data.pca, choice = "var")
fviz_pca_var(data.pca, repel = TRUE, col.var = "contrib", gradient.cols = c("red", "orange", "green"))

fviz_pca_ind(data.pca, repel = TRUE)

#cos2-ind
fviz_cos2(data.pca, choice = "ind")
fviz_pca_ind(data.pca, repel = TRUE, col.ind = "cos2", gradient.cols = c("red", "orange", "green"))

#contrib-ind
fviz_contrib(data.pca, choice = "ind")
fviz_pca_ind(data.pca, repel = TRUE, col.ind = "contrib", gradient.cols = c("red", "orange", "green"))

fviz_pca_biplot(data.pca, repel = T)


#zad2

# custom grouping

data2 = read.table("eucarpia2.txt", header = TRUE)
data3 = as.data.frame.matrix(data2)
data3
data2$kalusy = as.numeric(as.character(data2$kalusy))
typeof(data2)
#data3 = data3[2:4]
data3
#row.names(data2) = data2$pozywka

d = data3[2:4] %>%
  group_by(pozywka, genotyp) %>%
  pivot_wider(names_from = genotyp, values_from=kalusy)%>%
  unique()  #summary(across(DC:CL, ~ lapply(.x, mean)))

d  
#names = as.character(d$pozywka)
d = as.data.frame(row.names = as.character(d$pozywka), lapply(d[2:ncol(d)], function(x) as.numeric(unlist(lapply(x, mean)))))
#row.names(d) = names
d

# MC test
#d.pca = PCA(d, graph = F)
#factoextra::fviz_pca_biplot(d.pca, col.var = "cos2", gradient.cols = hcl.colors(2, "Warm"), addEllipses = T, repel = T)


data2 = read.table("kalusy-PCA.txt", header = T)
data2
data2.pca <- PCA(data2, graph = FALSE)
fviz_eig(data2.pca, addlabels = TRUE)
fviz_pca_var(data2.pca, repel = TRUE)

#cos2-var
fviz_pca_var(data2.pca, repel = TRUE, col.var = "cos2", gradient.cols = c("red", "orange", "green"))
fviz_cos2(data2.pca, choice = "var")

#contrib-var
fviz_contrib(data2.pca, choice = "var")
fviz_pca_var(data2.pca, repel = TRUE, col.var = "contrib", gradient.cols = c("red", "orange", "green"))

fviz_pca_ind(data2.pca, repel = TRUE)

#cos2-ind
fviz_cos2(data2.pca, choice = "ind")
fviz_pca_ind(data2.pca, repel = TRUE, col.ind = "cos2", gradient.cols = c("red", "orange", "green"))

#contrib-ind
fviz_contrib(data2.pca, choice = "ind")
fviz_pca_ind(data2.pca, repel = TRUE, col.ind = "contrib", gradient.cols = c("red", "orange", "green"))

fviz_pca_biplot(data2.pca)

#zad3
data3.pca <- PCA(iris[-5], graph = FALSE)
fviz_eig(data3.pca, addlabels = TRUE)
fviz_pca_var(data3.pca, repel = TRUE)

#cos2-var
fviz_pca_var(data3.pca, repel = TRUE, col.var = "cos2", gradient.cols = c("red", "orange", "green"))
fviz_cos2(data3.pca, choice = "var")

#contrib-var
fviz_contrib(data3.pca, choice = "var")
fviz_pca_var(data3.pca, repel = TRUE, col.var = "contrib", gradient.cols = c("red", "orange", "green"))

fviz_pca_ind(data3.pca, repel = TRUE, col.ind = iris$Species, geom.ind = "point")

#cos2-ind
fviz_cos2(data3.pca, choice = "ind")
fviz_pca_ind(data3.pca, repel = TRUE, col.ind = "cos2", gradient.cols = c("red", "orange", "green"), geom.ind = "point")

#contrib-ind
fviz_contrib(data3.pca, choice = "ind")
fviz_pca_ind(data3.pca, repel = TRUE, col.ind = "contrib", gradient.cols = c("red", "orange", "green"), geom.ind = "point")

fviz_pca_biplot(data3.pca, col.ind = iris$Species, repel = T, label="var", legend.title = "Species")

                