# linki

################## 1 plot.hclust(): R base function

# Load data
data(USArrests)
# Compute distances and hierarchical clustering
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

# x: an object of the type produced by hclust()
# labels: A character vector of labels for the leaves of the tree. The default value is row names. if labels = FALSE, no labels are drawn.
# hang: The fraction of the plot height by which labels should hang below the rest of the plot. A negative value will cause the labels to hang down from 0.
# main, sub, xlab, ylab: character strings for title.
plot(hc, labels = NULL, hang = 0.1, 
     main = "Cluster dendrogram", sub = NULL,
     xlab = NULL, ylab = "Height")

# Put the labels at the same height: hang = -1
plot(hc, hang = -1, cex = 0.6)

############ 2.plot.dendrogram() function

# x: an object of class dendrogram
# type of plot. Possible values are “rectangle” or “triangle”
# horiz: logical indicating if the dendrogram should be drawn horizontally or no
# Convert hclust into a dendrogram and plot
hcd <- as.dendrogram(hc)
# Default plot
plot(hcd, type = "rectangle", ylab = "Height")
# Triangle plot
plot(hcd, type = "triangle", ylab = "Height")

# Zoom in to the first dendrogram
plot(hcd, xlim = c(1, 20), ylim = c(1,8))

# nodePar: a list of plotting parameters to use for the nodes (see ?points). Default value is NULL. The list may contain components named pch, cex, col, xpd, and/or bg each of which can have length two for specifying separate attributes for inner nodes and leaves.
# edgePar: a list of plotting parameters to use for the edge segments (see ?segments). The list may contain components named col, lty and lwd (for the segments). As with nodePar, each can have length two for differentiating leaves and inner nodes.
# leaflab: a string specifying how leaves are labeled. The default “perpendicular” write text vertically; “textlike” writes text horizontally (in a rectangle), and “none” suppresses leaf labels.
# Define nodePar
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")

# Horizontal plot
plot(hcd,  xlab = "Height",
     nodePar = nodePar, horiz = TRUE)

# Change edge color
plot(hcd,  xlab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))

######### 3. PHYLO TREES

install.packages("ape")
library("ape")
# Default plot
plot(as.phylo(hc), cex = 0.6, label.offset = 0.5)

# Cladogram
plot(as.phylo(hc), type = "cladogram", cex = 0.6, 
     label.offset = 0.5)

# Unrooted
plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)

# Fan
plot(as.phylo(hc), type = "fan")

# Radial
plot(as.phylo(hc), type = "radial")

# Cut the dendrogram into 4 clusters
colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)

# Change the appearance
# change edge and label (tip)
plot(as.phylo(hc), type = "cladogram", cex = 0.6,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2,
     tip.color = "steelblue")

######### 4. ggdendro package : ggplot2 and dendrogram
install.packages("ggdendro")
library("ggplot2")
library("ggdendro")
# Visualization using the default theme named theme_dendro()
ggdendrogram(hc)
# Rotate the plot and remove default theme
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE)

# Build dendrogram object from hclust results
dend <- as.dendrogram(hc)
# Extract the data (for rectangular lines)
# Type can be "rectangle" or "triangle"
dend_data <- dendro_data(dend, type = "rectangle")
# What contains dend_data
names(dend_data)

# Extract data for line segments
head(dend_data$segments)

# Extract data for labels
head(dend_data$labels)

# Plot line segments and add labels
p <- ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label),
            hjust = 1, angle = 90, size = 3)+
  ylim(-3, 15)
print(p)

########## 5 dendextend package: Extending R’s dendrogram functionality
data <- scale(USArrests)
dist.res <- dist(data)
hc <- hclust(dist.res, method = "ward.D2")
dend <- as.dendrogram(hc)
plot(dend)

dend <- USArrests[1:5,] %>% # data
  scale %>% # Scale the data
  dist %>% # calculate a distance matrix, 
  hclust(method = "ward.D2") %>% # Hierarchical clustering 
  as.dendrogram # Turn the object into a dendrogram.
plot(dend)

install.packages('dendextend')
library(dendextend)

# Create a dendrogram and plot it
dend <- USArrests[1:5,] %>%  scale %>% 
  dist %>% hclust %>% as.dendrogram
dend %>% plot

# Get the labels of the tree
labels(dend)

# Change the labels, and then plot:
dend %>% set("labels", c("a", "b", "c", "d", "e")) %>% plot

# Change color and size for labels
dend %>% set("labels_col", c("green", "blue")) %>% # change color
  set("labels_cex", 2) %>% # Change size
  plot(main = "Change the color \nand size") # plot

# Color labels by specifying the number of cluster (k)
dend %>% set("labels_col", value = c("green", "blue"), k=2) %>% 
  plot(main = "Color labels \nper cluster")
abline(h = 2, lty = 2)

# Change the type, the color and the size of node points
# +++++++++++++++++++++++++++++
dend %>% set("nodes_pch", 19) %>%  # node point type
  set("nodes_cex", 2) %>%  # node point size
  set("nodes_col", "blue") %>% # node point color
  plot(main = "Node points")

# Change the type, the color and the size of leave points
# +++++++++++++++++++++++++++++
dend %>% set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 2) %>%  # node point size
  set("leaves_col", "blue") %>% # node point color
  plot(main = "Leaves points")

# Specify different point types and colors for each leave
dend %>% set("leaves_pch", c(17, 18, 19)) %>%  # node point type
  set("leaves_cex", 2) %>%  # node point size
  set("leaves_col", c("blue", "red", "green")) %>% #node point color
  plot(main = "Leaves points")

# Default colors
dend %>% set("branches_k_color", k = 2) %>% 
  plot(main = "Default colors")
# Customized colors
dend %>% set("branches_k_color", 
             value = c("red", "blue"), k = 2) %>% 
  plot(main = "Customized colors")

# Vertical plot
dend %>% set("branches_k_color", k = 3) %>% plot
dend %>% rect.dendrogram(k=3, border = 8, lty = 5, lwd = 2)
# Horizontal plot
dend %>% set("branches_k_color", k = 3) %>% plot(horiz = TRUE)
dend %>% rect.dendrogram(k = 3, horiz = TRUE, border = 8, lty = 5, lwd = 2)

grp <- c(1,1,1, 2,2)
k_3 <- cutree(dend,k = 3, order_clusters_as_data = FALSE) 
# The FALSE above makes sure we get the clusters in the order of the
# dendrogram, and not in that of the original data. It is like:
# cutree(dend, k = 3)[order.dendrogram(dend)]
the_bars <- cbind(grp, k_3)
dend %>% set("labels", "") %>% plot
colored_bars(colors = the_bars, dend = dend)

dend <- iris[1:30,-5] %>% scale %>% dist %>% 
  hclust %>% as.dendrogram %>%
  set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
# plot the dend in usual "base" plotting engine:
plot(dend)


library(ggplot2)
# Rectangle dendrogram using ggplot2
ggd1 <- as.ggdend(dend)
ggplot(ggd1)


# Change the theme to the default ggplot2 theme
ggplot(ggd1, horiz = TRUE, theme = NULL) 

# Theme minimal
ggplot(ggd1, theme = theme_minimal()) 

# Create a radial plot and remove labels
ggplot(ggd1, labels = FALSE) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

install.packages("pvclust")
library(pvclust)
data(lung) # 916 genes for 73 subjects
set.seed(1234)
result <- pvclust(lung[1:100, 1:10], method.dist="cor", 
                  method.hclust="average", nboot=10)


# Default plot of the result
plot(result)
pvrect(result)

# pvclust and dendextend
result %>% as.dendrogram %>% 
  set("branches_k_color", k = 2, value = c("purple", "orange")) %>%
  plot
result %>% text
result %>% pvrect
