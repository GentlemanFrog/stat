#r3_zad6
library(dplyr)
library(ggplot2)
data("iris")
#virg = filter(iris, Species == "virginica")
ggplot(filter(iris, Species == "virginica"), aes(Sepal.Length, Sepal.Width)) + geom_point()
