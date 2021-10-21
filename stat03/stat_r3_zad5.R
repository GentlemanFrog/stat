#r3_zad5
library(dplyr)
data("iris")
new_iris = mutate(iris, Petal_dim = iris$Petal.Length * iris$Petal.Width)
new_iris
