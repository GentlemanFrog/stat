#r2-zad7
data(iris)
iris

library(tidyr)
library(dplyr)
library(reshape2)
library(reshape)
data(iris)

dev.new()
png("iris.png")
iris
df = iris %>% # load
  select(Species, Sepal.Length) %>% # select those 2 cols
  group_by(Species) %>% # group
  mutate(row = row_number()) %>% # identify values (just to be able to distinct)
  pivot_wider(names_from = Species, values_from = Sepal.Length) %>% # (rearrange)
  select(-row) # ignore ids
df
#boxplot(df)
# albo lepiej
boxplot(iris$Sepal.Length ~ iris$Species)
dev.off()

