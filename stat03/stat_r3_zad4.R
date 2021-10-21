#r3_zad4
library(dplyr)
data("iris")
iris %>%
  filter(Sepal.Length < 4.5) %>%
  distinct(Species) %>%
  count()

