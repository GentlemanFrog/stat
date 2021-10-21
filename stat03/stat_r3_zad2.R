#r3_zad2
library(dplyr)
data("iris")
iris %>%
  filter(Petal.Width > 2) %>%
  count(.)
