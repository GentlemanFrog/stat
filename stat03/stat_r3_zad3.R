#r3_zad3
library(dplyr)
data("iris")
iris %>%
  filter(Sepal.Width < 4, Species != "setosa") %>%
  arrange(Petal.Length)
