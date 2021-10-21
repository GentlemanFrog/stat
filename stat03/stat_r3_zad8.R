#r3_zad8
library(dplyr)
library(ggplot2)
iris %>%
  group_by(Species) %>%
  mutate(Avg_dim = mean(Petal.Width) * mean(Petal.Length)) %>%
  filter(Avg_dim != 13 | Species == "virginica")

  
