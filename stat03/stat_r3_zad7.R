#r3_zad7
library(dplyr)
library(ggplot2)
iris %>%
  group_by(Species) %>%
  count()

