#R3-wina
library(dplyr)
setwd("~/stat/stat03")
vines = read.csv("wina-recenzje.csv", sep=",")

#z1
# cala informacja o ramce
str(vines)
# albo
vines %>%
  count() # obserwacje
  ncol(vines) # zmienne
  
lapply(vines, class) # typy

#z2
vines %>%
  filter(points >=94, price < 25)

#z3
vines %>%
  sample_frac(0.01)

#z4
vines %>%
  arrange(desc(points)) %>%
  slice_head(n=3)

#z5
vines %>%
  arrange(price) %>%
  slice_head(n=100)

#z6
vines %>%
  arrange(desc(points)) %>%
  select(title) %>%
  list(vines$title)

#z7
vines %>%
  select(country, province:region_2)

#z8 
vines %>%
  rename(score = points) #nieintuicyjne

#z9
vines %>%
  mutate(price_pln = price * 3.94)

#z10
vines %>%
  summarise(price_mean = mean(price, na.rm = T))
quantile(vines$price, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = T)

#z11  
median(vines$price, na.rm = T)

#z12
vines %>%
  mutate(proportion = points / price) %>%
  arrange(desc(proportion)) %>%
  slice_max(proportion)

#z13
vines %>%
  filter(points >= 90)

#z14  
vines %>%
  group_by(country) %>%
  summarize(country_price = mean(price, na.rm = T)) %>%
  slice_min(country_price)
  
#z15
vines %>%
  group_by(country) %>%
  mutate(country_price = mean(price, na.rm = T), num_country = n()) %>%
  select(country_price, num_country) %>%
  distinct(across())

