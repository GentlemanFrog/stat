#r2-zad6
png("Titanic.png")
library(dplyr)
library(ggplot2)
library(effects)
#summary(TitanicSurvival$passengerClass)
TitanicSurvival
T_survived = TitanicSurvival %>% 
  group_by(passengerClass) %>%
  summarise(count = sum(survived == "yes"))
T_survived
barplot(T_survived$count, 
        names.arg = T_survived$passengerClass, 
        main = "Liczba przeżytych pasażerów we wszystkich klasach")
dev.off()
# Problem z poleceniem - dlatego brak legendy:
# skoro mamy pokazać przeżytych pasażerów, to dlaczego mam dać
# legendę o tym, skoro tylko przeżyci mieli tutaj być?
# na siłe przypadek, gdy bierzemy wszystkie klasy i dajemy legende
# do ich przeżywalności
dev.new()
png("Titanic2.png")
barplot(summary(TitanicSurvival$passengerClass), col = "orange")
barplot(T_survived$count, col = "green", add = TRUE)
legend("topright", c("survived", "not survived"), fill = c("green", "orange"))
dev.off()

