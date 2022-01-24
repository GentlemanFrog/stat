#lab11
library(mvnormtest)
library(dplyr)
library(rstatix)
setwd("~/stat/stat11")
# zad1
iris
rstatix::mshapiro_test(iris %>% select(Sepal.Length, Sepal.Width))
# pvalue wieksze od 0.05 zatem zmienne zalezne maja rozklad normalny
bartlett.test(iris %>% select(Sepal.Length, Sepal.Width))
# pvalue jest mniejsze od 0.05, zatem brak jest jednorodnosci wariancji
cor.test(iris$Sepal.Length, iris$Sepal.Width)
# pvalue testu pearsona dla korelacji jest wieksze od 0.05, zatem korelacje jest nieistotna statystycznie i korelacja jest ujemna
model = manova(cbind(iris$Sepal.Length, iris$Sepal.Width) ~ iris$Species)
summary(model, test = "Pillai")
tests = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
summary(model, test = "Wilks")
summary(model, test = "Hotelling-Lawley")
summary(model, test = "Roy")

#przed kazdym modelem nalezy testowac rozklad, jednorodnosc wariancji i liniowosc

model2 = manova(cbind(iris$Petal.Length, iris$Petal.Width) ~ iris$Species)
summary(model2, test = "Pillai")
tests = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
summary(model2, test = "Wilks")
summary(model2, test = "Hotelling-Lawley")
summary(model2, test = "Roy")
model3 = manova(cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width) ~ iris$Species)
summary(model3, test = "Pillai")
tests = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
summary(model3, test = "Wilks")
summary(model3, test = "Hotelling-Lawley")
summary(model3, test = "Roy")

#zad2
library(openxlsx)
metals = read.xlsx("metals.xlsx", sheet=1)
metals

# tutaj trzeba sprawdzic zalozenia

model_metals = manova(cbind(C1, C2, C3, C4) ~ extractants + metals, data = metals)
summary(model_metals, test = "Pillai")
summary(model_metals, test = "Wilks")
summary(model_metals, test = "Hotelling-Lawley")
summary(model_metals, test = "Roy")



#drzewka-zadania
#zad1

library(openxlsx)
jaja <- read.xlsx("jaja-tree.xlsx", sheet = 1)

library(rpart)
library(rpart.plot)
model_tree = rpart(zaplodnienie ~ FA + HA + HT + ST + EST, data = jaja) 
model_tree2 = rpart(niewyklute ~ FA + HA + HT + ST + EST, data = jaja) 
summary(model_tree)
rpart.plot(model_tree, box.palette = "RdYlGn") # plot drzewa
summary(model_tree2)
rpart.plot(model_tree2, box.palette = "RdYlGn")
rpart.rules(model_tree2) # plot zasad do decyzji

