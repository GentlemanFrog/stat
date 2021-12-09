library(tidyr)
library(dplyr)
library("xlsx")
#zad1
data = c(-10:10)
data
curve(exp(x)/(1+exp(x)), -10, 10)

#zad2
xdata = read.xlsx("nadcisnienie.xlsx", 1, header = T)
rl = glm(xdata$nadcisnienie ~ xdata$wiek + xdata$palenie, family = "binomial")
summary(rl)
# 
# dla obu grup pvalue jest wieksze od 0.05, zatem brak istotnego wplywu wieku oraz palenia na poziom nadcisnienia

#zad3
#A
mtcars
model.lm = lm(mtcars$mpg ~ mtcars$disp + mtcars$hp)
summary(model.lm)
model.gauss = glm(mtcars$mpg ~ mtcars$disp + mtcars$hp, family = "gaussian")
summary(model.gauss)
# oba modele daja te same dopasowania, zatem dla disp jest obecny istotny wplyw na mpg, a dla hp nie istnieje istotny wplyw (pvalue powyzej 0.05)

model.logit = glm(mtcars$am ~ mtcars$disp + mtcars$hp, family = "binomial")
summary(model.logit)
model.poiss = glm(mtcars$am ~ mtcars$disp + mtcars$hp, family = "poisson")
summary(model.poiss)
# oba modele daja rozne dopasowania:
# - lepiej dopasowany jest model logistyczny, z AIC = 22 (model poissona ma AIC = 42)
# - dla modelu logistycznego disp ma istotny wplyw na am, a hp nie wykazuje istotnego wplywu
# - dla modelu poissona disp oraz hp maja istotny wplyw na am

# ZAD4
tdata = read.table("days-students.txt", header = TRUE)[2:3]
plot(tdata)
model2.lm = glm(tdata$Students ~ tdata$Days, family = "gaussian")
summary(model2.lm)
model2.logit = glm(tdata$Students ~ tdata$Days, family = "binomial")
summary(model2.logit)
model2.poiss = glm(tdata$Students ~ tdata$Days, family = "poisson")
summary(model2.poiss)
# najlepszym modelem jest model poissona, poniewaz wykazuje najnizsza wartosc AIC = 393

#zad5
library(datasets)
PlantGrowth
#A
plot(PlantGrowth)

#B
plant = PlantGrowth
weight.factors = cut(PlantGrowth$weight, 2, labels=c(0, 1))
plant$weight2 = weight.factors
plant
model = glm(plant$weight2 ~ plant$group, family = "binomial")
summary(model)

#zad6
data = data.frame("ck" = c(20, 60, 100, 140, 180, 220, 260, 300, 340, 380, 420, 460),
                  "ha" = c(2,13,30,30,21,19,18,13,19,15,7,8),
                  "ok" = c(88,26,8,5,0,1,1,1,1,0,0,0))
#A
plot(data$ck, data$ha/(data$ha + data$ok), ylab = "Prob o heart attack")
#B                                                                                                                                                                                                               
model.logit = glm(cbind(data$ha, data$ok) ~ data$ck, family = "binomial")
#C
summary(model.logit)
# AIC 62.334
#D
model.logitsq = glm(cbind(data$ha, data$ok) ~ data$ck + I(data$ck^2), family = "binomial")
#E
summary(model.logitsq)
# AIC 42.815
#F
#lepszym modelem jest model kwadratowej regresji logistycznej z rozkladem dwumianowym (42.815<62.334)
#G
dev.off()
plot(data$ck, data$ha / (data$ha + data$ok), ylab = "Prob o heart attack")
lines(data$ck, model.logit$fitted.values, col = "cyan")
lines(data$ck, model.logitsq$fitted.values, lty= 69, col = "magenta")
legend("bottomright", c("Binomial", "Binomial Squared"), col=c("cyan", "magenta"), lty=c(1, 69))

