# zad1
plon1 = c(290, 286, 266, 270, 301, 270, 264, 277)
plon2 = c(445, 450, 413, 448, 454, 442, 430, 438)
plon3 = c(520, 470, 516, 530, 475, 508, 485, 480)
plon4 = c(370, 405, 412, 403, 384, 410, 415, 377)
plon = c(plon1, plon2, plon3, plon4)
powtorzenia = c(rep(c(1, 2, 3, 4, 5, 6, 7, 8),4))
terminy = c(rep(1,8), rep(2,8), rep(3,8), rep(4,8))

data = data.frame(plon, terminy, powtorzenia)
data

# test normalnosci
shapiro.test(plon1)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
shapiro.test(plon2)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
shapiro.test(plon3)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
shapiro.test(plon4)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

#test rownosci wariancji
bartlett.test(data$plon, data$terminy)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione

# analiza wariancji ANOVA
# uklad hipotez
# H0: plon zielonki w roznych terminach nie rozni sie istotnie 
# H1: ~H0
model = aov(data$plon ~ factor(data$terminy))
summary(model)

# pvslue mniejssze od 0.05, zatem odrzucamy hipoteze zerowa i przyjmujemy alternatywna, ktora wskazuje na to, ze termin ma istotny wplyw na zielonke
TukeyHSD((model))
plot(TukeyHSD(model))

# zad2
K <- c(6.8, 4.6, 5.6, 6.1, 4.9)
M <- c(5.4, 6.8, 6.1, 6.0, 5.7)
P <- c(5.3, 5.3, 6.5, 6.3, 7.2)

KMP <- c(K, M, P)
dawki <- c(rep(c(1,2,3,4,5),3))
odmiany <- c(rep(1,5), rep(2,5), rep(3,5))

data2 <- data.frame(KMP, odmiany, dawki)

data2

shapiro.test(K)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
shapiro.test(M)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
shapiro.test(P)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

bartlett.test(data2$KMP, data2$odmiany)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione

# analiza wariancji ANOVA
# uklad hipotez
# H0_odmiany: srednie plony jeczmienia dla odmian sa rowne
# H1_odmiany: ~H0_odmiany
# H0_dawki: srednie plony jeczmienia dla roznych dawek nawozu sa rowne
# H1_dawki: ~H0_dawki
model = aov(data2$KMP ~ factor(data2$odmiany) + factor(data2$dawki))
summary(model)

# pvalue wieksze od 0.05 w obu przypadkach, zatem przyjmujemy hipoteze zerowa ktora wskazuje na to, srednie plony dla odmian i dawek sa rozne

TukeyHSD((model))
plot(TukeyHSD(model))

# zad3 
#library(reshape2)
library(tidyr)
library("xlsx")

xdata = read.xlsx("zad3_2.xlsx", 1, header = T)
colnames(xdata) = c("Siew", 0, 0, 40, 40, 80, 80, 120, 120)
xdata %>%
  group_by(colnames(xdata))
xdata2 = pivot_longer(xdata, cols = 2:5, names_to = "nawozenia", values_to = "plony")
xdata2

plon1 <- c(33.2, 44.2, 18.6, 14.6, 20.4, 11.0, 36.2, 51.0, 13.0, 18.8, 14.4, 22.6)
plon2 <- c(44.2, 50.6, 18.0, 14.2, 21.9, 16.2, 41.4, 45.2, 20.0, 19.1, 24.0, 25.6)
plon3 <- c(50.2, 52.6, 24.2, 16.4, 18.2, 27.3, 53.0, 45.0, 21.6, 19.0, 21.0, 27.6)
plon4 <- c(46.2, 49.0, 34.2, 15.5, 16.4, 21.6, 52.4, 43.6, 17.2, 22.2, 15.0, 27.8)

plon <- c(plon1, plon2, plon3, plon4)
dawki <- c(rep(c(0,40,80,120),12))
odmiany <- c(rep(c(1,1),8), rep(c(2,2),8), rep(c(3,3),8))

data3 <- data.frame(plon, odmiany, dawki)

data3

summary(aov(xdata2$plony ~ factor(xdata2$nawozenia)*factor(xdata2$Siew)))
