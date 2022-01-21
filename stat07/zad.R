setwd("~/stat/stat07")
library(tidyr)
library(dplyr)
library("xlsx")

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

# dla odmian
shapiro.test(K)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
shapiro.test(M)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
shapiro.test(P)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

# dla dawek nawozenia
p1 = data2 %>%
  filter(dawki == 1)
p1
shapiro.test(p1$KMP)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
p2 = data2 %>%
  filter(dawki == 2)
p2
shapiro.test(p2$KMP)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
p3 = data2 %>%
  filter(dawki == 3)
p3
shapiro.test(p3$KMP)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
p4 = data2 %>%
  filter(dawki == 4)
p4
shapiro.test(p4$KMP)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
p5 = data2 %>%
  filter(dawki == 5)
p5
shapiro.test(p5$KMP)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym


bartlett.test(data2$KMP, data2$odmiany)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione
bartlett.test(data2$KMP, data2$dawki)
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
library(dplyr)
library("xlsx")

xdata = read.xlsx("zad3_2.xlsx", 1, header = T)
xdata
colnames(xdata) = c("Siew", 0, 0, 40, 40, 80, 80, 120, 120)
xdata
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

p1 = xdata2 %>%
      filter(nawozenia == 0)
shapiro.test(p1$plony)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

p2 = xdata2 %>%
      filter(nawozenia == 40)
shapiro.test(p2$plony)
# pvalue ponizej 0.05, zatem jest podstawa do odrzucenia hipotezy zerowej i przyjecia alternatywnej, zatem uklad nie jest zgodny z rozkladem normalnym

p3 = xdata2 %>%
  filter(nawozenia == 80)
shapiro.test(p3$plony)
# pvalue ponizej 0.05, zatem jest podstawa do odrzucenia hipotezy zerowej i przyjecia alternatywnej, zatem uklad nie jest zgodny z rozkladem normalnym

p4 = xdata2 %>%
  filter(nawozenia == 120)
shapiro.test(p4$plony)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
p5 = xdata2 %>%
  filter(Siew == "C")
p5
shapiro.test(p5$plony)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym
p6 = xdata2 %>%
  filter(Siew == "M")
p6
shapiro.test(p6$plony)
# pvalue mniejsze od 0.05, zatem jest podstawa do odrzucenia hipotezy zerowej, zatem uklad nie jest zgodny z rozkladem normalnym
p7 = xdata2 %>%
  filter(Siew == "P")
p7
shapiro.test(p7$plony)
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

bartlett.test(xdata2$plony, xdata2$nawozenia)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione
bartlett.test(xdata2$plony, xdata2$Siew)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione

# analiza wariancji ANOVA
# uklad hipotez
# H0_nawozenie: nawożenie nie wpływa na średnie plony masy zielonej.
# H1_nawozenie: ~H0_nawozenie
# H0_siew: sposoby siewu nie wpływają na średnie plony masy zielonej
# H1_siew: ~H0_siew
# H0_NxS: interakcja nawożeń ze sposobami siewu jest nieistotna
# H1_NxS: ~H0_NxS

model = aov(xdata2$plony ~ factor(xdata2$nawozenia)*factor(xdata2$Siew))
summary(model)
# pvalue mnniejsze od 0.05 w przypadku nawozen oraz siewu, zatem odrzucamy hipoteze zerowa i przyjmujemy alternatywna, zatem nawozenie i siew wplywaja istotnie statystyczniee na srednie plony masy zielonej
# pvalue wieksze od 0.05 w przypadku interakcji, zatem przyjmujemy hipoteze zerowa, zatem inteerakcja nawozenia i siewu nie wplywa istotnie statystyczniee na srednie plony masy zielonej

t = TukeyHSD(model)
t$`factor(xdata2$nawozenia)`
t$`factor(xdata2$Siew)`
t$`factor(xdata2$nawozenia):factor(xdata2$Siew)`

# zad4
xdata = read.xlsx2("zad4.xlsx", 1, header = T)
xdata
colnames(xdata) = c("Bloki", 1, 2, 3, 4)
xdata2 = xdata %>%
  slice(1:5)
xdata2
xdata3 = pivot_longer(xdata2, cols = 2:5, names_to = "Typy", values_to = "plon")
xdata3

p1 = xdata3 %>%
  filter(Typy == 1)
shapiro.test(as.numeric(p1$plon))
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

p2 = xdata3 %>%
  filter(Typy == 2)
shapiro.test(as.numeric(p2$plon))
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

p3 = xdata3 %>%
  filter(Typy == 3)
shapiro.test(as.numeric(p3$plon))
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

p4 = xdata3 %>%
  filter(Typy == 4)
shapiro.test(as.numeric(p4$plon))
# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym

bartlett.test(as.numeric(xdata2$plon), xdata2$Typy)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione

########
# w teorii mamy sprawdzac tylko obiekty czyli typy uprawy, zatem bartlett plonów z blokami teoretycznie nie jest potrzebny 
bartlett.test(as.numeric(xdata2$plon), xdata2$Bloki)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione
########


# analiza wariancji ANOVA
#Układ hipotez:
#H0: średnie plonow dla typów uprawy sa rowne
#H1: ~H0

model = aov(as.numeric(xdata3$plon) ~ factor(xdata3$Typy)+factor(xdata3$Bloki))
summary(model)

# pvalue mniejsze od 0.05 dla typu uprawy, zatem przyjmujemy hipoteze alternatywna ktora wskazuje na to, ze srednie plony nie sa rowne dla roznych typow uprawy

TukeyHSD((model))
plot(TukeyHSD(model))

# pod wzgledem blokow - sa one wszystkie statystycznie znaczaco nierowne
# pod wzgledem typow uprawy - 2-1 oraz 3-1 są statystycznie znaczaco rozne

#zad5
xdata = read.xlsx("zad5.xlsx", 1, header = T)
xdata

xdata2 = pivot_longer(xdata, cols = 2:6, names_to = "Terminy", values_to = "temp")
xdata2

xdata3 = xdata2 %>%
  separate(temp, c("Herbicydy", "Plony"), sep = ": ")
xdata3$Plony = as.numeric(sub(",", ".", xdata3$Plony))
xdata3

map = hash()
map$Odmiany = unique(xdata3$Odmiany)
map$Terminy = unique(xdata3$Terminy)
map$Herbicydy = unique(xdata3$Herbicydy)
map

for (col in colnames(xdata3)[1:3])
{
  print(col)
  for (item in map[[col]])
  {
    print(item)
    d = xdata3 %>% filter(xdata3[col] == item)
    shap = shapiro.test(d$Plony)
    print(shap)
    if(shap$p.value < 0.05)
    {
      print("# pvalue mnniejsze od 0.05, zatem jest podstawa do odrzucenia hipotezy zerowej, zatem uklad nie jest zgodny z rozkladem normalnym")
    }
    else
    {
      print("# pvalue powyzej 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem uklad jest zgodny z rozkladem normalnym")
    }
  }
}

bartlett.test(xdata3$Plony, xdata3$Herbicydy)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione
bartlett.test(xdata3$Plony, xdata3$Odmiany)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione
bartlett.test(xdata3$Plony, xdata3$Terminy)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione


# analiza wariancji ANOVA
#Układ hipotez:
#H0_H:  średnie plony dla herbicydów (obiektów) są równe
#H1_H: ~H0_H
#H0_O: średnie plony dla odmian (wierszy) są równe
#H1_O: ~H0_O
#H0_T: średnie plony dla terminów siewów (kolumn) są równe
#H1_T: ~H0_T

summary(aov(xdata3$Plony ~ factor(xdata3$Herbicydy)+factor(xdata3$Odmiany)+factor(xdata3$Terminy)))

# pvalue wieksze od 0.05 w kazdym przypadku, zatem przyjmujemy hipoteze zerowa, zatem srednie ilosci plonow sie nie roznia pod wplywem zadnego czynnika
