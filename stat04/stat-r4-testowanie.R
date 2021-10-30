#r4-testowanie

setwd("~/stat/stat04")

#zad1
rody = read.csv("zad1.csv", sep=",")

## test zgodności z rozkladem normalnym ###
shapiro.test(rody$Rod.A)
shapiro.test(rody$Rod.B)
## próby sa zgodne z rozkładem normalnym i próby są niezależne

## test równosci wariancji
var.test(rody$Rod.A, rody$Rod.B)
## próby maja rowne wairancje

## układ hipotez
#H0: średni ciezar A = sredni ciezar B
#H1: sr ciezar A != sr ciezar B

t.test(rody$Rod.A, rody$Rod.B, var.equal = T)
# pvalue jestm nniejsze od 0.05, zatem można odrzucić hipotezę zerową i przyjac alternatywna, co oznacza, że roznica w cizarach pomiedzy rodami jest istotna

#zad2
methods = read.csv("zad2.csv", sep = ",")

## test zgodności z rozkladem normalnym ###
shapiro.test(methods$MetodaG)
shapiro.test(methods$B)
## próby sa zgodne z rozkładem normalnym i proby sa zależne

##układ hipotez
#H0: średnia wynikow metody G = srednia wynikow metody B
#H1: średnia wynikow metody G != srednia wynikow metody B

t.test(methods$MetodaG, methods$B, paired = T)
## pvalue jest mniejsze od 0.05, zatem mozna odrzucic hipoteze zerowa i przyjac alternatywna, co oznacza, ze metody daja rozne wyniki

#zad3
trees = read.csv("zad3.csv")

## test zgodności z rozkladem normalnym ###
shapiro.test(trees$ulica)
shapiro.test(treea$park)
## próby sa niezgodne z rozkładem normalnym i proby sa niezależne

##uklad hipotez
#H0: średnia wysokosc przy ulicy = srednia wysokosc przy parku
#H1: średnia wysokosc przy ulicy != srednia wysokosc przy parku

wilcox.test(trees$ulica, trees$park)
# pvalue jest mniejsze od 0.05, zatem mozna odrzucic hipoteze zerowa i przyjac alternatywna, co oznacza, ze lokalizacja ma istotny wplyw na wysokosc

#zad4
fuel = read.csv("zad4.csv")
## test zgodności z rozkladem normalnym ###
shapiro.test(fuel$Star_Paliwo)
shapiro.test(fuel$Nowe_Paliwo)
## próby sa niezgodne z rozkładem normalnym i proby sa niezależne

##uklad hipotez
#H0: dystans na starym paliwie = dystans na nowym paliwie
#H1: dystans na starym paliwie < dystans na nowym paliwie

wilcox.test(fuel$Star_Paliwo, fuel$Nowe_Paliwo, alternative = "less")
# pvalue jest mniejsze od 0.05, zatem mozna  odrzucic hipotez2ze zerowa i przyjac altwernatywna, co oznacza, ze na nowym paliwie mozna przejechac wiekszy dystans

#zad5
students = read.csv("zad5.csv")
## test wilcoxona, ponieważ są to wartości ocen, ktore nie są wyliczalne

##uklad hipotez
#H0: wynikki studentow = wyniki studentek
#H1: wyniki studentow > wynikow studentek
wilcox.test(students$studenci, students$studentki, alternative = "greater")
# pvalue jest wieksze od 0.05, zatem nie map podstaw do odrzucenia hipotezy zerowej i przyjmuje hipoteze zerowa, zatem wyniki studentow są takie same jak studentek

#zad6
wheat = read.csv("zad6.csv")
## test zgodności z rozkladem normalnym ###
shapiro.test(wheat$Gatunek_A)
shapiro.test(wheat$Gatunek_B)
shapiro.test(wheat$Gatunek_C)
#albo, bardziej elegancko
shapiro_results = lapply(wheat, function(x){shapiro.test(x)$p.value})
all(shapiro_results > 0.05)
# jesli powyzsze da TRUE, to wszystkie proby sa zgodne z rozkladem normalnym

## próby sa zgodne z rozkładem normalnym i proby sa niezależne

## test rownosci wariancji
bartlett.test(wheat)
## proby maja rowne wariancje

##uklad hipotez
#H0: dlugosc klosow A = dlugosc klosow B = dlugosc klosow C
#H1: dlugosc klosow A != dlugosc klosow B != dlugosc klosow C
gat_value = c(wheat$Gatunek_A, wheat$Gatunek_B, wheat$Gatunek_C)
gat_name = c(rep("A", 12), rep("B", 12), rep("C", 12))

summary(aov(gat_value ~ gat_name))
# pvalue jest wieksze od 0.05, mzatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem dlugosci klosow sa statystycznie rowne

#zad7
grass = read.csv("zad7.csv")
## test rownosci wariancji
bartlett.test(grass)
## proby maja rowne wariancje

## test zgodności z rozkladem normalnym ###
shapiro.test(grass$D)
shapiro.test(grass$A)
shapiro.test(grass$J)
shapiro.test(grass$N)
#albo, bardziej elegancko
shapiro_results = lapply(grass, function(x){shapiro.test(x)$p.value})
all(shapiro_results > 0.05)

## próby sa zgodne z rozkładem normalnym i proby sa niezależne

##uklad hipotez
#H0: dl klosow D = dl klosow A = dl klosow J = dl klosow N 
#H1: !H0

grass_value = c(grass$D, grass$A, grass$J, grass$N)
grass_name = c(rep("D", 10), rep("A", 10), rep("J", 10), rep("N", 10))

a_model = aov(grass_value ~ grass_name)
summary(a_model)
# pvalue jest mniejsze od 0.05, zatem mozna odrzucic hipoteze zerowa, zatem dlugosci klosow sa istotnie statystycznie rozne

# pvalue jest mniejsze od 0.05, zatem mozna wykonac testy post hoc
# test tukeya
TukeyHSD(a_model)

#zad8
bees = read.csv("zad8.csv")
boxplot(bees)

## test zgodności z rozkladem normalnym ###
shapiro.test(bees$ul1)
shapiro.test(bees$ul2)
## próby sa zgodne z rozkładem normalnym i proby sa niezależne

## test równosci wariancji
var.test(bees$ul1, bees$ul2)
## próby maja rowne wairancje

##uklad hipotez
#H0: srednia srednica ula 1 = srednia srendica ula 2
#H1: !H0
t.test(bees$ul1, bees$ul2, var.equal = T)
# pvalue jest wieksze od 0.05, mzatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem srednie sa statystycznie rowne

#zad9
rabbit = read.csv("zad9.csv")
boxplot(rabbit)

## test zgodności z rozkladem normalnym ###
shapiro.test(rabbit$Kontrolna)
shapiro.test(rabbit$Preparat)
## próby sa zgodne z rozkładem normalnym i proby sa zależne

##uklad hipotez
#H0: srednie frakcji kontroli = srednie frakcje leczonych
#H1: !H0

t.test(rabbit$Kontrolna, rabbit$Preparat, paired = T)
# pvalue jest wieksze od 0.05, mzatem nie ma podstaw do odrzucenia hipotezy zerowej, zatem srednie frakcje sa statystycznie rowne

#zad10
cut = read.csv("zad10.csv")
boxplot(cut)
## test rownosci wariancji
bartlett.test(cut)
## proby maja rowne wariancje
## test zgodności z rozkladem normalnym ###
shapiro.test(cut$I)
shapiro.test(cut$II)
shapiro.test(cut$III)
shapiro.test(cut$IV)
#albo, bardziej elegancko
shapiro_results = lapply(cut, function(x){shapiro.test(x)$p.value})
all(shapiro_results > 0.05)
## próby sa zgodne z rozkładem normalnym i proby sa niezależne

##uklad hipotez
#H0: temrin ciecia nie wplywa na plon zielonki
#H1: !H0

# zgodnosc z rozkladem normalnym, proby zalezne oraz wariancje sa rowne - test anova z powt. pomiarami 
cut_value = c(cut$I, cut$II, cut$III, cut$IV)
cut_name = c(rep("I", 8), rep("II", 8), rep("III", 8), rep("IV", 8))
a_model = aov(cut_value ~ cut_name)
summary(a_model)
# pvalue jest mniejsze od 0.05, zatem mozna odrzucic hipoteze zerowa, zatem termin ciecia lubinu ma wplyw na plon zielonki lubinu




