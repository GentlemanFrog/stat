# Stat reference
setwd("C:/Users/Alek/Desktop/stat")


# lab01: 07-10-2021; podstawowe operacje i statystyki, manipulacje wektrow -------------------------------------------------------

# 5 do potegi 3
5^3

# pierwiastek trzeciego stopnia
8^(1/3)

# logarytm naturalny
log(7)

# logarytm o podstawie
log(7, 10)

# e do potegi
exp(3)

# sinus kata
sin(90 * (pi / 180))

# cosinus kata
cos(90 * (pi / 180))

# wektor z konkretnymi wartosciami
a = c(1,3,5)
a

# wektor z przedzialem wartosci
b = c(3:14)
b

# zlaczenie wektorow
ab = c(a, b)
ab

# zastapienie wartosci z konkretnych pozycji innymi wartosciami
a = c(1,3,5)
b = c(3:14)
ab = c(a, b)
ab[c(6:10)] = c(0, -6, -3, -1, -5)
ab

# powtorzenie calego wektora n-razy
rep(c(1, 2), 3)

# powtorzenie kazdej wartosci wektora n-razy
rep(c(1, 2), each = 3)

# powtorzenie kazdej wartosci wektora o unikalna wartosc
rep(c(1, 2), c(8, 2)) # 1 zostanie powtorzone 8 razy, a 2 bedzie powt 2 razy

# utworzenie sekwencji (od, do, co ile)
seq(1, 10, 2)

# powtorzenie kazdego elementu sekwencji
rep(seq(1, 11, 2), each = 2)

# powtorzenie kazdego elementu sekwencji o unikalna wartosc
rep(seq(1, 11, 2), c(3, 2, 1, 1, 2, 3))

# operacje arytmetyczne na wektorach sa aplikowane na kazda pozycje
wek3 = seq(1,10, 2)
wek4 = seq(101,110, 2)
wek3
wek4
wek7 = 5 * wek4 + 6 * wek3
wek7

# wyswietlenie wszystkich elementow wektora mniejszych/wiekszych/rownych
wek = 1:7
wek[wek > 4]
wek[wek < 6]
wek[wek = 2]

# podstawowe statystyki
plon = c(1.52, 1.57, 1.30, 1.62, 1.70, 2.05, 1.64, 1.95, 1.80, 1.76, 1.40, 1.92, 2.20)
plon
min(plon) # min wartosc
max(plon) # max wartosc
range(plon) # rozstep 
sum(plon) # suma elementow
prod(plon) # iloczyn elementow
mean(plon) # srednia
median(plon) # mediana
var(plon) # wariancja
sd(plon) # odchylenie standardowe
sort(plon) # sortowanie wektora rosnaco
summary(plon) # krotkie podsumowanie 




# lab02: 14-10-2021; wizualizacje i manipulacje ramkami danych -------------------------------------------------------

library(tidyr)
library(dplyr)

# podstawowe wykresy
plon = c(1.52, 1.57, 1.30, 1.62, 1.55, 1.70, 2.05, 1.64, 1.95, 
         1.80, 1.76, 1.40, 1.92, 2.20, 1.57, 1.59, 1.27, 1.79, 1.29, 1.84, 1.77, 1.72, 1.53, 1.32, 
         1.69, 1.95, 1.75, 1.08, 1.70, 1.45)
barplot(plon)
plot(plon)
hist(plon)
boxplot(plon)

# utworzenie dataframe z konkretnymi nazwami kolumn
A = c(6.7, 7.3, 8.0, 8.0, 7.9, 9.2, 10.1, 9.2, 8.3, 8.4, 8.0, 7.9)
B = c(7.5, 7.7, 7.7, 8.2, 8.9, 8.9, 10.6, 10.2, 9.4, 9.4, 8.2, 7.8)
C = c(5.9, 6.9, 7.0, 7.0, 9.5, 9.6, 9.6, 10.3, 8.1, 8.5, 8.6, 8.8)
zyto = data.frame(A, B, C)
zyto
# lub
zyto2 = data.frame('A' = c(6.7, 7.3, 8.0, 8.0, 7.9, 9.2, 10.1, 9.2, 8.3, 8.4, 8.0, 7.9),
                   'B' = c(7.5, 7.7, 7.7, 8.2, 8.9, 8.9, 10.6, 10.2, 9.4, 9.4, 8.2, 7.8),
                   'C' = c(5.9, 6.9, 7.0, 7.0, 9.5, 9.6, 9.6, 10.3, 8.1, 8.5, 8.6, 8.8))
zyto2

# lub
r = c(6.7, 7.3, 8.0, 8.0, 7.9, 9.2, 10.1, 9.2, 8.3, 8.4, 8.0, 7.9)
g = c(7.5, 7.7, 7.7, 8.2, 8.9, 8.9, 10.6, 10.2, 9.4, 9.4, 8.2, 7.8)
b = c(5.9, 6.9, 7.0, 7.0, 9.5, 9.6, 9.6, 10.3, 8.1, 8.5, 8.6, 8.8)
zyto3 = data.frame(r, g, b)
colnames(zyto3) = c('A', 'B', 'C')
zyto3

# wybor n elementow (wierszy) z danej kolumny
zyto$A[1:3]
zyto$C[1:2]
zyto$B

# zapis wykresu do pliku
png("gatunki.png")
boxplot(zyto)
dev.off()

# wykres krzywej wraz z rysowaniem prostych
curve((x+1)/(x**2-4), from = -5, to = 5)
abline(h = 0, v = 0)
abline(v = c(-2,2))

# dodanie nowego plota do wykresu - add = TRUE
curve(cos, from = -3*pi, to = 3*pi, add = TRUE, col = "red")

# dodanie kilku punktow w ukladzie wspolrzednych dla funkcji
f = function(x)
  (x+1)/(x**2-4)
points(c(f(0.8), f(1)), c(f(-0.2), f(0))) # pierwszy wektor to wartosci x, drugi wektor to wartosci y

# wpisanie legendy
legend(0,20, 
       legend = c("x**2", "(x-2)**2", "(x-2)**2 + 3", "x**2 + 3", "(x + 1)**2 - 2"),
       col = c("blue", "green", "yellow", "magenta", "cyan"), 
       lty = 1)

# format dlugi do formatu szerokiego dataframe
iris
df = iris %>% # load
  select(Species, Sepal.Length) %>% # select those 2 cols
  group_by(Species) %>% # group
  mutate(row = row_number()) %>% # identify values (just to be able to distinct)
  pivot_wider(names_from = Species, values_from = Sepal.Length) %>% # (rearrange)
  select(-row) # ignore ids
df
boxplot(iris$Sepal.Length ~ iris$Species)


# lab03: 21-10-2021; dplyr i ggplot -------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
vines = read.csv("stat03/wina-recenzje.csv", sep=",")

# wybor kolumn
data("iris")
dplyr::select(iris, Species, Sepal.Length)

# filtrowanie wierszy dla kolumn
iris %>%
  filter(Sepal.Width < 4, Species != "setosa")

# zliczenie wierszy
iris %>%
  filter(Petal.Width > 2) %>%
  count(.)

# ulozenie wierszy w kolejnosci na podstawie wartosci z danej kolumny
iris %>%
  filter(Sepal.Width < 4, Species != "setosa") %>%
  arrange(Petal.Length)

# unikalne wartosci
iris %>%
  distinct(Species)

# dodanie kolumny z danych z innych kolumn
new_iris = mutate(iris, Petal_dim = iris$Petal.Length * iris$Petal.Width)
new_iris

# wykres punktowy z ggplot
# arg1: dane, arg2: x/y, arg3: rodzaj wykresu
ggplot(filter(iris, Species == "virginica"), aes(Sepal.Length, Sepal.Width)) + geom_point()

# zliczenia wierszy dla grup
iris %>%
  group_by(Species) %>%
  count()

# dodanie kolumny z rozroznieniem na grupy (w kazdym wierszu dla grupy ta sama wartosc, np. sredniej)
iris %>%
  group_by(Species) %>%
  mutate(Avg_dim = mean(Petal.Width) * mean(Petal.Length))

# ilosc zmiennych i ilosc obserwacji w dataframe
vines %>%
  count() # obserwacje
ncol(vines) # zmienne

# typy danych w dataframe
lapply(vines, class)

# calkowita informacja o dataframe
str(vines)

# wylosowanie n procenta obserwacji z dataframe
vines %>%
  sample_frac(0.01)

# n pierwszych wynikow 
vines %>%
  arrange(desc(points)) %>%
  slice_head(n=3)

# zmiana nazwy kolumny
vines %>%
  rename(score = points)

# podusmowanie pipeline (przechwycenie do tymczasowego df)
vines %>%
  summarise(price_mean = mean(price, na.rm = T))

# obliczenie kwantyli
quantile(vines$price, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = T)

# przechwycenie wyniku i wybor najmniejszej wartosci
vines %>%
  group_by(country) %>%
  summarize(country_price = mean(price, na.rm = T)) %>%
  slice_min(country_price)

# zastosowanie funkcji dla wszystkich kolumn - across
vines %>%
  group_by(country) %>%
  mutate(country_price = mean(price, na.rm = T), num_country = n()) %>%
  dplyr::select(country_price, num_country) %>%
  distinct(across())

# lab04: 28-10-2021; testowanie statystyczne ------------------------------

# nalezy uzywac drzewa hipotez i testow statystycznych Andrzeja
# ogolne zasady:
# w pierwszym kroku sprawdzic ile prob testujemy i jaki jest rodzaj danych (ilosciowe/jakosciowe)
# nastepnie postepowac zgodnie z drzewem - najczesciej jest tak:
  # 1. trzeba sie upewnic czy proby sa zalezne lub niezalezne
  # 2. pozniej zwykle trzeba sprawdzic zgodnosc wszystkich prob z rozkladem normalnym
  #____________________________________________________
  # zgodnosc z rozkladem normalnym: FALSE < 0.05 < TRUE
  # pvalue > 0.05 oznacza, ze nie ma podstaw do odrzucenia H0
  # pvalue < 0.05 oznacza, ze jest podstawa do odrzucenia H0 i przyjecia hipotezy alternatywnej (H1)
  # Oznacza to tyle, ze domyslnie hipoteza zerowa zaklada zgodnosc z rozkladem normalnym
  #____________________________________________________
  # 3. nastepnie, w zaleznosci od sytuacji na drzewie, byc moze trzeba bedzie sprawdzic rownosc wariancji
  #____________________________________________________
  # rownosc wariancji: FALSE < 0.05 < TRUE
  # Oznacza to tyle, ze domyslnie hipoteza zerowa zaklada rownosc wariancji
  #____________________________________________________
  # 4. Nastepnie nalezy zapisac uklad hipotez
  # 5. Nastepnie wykonuje sie odpowiedni test statystyczny
  # 6. Interpretacja testu
  #____________________________________________________
  # pvalue > 0.05 oznacza, ze nie ma podstaw do odrzucenia H0
  # pvalue < 0.05 oznacza, ze jest podstawa do odrzucenia H0 i przyjecia hipotezy alternatywnej (H1)
  #____________________________________________________
  # 7. Ewentualne testy post-hoc (tylko wtedy jak pvalue < 0.05)

#______________________________________________________
# dany jest ciężar w gramach 1000 nasion dla dwóch rodów seradeli, Czy różnica w średnim ciężarze tych rodów jest istotna?

rody = read.csv("stat04/zad1.csv", sep=",")

## test zgodnoĹ›ci z rozkladem normalnym ###
shapiro.test(rody$Rod.A)
shapiro.test(rody$Rod.B)
## prĂłby sa zgodne z rozkĹ‚adem normalnym i prĂłby sÄ… niezaleĹĽne

## test rĂłwnosci wariancji
var.test(rody$Rod.A, rody$Rod.B)
## prĂłby maja rowne wairancje

## ukĹ‚ad hipotez
#H0: Ĺ›redni ciezar A = sredni ciezar B
#H1: sr ciezar A != sr ciezar B

t.test(rody$Rod.A, rody$Rod.B, var.equal = T) # var.equal = T, bo rowne wariancje
# pvalue jestm nniejsze od 0.05, zatem moĹĽna odrzuciÄ‡ hipotezÄ™ zerowÄ… i przyjac alternatywna, co oznacza, ĹĽe roznica w cizarach pomiedzy rodami jest istotna

#______________________________________________________
# Oznaczono procent tłuszczu w 18 próbkach mleka za pomocą dwóch metod: metody Gerbera (metoda G) i metody Burata (metoda B).Czy metody te dają takie same wyniki?

methods = read.csv("stat04/zad2.csv", sep = ",")

## test zgodnoĹ›ci z rozkladem normalnym ###
shapiro.test(methods$MetodaG)
shapiro.test(methods$B)
## prĂłby sa zgodne z rozkĹ‚adem normalnym i proby sa zaleĹĽne

##ukĹ‚ad hipotez
#H0: Ĺ›rednia wynikow metody G = srednia wynikow metody B
#H1: Ĺ›rednia wynikow metody G != srednia wynikow metody B

t.test(methods$MetodaG, methods$B, paired = T)
## pvalue jest mniejsze od 0.05, zatem mozna odrzucic hipoteze zerowa i przyjac alternatywna, co oznacza, ze metody daja rozne wyniki

#______________________________________________________
#Na pierwszym roku studiów przebadano 5 studentów oraz 4 studentki pod względem zdolności matematycznych w celu weryfikacji przypuszczenia, że studenci są pod tym względem 
#lepsi od studentek. Wyniki testu są następujące :

students = read.csv("stat04/zad5.csv")
## test wilcoxona, poniewaĹĽ sÄ… to wartoĹ›ci ocen, ktore nie sÄ… wyliczalne

##uklad hipotez
#H0: wynikki studentow = wyniki studentek
#H1: wyniki studentow > wynikow studentek
wilcox.test(students$studenci, students$studentki, alternative = "greater")
# pvalue jest wieksze od 0.05, zatem nie map podstaw do odrzucenia hipotezy zerowej i przyjmuje hipoteze zerowa, zatem wyniki studentow sÄ… takie same jak studente

#______________________________________________________
# Porównano długości kłosów czterech odmian uprawnych D, A, J i N pewnej trawy. Uzyskano 
# następujące obserwacje (w cm): Dokonać szczegółowych porównań odmian.

grass = read.csv("stat04/zad7.csv")
## test rownosci wariancji
bartlett.test(grass)
## proby maja rowne wariancje

## test zgodnoĹ›ci z rozkladem normalnym ###
shapiro.test(grass$D)
shapiro.test(grass$A)
shapiro.test(grass$J)
shapiro.test(grass$N)
#albo, bardziej elegancko
shapiro_results = lapply(grass, function(x){shapiro.test(x)$p.value})
all(shapiro_results > 0.05)

## prĂłby sa zgodne z rozkĹ‚adem normalnym i proby sa niezaleĹĽne

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



# lab05: 18-10-2021; korelacje i regresja liniowa -------------------------

# korelacja wskazuje sile i kierunek zaleznosci pomiedzy dwoma cechami
# korelacja moze byc ujemna (wraz ze wzrostem jednej cechy, druga maleje) i dodatnia (wraz ze wzrostem jednej, rosnie druga)
# wartosci wspolczynnika korelacji:
  # |r| = 0 -> brak korelacji
  # 0.0 < |r| <= 0.1 -> korelacja nikla
  # 0.1 < |r| <= 0.3 -> korelacja slaba
  # 0.3 < |r| <= 0.5 -> korelacja przecietna (srednia)
  # 0.5 < |r| <= 0.7 -> korelacja wysoka
  # 0.7 < |r| <= 0.9 -> korelacja bardzo wysoka
  # 0.9 < |r| < 1.0  -> korelacja niemal pelna (silna)
  # |r| = 1 -> korelacja pelna (bardzo silny zwiazek liniowy)
# kazda korelacje powinno sie przetestowac pod katem istotnosci statystycznej
# przy okreslaniu korelacji trzeba zwrocic uwage na typ danych:
  # cechy ilosciowe: cor.test(x, y, method = "pearson")
  # cechy jakosciowe: cor.test(x, y, method = "spearman")
  # tablice kontygencji:
    # dane musza byc w macierzy (matrix)
    # liczebnosc wszytkich cech > 5: chisq.test(matrix)
    # liczebnosc przynajmniej jednej cechy < 5: fisher.test(matrix)

# regresja liniowa wskazuje zaleznosc liniowa miedzy 2 cechami - wplyw zmiennej x na y
# regresja liniowa ma postac response (y) = intercept (a) + predictor (x) * coef (b) 
# miara dopasowania regresji do danych oznacza wspolczynnik determinacj R^2
# nalezy sprawdzac istotnosc statystyczna komponentow rownania regresji - intercepta oraz predictora
# wielokrotna regresja dziala podobnie:
  # roznica - jest wiele predictorow
  # statystyczna istotnosc komponentow jest sprawdzana przez ANOVA
  # krokowa regresja wsteczna:
    # moze sie okazac, ze mozliwa jest redukcja wielokrotnosci regresji, gdy dla testu statystycznego regresji wielokrotnej 
      #ktorys z elementow nie bedzie statystycznie istotny - wtedy mozna przerpowadzic regresje na mniejszej ilosci cech (moze to zmniejszyc dopasowanie - generalnie tego na cw nie robilismy)

#______________________________________________________
# W pewnym doświadczeniu farmakologicznym bada się korelację pomiędzy wydzielaniem się dwóch substancji A i B. 
# Uzyskano następujące obserwacje (w mg/kg wagi): Na podstawie powyższej próby oszacować i przetestować współczynnik korelacji między wydzielaniem się obu substancji.

A = c(0.6, 0.2, 0.3, 0.7, 0.5, 0.8, 0.7, 0.4, 0.6, 0.8)
B = c(1.5, 1.5, 1.1, 1.2, 1.4, 1.1, 1.6, 1.0, 1.9, 1.7)
cor.test(A, B, method = "pearson")

# wspolczynnik korelacji wynosi 0.2375 (slaba korelacja)
# test pearsona wskazal pvalue wieksze niz 0.05, zatem przyjmujemy hipoteze zerowa i stwierdzamy, ze korelacja miedzy wydzielaniem sie obu substancji jest statysztycznie nieistotna

#______________________________________________________
#Podczas badania jakości jabłek oceniano owoce ze względu na uszkodzenia spowodowane przez owocówkę 
#jabłkóweczkę (U – owoce uszkodzone, N – owoce nieuszkodzone) oraz porażenie parchem jabłoniowym (C – owoce czyste, 
#P – owoce z plamami). W wyniku klasyfikacji owoców uzyskano następujące liczebności
#Czy na poziomie istotności 0,01 można uznać, że badane zmienne są niezależne?

apples = matrix(c(29,17,194,68), ncol=2)
apples
chisq.test(apples)

# test chi-squared wskazal pvalue wieksze niz 0.01, zatem nie odrzucamy hipotezy zerowej i stwierdzamy, ze zmienne sa niezalezne

#______________________________________________________
# Opinie konsumentów dotyczące dwóch gatunków kawy na podstawie badań sondażowych. Każdy z sześćdziesięciu 
#konsumentów oceniał każdą z dwóch kaw w skali punktowej. Uzyskano następujące wyniki:
#Czy istniej zależność między ocenami obu kaw?

opinions = matrix(c(4,14,17,19,6,6,16,21,13,4), ncol = 2)
opinions
fisher.test(opinions)

# test fishera wskazal pvalue wieksze od 0.05, zatem przyjmujemy hipoteze zerowa i stwierdzamy, ze nie ma zaleznosci pomiedzy ocenami

#______________________________________________________
# W badaniach nad szybkością oddawania wody przez wędlinę typu serwolatka poszukiwano związku między liczbą dni 
# przechowywania a zawartością wody w wędlinie (w %). Wyniki przedstawia tabela
# Wyznaczyć regresję liniową procentowej zawartości wody w wędlinie względem liczby dni przechowywania.
# Obliczyć i przetestować współczynnik korelacji między zawartością wody w wędlinie a liczbą dni przechowywania.

ham = read.csv("stat05/z6.csv", sep = ",")
reg = lm(ham$zw ~ ham$LDP)
summary(reg)

# regresja
# rownanie regresji: y = 49.29 - 0.77 * LDP
# pvalue wyrazu wolnego a oraz wspolczynnika kierunkowego b sa mniejsze niz 0.05, zatem odrzucamy hipotezy zerowe, co oznacza, ze te wspolczynniki sa istotne statystycznie
# stopien dopasowania regresji liniowej do danych wynosi okolo 82%

# korelacja
cor.test(ham$zw, ham$LDP, method = "pearson")
# wspolczynnik korelacji wynosi -0.92 (bardzo moncna ujemna korelacja)
# test pearsona wskazal pvalue mniejsze niz 0.05, zatem odrzucamy hipoteze zerowa i stwierdzamy, ze korelacja miedzy zawartoscia wody w wedlinie a liczba dni przechowywania jest statysztycznie istotna

#______________________________________________________
#  Badano cztery cechy rzepaku ozimego: plon, długość łuszczyn, zawartość tłuszczu i sumę glukozynolanów.
# Uzyskano następujące wyniki:
# Znaleźć równanie regresji wielokrotnej liniowej, określającej zależność plonu od: długości łuszczyn, zawartości tłuszczu i sumy glukozynolanów.

rape = read.csv("stat05/z11.csv", sep = ",")
reg = lm(rape$Plon ~ rape$DlugoscLuszczyn + rape$ZawartoscTluszczu + rape$SumaGlukozynolanow)
summary(reg)

# rownanie regresji: y = -51.03 + 0.47 * dlugoscLuszczyn +  1.3 * zawartoscTluszczu - 0.35 * sumaGlukozynolanow

# powinna byc tutaj jeszcze ANOVA - nie ma ze wzgledu na brak polecenia 

# potencjalna regresja krokowa wsteczna idk
reg2 = lm(rape$Plon ~ rape$DlugoscLuszczyn + rape$ZawartoscTluszczu)
summary(reg2)

# bardziej elegancka regresja krokowa wsteczna
modelstep = step(reg)
summary(modelstep)
# opis tak jak zwykle
# w teorii znow powinna tu byc ANOVA
# lab06: 25-11-2021; modele krzywych wzrostu ------------------------------

# lista modeli krzywych:
getMeanFunctions()
# EXD.2() oraz EXD.3() - wykladniczy
# 
# miara dopasownaia dnaych z modelu do danych empirycznych:
  # AIC
  # BIC
  # obie daja zblizone wartosci: im mniejsze, tym lepsze dopasowanie

# mozna rowniez fitowac arbitralne modele uzywajac funkcji nls()

library(drc)
library(flexmix)
library(stats)
library(ggplot2)
library(dplyr)


#______________________________________________________
#  Niech dany będzie model postaci v = (Vmax * [S]) / (Km + [S])
# Niech będą dane wyniki postaci:
# Zastosować:
#1. dwuparametryczny model Michaelisa Mentena z pakietu drc
#2. funkcję nls.
#Przedstawić otrzymane wyniki oraz wykonać odpowiednie rysunki.

curves = read.csv("stat06/krzywe.csv", sep=",")
curves.mm = drm(v ~ S, data=curves, fct=MM.2())
summary(curves.mm)
plot(curves)
lines(sort(curves$S), sort(predict(curves.mm)))

curves.nls = nls(v ~ Vm * S / (K + S), data = curves, start = list(K=max(curves$v), Vm=max(curves$v)))
summary(curves.nls)
plot(curves)
#sortownaie tutaj
lines(sort(curves$S), sort(predict(curves.nls)))


#______________________________________________________
# Dla danych „dataset1.txt” i „dataset2.txt” dopasować różne funkcje z pakietu drc (np.
# wykładniczy, Gompertza, logistyczny, log-logistyczny i Weibulla).

d1 = read.csv("stat06/dataset1.csv", sep = ",")
d2 = read.csv("stat06/dataset2.csv", sep=",")
d1
d2

d1.weibull = drm(response ~ time, data=d1, fct = weibull2())
d2.weibull = drm(response ~ time, data=d2, fct = weibull2())
summary(d1.weibull)
plot(d1.weibull)
summary(d2.weibull)
plot(d2.weibull)

d1.exd = drm(response ~ time, data = d1, fct = EXD.2())
summary(d1.exd)
plot(d1.exd)
d2.exd = drm(response ~ time, data = d2, fct = EXD.2())
summary(d2.exd)
plot(d2.exd)

d1.gompertz = drm(response ~ time, data = d1, fct = gompertz())
summary(d1.gompertz)
plot(d1.gompertz)
d2.gompertz = drm(response ~ time, data = d2, fct = gompertz())
summary(d2.gompertz)
plot(d2.gompertz)

d1.LL = drm(response ~ time, data = d1, fct = LL.2())
summary(d1.LL)
plot(d1.LL)
d2.LL = drm(response ~ time, data = d2, fct = LL.2())
summary(d2.LL)
plot(d2.LL)

# logistic(method = n) n zalezy od ilosci parametrow
d1.log = drm(response ~ time, data = d1, fct = logistic(method = "2"))
summary(d1.log)
plot(d1.log)
d2.log = drm(response ~ time, data = d2, fct = logistic(method = "2"))
summary(d2.log)
plot(d2.log)

AIC(d1.weibull, d1.exd, d1.gompertz, d1.LL, d1.log)
BIC(d1.weibull, d1.exd, d1.gompertz, d1.LL, d1.log)

AIC(d2.weibull, d2.exd, d2.gompertz, d2.LL, d2.log)
BIC(d2.weibull, d2.exd, d2.gompertz, d2.LL, d2.log)


# lab07: 02-12-2021; uklady doswiadczalne ---------------------------------

# doswiadczenia jednoczynnikowe
# doswiadczenia dwuczynnikowe bez interakcji
# doswiadczenia dwuczynnikowe z interakcja
# bloki losowane
# kwadraty lacinskie


# nie wypisywalem tresci zadan bo syf bylby jeszcze wiekszy
# kazde zadanie odpowiada kazdemu kolejnemu ukladowi wymienionemu wyzej
#______________________________________________________
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

# lub bez wczytywania i pivotowania
# plon1 <- c(33.2, 44.2, 18.6, 14.6, 20.4, 11.0, 36.2, 51.0, 13.0, 18.8, 14.4, 22.6)
# plon2 <- c(44.2, 50.6, 18.0, 14.2, 21.9, 16.2, 41.4, 45.2, 20.0, 19.1, 24.0, 25.6)
# plon3 <- c(50.2, 52.6, 24.2, 16.4, 18.2, 27.3, 53.0, 45.0, 21.6, 19.0, 21.0, 27.6)
# plon4 <- c(46.2, 49.0, 34.2, 15.5, 16.4, 21.6, 52.4, 43.6, 17.2, 22.2, 15.0, 27.8)
# 
# plon <- c(plon1, plon2, plon3, plon4)
# dawki <- c(rep(c(0,40,80,120),12))
# odmiany <- c(rep(c(1,1),8), rep(c(2,2),8), rep(c(3,3),8))
# 
# data3 <- data.frame(plon, odmiany, dawki)
# 
# data3

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
# H0_nawozenie: nawoĹĽenie nie wpĹ‚ywa na Ĺ›rednie plony masy zielonej.
# H1_nawozenie: ~H0_nawozenie
# H0_siew: sposoby siewu nie wpĹ‚ywajÄ… na Ĺ›rednie plony masy zielonej
# H1_siew: ~H0_siew
# H0_NxS: interakcja nawoĹĽeĹ„ ze sposobami siewu jest nieistotna
# H1_NxS: ~H0_NxS

model = aov(xdata2$plony ~ factor(xdata2$nawozenia)*factor(xdata2$Siew))
summary(model)
# pvalue mnniejsze od 0.05 w przypadku nawozen oraz siewu, zatem odrzucamy hipoteze zerowa i przyjmujemy alternatywna, zatem nawozenie i siew wplywaja istotnie statystyczniee na srednie plony masy zielonej
# pvalue wieksze od 0.05 w przypadku interakcji, zatem przyjmujemy hipoteze zerowa, zatem inteerakcja nawozenia i siewu nie wplywa istotnie statystyczniee na srednie plony masy zielonej

# w tym zadaniu opcjonalne
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

####
# w teorii mamy sprawdzac tylko obiekty czyli typy uprawy, zatem bartlett plonĂłw z blokami teoretycznie nie jest potrzebny 
bartlett.test(as.numeric(xdata2$plon), xdata2$Bloki)
# ppvalue powyzej 0.05, zatem noie podstawy do odrzucenia hipotezy zerowej, zatem uklad zalozenie o jednorodnosci wariancji jest spelnione
####


# analiza wariancji ANOVA
#UkĹ‚ad hipotez:
#H0: Ĺ›rednie plonow dla typĂłw uprawy sa rowne
#H1: ~H0

model = aov(as.numeric(xdata3$plon) ~ factor(xdata3$Typy)+factor(xdata3$Bloki))
summary(model)

# pvalue mniejsze od 0.05 dla typu uprawy, zatem przyjmujemy hipoteze alternatywna ktora wskazuje na to, ze srednie plony nie sa rowne dla roznych typow uprawy

TukeyHSD((model))
plot(TukeyHSD(model))

# pod wzgledem blokow - sa one wszystkie statystycznie znaczaco nierowne
# pod wzgledem typow uprawy - 2-1 oraz 3-1 sÄ… statystycznie znaczaco rozne

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
#UkĹ‚ad hipotez:
#H0_H:  Ĺ›rednie plony dla herbicydĂłw (obiektĂłw) sÄ… rĂłwne
#H1_H: ~H0_H
#H0_O: Ĺ›rednie plony dla odmian (wierszy) sÄ… rĂłwne
#H1_O: ~H0_O
#H0_T: Ĺ›rednie plony dla terminĂłw siewĂłw (kolumn) sÄ… rĂłwne
#H1_T: ~H0_T

summary(aov(xdata3$Plony ~ factor(xdata3$Herbicydy)+factor(xdata3$Odmiany)+factor(xdata3$Terminy)))

# pvalue wieksze od 0.05 w kazdym przypadku, zatem przyjmujemy hipoteze zerowa, zatem srednie ilosci plonow sie nie roznia pod wplywem zadnego czynnika


# lab08: 09-12-2021; uogolnione modele liniowe (GLM) ------------------------





# lab09: 16-12-2021; PCA --------------------------------------------------
# lab10: 13-01-2022; Klaster ---------------------------------------------
# lab11: 20-01-2022; Manova -----------------------------------------------



