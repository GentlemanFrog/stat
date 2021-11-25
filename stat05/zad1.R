#r5-zad1
m = c(15,20,25,30,35,50)
c = c(15, 25, 30,35,45,50)
cor.test(m, c, method = "pearson")

# wspolczynnik korelacji wynosi 0.956 (niemal pelna korelacja)
# test pearsona wskazal pvalue mniejsze niz 0.05, zatem odrzucamY hipoteze zerowa i stwierdzamy, ze zaleznosc pomiedzy spozyciem cukru i maaki jest statysztycznie istotna

#zad2
A = c(0.6, 0.2, 0.3, 0.7, 0.5, 0.8, 0.7, 0.4, 0.6, 0.8)
B = c(1.5, 1.5, 1.1, 1.2, 1.4, 1.1, 1.6, 1.0, 1.9, 1.7)
cor.test(A, B, method = "pearson")
# wspolczynnik korelacji wynosi 0.2375 (slaba korelacja)
# test pearsona wskazal pvalue wieksze niz 0.05, zatem przyjmujemy hipoteze zerowa i stwierdzamy, ze korelacja miedzy wydzielaniem sie obu substancji jest statysztycznie nieistotna

#zad3
apples = matrix(c(29,17,194,68), ncol=2)
chisq.test(apples)

# test chi-squared wskazal pvalue wieksze niz 0.01, zatem nie odrzucamy hipotezy zerowej i stwierdzamy, ze zmienne sa niezalezne

#zad4
data = matrix(c(21,33,45,30,71,41,37,75,48,47,93,35,27,50,49,47,53,43,55,50), ncol=4)
data
chisq.test(data)

# test chi-squared wskazal pvalue mniejsze niz 0.05, zatem odrzucamy hipoteze zerowa i stwierdzamy, ze zmienne sa zalezne (jest zaleznosc miedzy zarobkami a wyksztalceniem)

#zad5
opinions = matrix(c(4,14,17,19,6,6,16,21,13,4), ncol = 2)
fisher.test(opinions)

# test fishera wskazal pvalue wieksze od 0.05, zatem hipoteze zerowa i stwierdzamy, ze nie ma zaleznosci pomiedzy ocenami

#zad6
ham = read.csv("z6.csv", sep = ",")
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

#zad7

# regresja
income = read.csv("z7.csv", sep=",")
reg = lm(income$obroty ~ income$ls)
summary(reg)
# rownanie regresji: y = 0.43 + 0.81 * income$ls
# pvalue wyrazu wolnego a jest wieksze od 0.05, zatem przyjmujemy dla niego hipoteze zerowa, zatem jest on nieistotny statystycznie; wspolczynnik kierunkowy b jest mniejsze niz 0.05, zatem odrzucamy hipoteze zerowa, co oznacza, ze tn wspolczynnik jest istotny statystycznie

# korelacja
cor.test(income$ls, income$obroty, method = "pearson")
# wspolczynnik korelacji wynosi 0.97 (niemal pelna korelacja)
# test pearsona wskazal pvalue mniejsze niz 0.05, zatem odrzucamy hipoteze zerowa i stwierdzamy, ze korelacja miedzy liczba sprzedawcow a obrotami jest statysztycznie istotna

#zad8
fat = read.csv("z8.csv", sep = ",")
reg = lm(fat$wk~fat$zt)
summary(reg)

# rownanie regresji: y = 23.16 + 9.02 * fat$zt
# pvalue wyrazu wolnego a oraz wspolczynnika kierunkowego b sa mniejsze niz 0.05, zatem odrzucamy hipotezy zerowe, co oznacza, ze te wspolczynniki sa istotne statystycznie

#zad9
# regresja
vit = read.csv("z9.csv", sep = ",")
reg = lm(vit$swC ~ vit$cmwg)
summary(reg)

# rownanie regresji: y = 6.25 + 0.74 * vit$cmwg
# pvalue wyrazu wolnego a oraz wspolczynnika kierunkowego b sa mniejsze niz 0.05, zatem odrzucamy hipotezy zerowe, co oznacza, ze te wspolczynniki sa istotne statystycznie

# korelacja
cor.test(vit$cmwg, vit$swC)
# wspolczynnik korelacji wynosi 0.93 (bardzo mocna korelacja)
# test pearsona wskazal pvalue mniejsze niz 0.05, zatem odrzucamy hipoteze zerowa i stwierdzamy, ze korelacja miedzy badanymi cechami jest statysztycznie istotna

#zad10
sales = read.csv("z10.csv", sep = ",")
reg = lm(sales$ws~sales$cr)
summary(reg)

# rownanie regresji: y = 1.39 + 0.12 * sales$cr
# pvalue wyrazu wolnego a oraz wspolczynnika kierunkowego b sa mniejsze niz 0.05, zatem odrzucamy hipotezy zerowe, co oznacza, ze te wspolczynniki sa istotne statystycznie

#zad11
rape = read.csv("z11.csv", sep = ",")
reg = lm(rape$Plon ~ rape$DlugoscLuszczyn + rape$ZawartoscTluszczu + rape$SumaGlukozynolanow)
summary(reg)

# rownanie regresji: y = -51.03 + 0.47 * dlugoscLuszczyn +  1.3 * zawartoscTluszczu - 0.35 * sumaGlukozynolanow

# potencjalna regresja krokowa wsteczna idk
reg2 = lm(rape$Plon ~ rape$DlugoscLuszczyn + rape$ZawartoscTluszczu)
summary(reg2)

#zad-excel

x1 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y1 = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68)

x2 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y2 = c(9.14, 8.14, 8.74, 8.77, 9.26, 8.1, 6.13, 3.1, 9.13, 7.26, 4.74)

x3 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y3 =c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73)

x4 = c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8)
y4 = c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.5, 5.56, 7.91, 6.89)

z12 = data.frame(x1, y1, x2, y2, x3, y3, x4, y4)

#srednia
round(mean(z12$x1),2)
round(mean(z12$y1),2)

round(mean(z12$x2),2)
round(mean(z12$y2),2)

round(mean(z12$x3),2)
round(mean(z12$y3),2)

round(mean(z12$x4),2)
round(mean(z12$y4),2)

#wariancja
round(var(z12$x1),2)
round(var(z12$y1),2)

round(var(z12$x2),2)
round(var(z12$y2),2)

round(var(z12$x3),2)
round(var(z12$y3),2)

round(var(z12$x4),2)
round(var(z12$y4),2)

#wspolczynnik korelacji

round(cor(z12$x1, z12$y1),2)

round(cor(z12$x2, z12$y2),2)

round(cor(z12$x3, z12$y3),2)

round(cor(z12$x4, z12$y3),2)

#Rownanie regresji liniowej
lm(z12$y1~z12$x1)

lm(z12$y2~z12$x2)

lm(z12$y3~z12$x3)

lm(z12$y4~z12$x4)

#wspolczynnik determinacji r^2

round(cor(z12$x1, z12$y1)^2,2)

round(cor(z12$x2, z12$y2)^2,2)

round(cor(z12$x3, z12$y3)^2,2)

round(cor(z12$x4, z12$y3)^2,2)

#wykresy
par(mfrow=c(2,2))
ab1 = lm(z12$y1~z12$x1)
plot(z12$y1~z12$x1)
abline(ab1)

ab2 = lm(z12$y2~z12$x2)
plot(z12$y2~z12$x2)
abline(ab2)

ab3 = lm(z12$y3~z12$x3)
plot(z12$y3~z12$x3)
abline(ab3)

ab4 = lm(z12$y4~z12$x4)
plot(z12$y4~z12$x4)
abline(ab4)

