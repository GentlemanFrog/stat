#zad1
library(drc)
library(flexmix)
library(stats)
library(ggplot2)
library(dplyr)
curves = read.csv("krzywe.csv", sep=",")
curves.mm = drm(v ~ S, data=curves, fct=MM.2())

#plot(curves)
#mml <- data.frame(S = seq(0, max(curves$S), length.out = 100))
#mml$v <- predict(curves.mm, newdata = mml)
summary(curves.mm)
#p = ggplot(curves, aes(S, v, color = v)) + geom_point()
#p + geom_smooth(aes(S, predict(curves.mm)))
plot(curves)
#plot(curves.mm, add = TRUE)

# TODO: DELETe
#mselect(curves.mm, list(MM.3(), logistic(), l2()), icfct = AIC)

### 

lines(sort(curves$S), sort(predict(curves.mm)))


curves.nls = nls(v ~ Vm * S / (K + S), data = curves, start = list(K=max(curves$v), Vm=max(curves$v)))
summary(curves.nls)
plot(curves)
#sortownaie tutaj
lines(sort(curves$S), sort(predict(curves.nls)))

#zad2
woods = read.csv("Woods.csv", sep=",")
woods2 = data.frame(woods$Plon, woods$Tydzien)
colnames(woods2) = c("plon", "Tydzien")
woods.nls = nls(plon ~ a * Tydzien^b * exp(-c*Tydzien), data=woods2, start = list(a=1,b=1,c=0.01))
plot(woods2$Tydzien, woods2$plon)
lines(woods2$Tydzien, predict(woods.nls))

#zad3
d1 = read.csv("dataset1.csv", sep = ",")
d2 = read.csv("dataset2.csv", sep=",")

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

#zad4
ryegrass
ryegrass.LL = drm(rootl ~ conc, data = ryegrass, fct = LL.2())
summary(ryegrass.LL)
plot(ryegrass.LL)

ryegrass.weibull = drm(rootl ~ conc, data = ryegrass, fct = weibull2())
summary(ryegrass.weibull)
plot(ryegrass.weibull)

AIC(ryegrass.LL, ryegrass.weibull)
BIC(ryegrass.LL, ryegrass.weibull)
