#r2-zad3
png("wykres1.png")
curve((x+1)/(x**2-4), from = -5, to = 5)
dev.off()

png("wykres2.png")
curve((x+1)/(x**2-4), from = -5, to = 5)
abline(h = 0, v = 0)
#abline(v = c(-2,2))
dev.off()

png("wykres3.png")
curve((x+1)/(x**2-4), from = -5, to = 5)
abline(h = 0, v = 0)
abline(v = c(-2,2))
dev.off()

