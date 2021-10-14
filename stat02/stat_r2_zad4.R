#r2-zad4
png("sincos1.png")
curve(sin, from = -3*pi, to = 3*pi)
curve(cos, from = -3*pi, to = 3*pi, add = TRUE, col = "red")
dev.off()
