#r2-zad5
dev.new()
png("wykresy.png")
f1 = function(x)
  x**2
f2 = function(x)
  (x-2)**2
f3 = function(x)
  (x-2)**2 + 3
f4 = function(x)
  x**2 + 3
f5 = function(x)
  (x + 1)**2 - 2

curve(f1, -5, 5, col = "blue", main="Wykresy funkcji przesuniętych")
curve(f2, -5, 5, add = TRUE, col = "green")
curve(f3, -5, 5, add =TRUE, col = "yellow")
curve(f4, -5, 5, add = TRUE, col = "magenta")
curve(f5, -5, 5, add = TRUE, col = "cyan")
abline(h = 0, v = 0, col = "black")
points(c(f1(0.8),
         f2(0.8),
         f3(0.8),
         f4(0.8),
         f5(0.8)),
       c(f1(-0.2),
         f2(-0.2),
         f3(-0.2),
         f4(-0.2),
         f5(-0.2)))
legend(0,20, 
       legend = c("x**2", "(x-2)**2", "(x-2)**2 + 3", "x**2 + 3", "(x + 1)**2 - 2"),
       col = c("blue", "green", "yellow", "magenta", "cyan"), 
       lty = 1)

dev.off()

