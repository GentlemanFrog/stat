A = c(6.7, 7.3, 8.0, 8.0, 7.9, 9.2, 10.1)
B = c(7.5, 7.7, 7.7, 8.2, 8.9, 8.9, 10.6)
C = c(5.9, 6.9, 7.0, 7.0, 9.5, 9.6, 9.6)
df = data.frame(A, B, C)
df
# dla kolumn
mean(df$A)
mean(df$B)
mean(df$C)
# dla kolumne 2 sposob
colMeans(df)
# dla wierszy
rowMeans(df)
