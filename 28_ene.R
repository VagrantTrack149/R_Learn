# Archivo de la base de datos
filename = "../ReconocimientoPatrones/data/regresion/Datos_1_1.txt"

data <- read.table(filename, sep = "", header = FALSE)

x <- as.numeric(data[1,])
y <- as.numeric(data[2,])

n <- length(x)
Sx <- sum(x)
Sy <- sum(y)
Sxy = sum(x*y)
Sx2 <- sum(x^2)

M = matrix(c(Sx, Sx2, n, Sx), nrow = 2, ncol = 2)
v = matrix(c(Sy, Sxy), nrow = 2, ncol = 1)
a <- solve(M, v)
Mx <- matrix(c(x, rep(1, n)), nrow = n, ncol = 2)
yl <- Mx %*% a

SSE <- sum((y-yl)^2)
s2y <- n*var(y)
R2 <- 1-SSE/s2y
print(R2)

plot(x, y, col = "red", pch = 19)
lines(x, yl, col = "blue", lwd = 4)
grid()

