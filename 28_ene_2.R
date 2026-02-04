require(stats)

# Archivo de la base de datos
filename = "../ReconocimientoPatrones/data/regresion/Datos_1_1.txt"

data <- read.table(filename, sep = "", header = FALSE)

x <- as.numeric(data[1,])
y <- as.numeric(data[2,])
p <- 2

r_mS <- function(x, y, p)
{
  stopifnot(length(x) == length(y))
  n <- length(x)
  x <- as.numeric(scale(x))
  
  X <- outer(x, 0:p, `^`)
  M <- crossprod(X)
  v <- crossprod(X, y)
  
  return(solve(M, v))
}

s_mS <- function(a, x)
{
  p <- length(a)-1
  n <- length(x)
  
  x <- as.numeric(scale(x))
  
  X <- outer(x, 0:p, `^`)
  return(X %*% a)
}

e_R2 <- function(y, yl)
{
  stopifnot(length(y) == length(yl))
  n <- length(y)
  
  SSE <- sum((y-yl)^2)
  s2y <- n*var(y)
  return(1-SSE/s2y)
}

a <- r_mS(x, y, p)
yl <- s_mS(a, x)
R2 <- e_R2(y, yl)
print(R2)

plot(x, y, col = "red", pch = 19)
lines(x, yl, col = "blue", type = "p", lwd = 4)
grid()

