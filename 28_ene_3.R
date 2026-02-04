require(stats)
# Archivo de la base de datos
filename = "../ReconocimientoPatrones/data/regresion/Datos_1_1.txt"

data <- read.table(filename, sep = "", header = FALSE)

x <- as.numeric(data[1,])
y <- as.numeric(data[2,])
p <- 15

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
  n <- length(y)
  
  yl <- as.matrix(yl)
  stopifnot(nrow(yl) == n)
  
  s2y <- n * var(y)
  SSE <- colSums((yl - y)^2)
  return(1 - SSE / s2y)
}

vr_mS <- Vectorize(r_mS, vectorize.args = "p")
vs_mS <- Vectorize(s_mS, vectorize.args = "a")

vp <- 1:p
a <- vr_mS(x, y, vp)
yl <- vs_mS(a, x)
R2 <- e_R2(y, yl)
print(R2)

R2_th <- mean(R2) - qt(0.975, df = p-1) * sd(R2)/sqrt(p)
p_sel <- min(vp[R2>R2_th])
print(p_sel)

plot(vp, R2, col = "red", pch = 19, type = "b", lwd = 2)
points(p_sel, R2[p_sel==vp], col = "blue", pch = 1, cex = 2, lwd = 4)
grid()

