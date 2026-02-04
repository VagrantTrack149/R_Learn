require(stats)

# Archivo de la base de datos
filename = "../ReconocimientoPatrones/data/regresion/Datos_1_3.txt"


data <- read.table(filename, sep = "", header = FALSE)

x <- as.numeric(data[1,])
ix <- order(x)
x<-x[ix]
n <- length(x)
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

r_mSo <- function(x, y, p, xi)
{
  stopifnot(xi %in% x)
  ix <- x != xi
  
  x <- x[ix]
  y <- y[ix]
  
  a <- r_mS(x, y, p)
  yl <- s_mS(a, x)
  R2 <- e_R2(y, yl)
  return(R2)
}

vr_mS <- Vectorize(r_mS, vectorize.args = "p")
vs_mS <- Vectorize(s_mS, vectorize.args = "a")
vr_mSo <- Vectorize(r_mSo, vectorize.args = "xi")

vp <- 1:p
a <- vr_mS(x, y, vp)
yl <- vs_mS(a, x)
R2 <- e_R2(y, yl)

R2_th <- mean(R2) - qt(0.975, df = p-1) * sd(R2)/sqrt(p)
p_sel <- min(vp[R2>R2_th])

a_sel <- a[[p_sel]]
y_sel <- yl[,p_sel]

i_th= 40 #p+1
xi= x[1:i_th]
yi= y[1:i_th]

xs= x[i_th:n]
ys= y[i_th:n]
x_th = x[i_th]

ai <- vr_mS(xi, yi, vp)
yli <- vs_mS(ai, xi)
R2i <- e_R2(yi, yli)


as <- vr_mS(xs, ys, vp)
yls <- vs_mS(as, xs)
R2s <- e_R2(ys, yls)


#R2o <- vr_mSo(x, y, p_sel, x)
#R2o_th <- mean(R2o) + qt(0.995, df = n-1) * sd(R2o)/sqrt(n)
#id_o <- which(R2o>R2o_th)

plot(x, y, col = "red", pch = 19)
lines(xi, yi, col = "blue", type = "l", lwd = 4)
lines(xs, ys, col="green", type="l", lwd=4)
#points(x[id_o], y[id_o], col = "blue", pch = 1, cex = 2, lwd = 4)
grid()
