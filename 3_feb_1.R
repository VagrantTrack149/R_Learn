require(stats)
# Archivo de la base de datos
filename = "../ReconocimientoPatrones/data/regresion/Datos_1_3.txt"

data <- read.table(filename, sep = "", header = FALSE)

x <- as.numeric(data[1,])
y <- as.numeric(data[2,])
ix <- order(x)
x <- x[ix]
y <- y[ix]
n <- length(x)
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

s_mSt <- function(ai, as, th, x)
{
  i_th <- x<th
  xi <- x[i_th]
  xs <- x[!i_th]
  
  pi <- length(ai)-1
  ps <- length(as)-1
  ni <- length(xi)
  ns <- length(xs)
  
  xi <- as.numeric(scale(xi))
  xs <- as.numeric(scale(xs))
  
  Xi <- outer(xi, 0:pi, `^`)
  Xs <- outer(xs, 0:ps, `^`)
  yi <- Xi %*% ai
  ys <- Xs %*% as
  return(c(yi, ys))
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

# Modelo completo
vp <- 1:p
a <- vr_mS(x, y, vp)
yl <- vs_mS(a, x)
R2 <- e_R2(y, yl)

R2_th <- mean(R2) - qt(0.975, df = p-1) * sd(R2)/sqrt(p)
p_sel <- min(vp[R2>R2_th])

a_sel <- a[[p_sel]]
y_sel <- yl[,p_sel]
R2_sel <- R2[p_sel]

# Modelo en partes
i_th <- 30 # p+1:n-p-1

xi <- x[1:i_th]
yi <- y[1:i_th]

xs <- x[i_th:n]
ys <- y[i_th:n]

x_th <- x[i_th]

ai <- vr_mS(xi, yi, vp)
yli <- vs_mS(ai, xi)
R2i <- e_R2(yi, yli)
R2i_th <- mean(R2i) - qt(0.975, df = p-1) * sd(R2i)/sqrt(p)
pi_sel <- min(vp[R2i>R2i_th])
ai_sel <- ai[[pi_sel]]
yi_sel <- yli[,pi_sel]
R2i_sel <- R2i[pi_sel]

as <- vr_mS(xs, ys, vp)
yls <- vs_mS(as, xs)
R2s <- e_R2(ys, yls)
R2s_th <- mean(R2s) - qt(0.975, df = p-1) * sd(R2s)/sqrt(p)
ps_sel <- min(vp[R2s>R2s_th])
as_sel <- as[[ps_sel]]
ys_sel <- yls[,ps_sel]
R2s_sel <- R2s[ps_sel]

yt <- s_mSt(ai_sel, as_sel, x_th, x)
R2t <- e_R2(y, yt)

plot(x, y, col = "red", pch = 19)
lines(x, y_sel, col = "blue", type = "l", lwd = 4)
lines(x, yt, col = "green", type = "l", lwd = 4)
grid()
