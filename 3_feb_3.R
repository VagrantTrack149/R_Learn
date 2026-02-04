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

# Ajuste con restricción: fuerza un valor en x_th (continuidad)
r_mS_constrained <- function(x, y, p, x_th, value_th)
{
  stopifnot(length(x) == length(y))
  n <- length(x)
  x_mean <- mean(x)
  x_sd <- sd(x)
  xs <- (x - x_mean)/x_sd
  X <- outer(xs, 0:p, `^`)
  M <- crossprod(X)
  v <- crossprod(X, y)
  a_ls <- solve(M, v)
  t_th <- (x_th - x_mean)/x_sd
  e <- t_th^(0:p)
  # denom = e^T M^{-1} e
  Minv_e <- solve(M, e)
  denom <- as.numeric(crossprod(e, Minv_e))
  correction <- as.numeric((crossprod(e, a_ls) - value_th) / denom)
  a_cons <- a_ls - correction * Minv_e
  return(a_cons)
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

r_mSth <- function(x, y, p, i_th)
{
  vp <- 1:p
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
  
  as <- vr_mS(xs, ys, vp)
  yls <- vs_mS(as, xs)
  R2s <- e_R2(ys, yls)
  R2s_th <- mean(R2s) - qt(0.975, df = p-1) * sd(R2s)/sqrt(p)
  ps_sel <- min(vp[R2s>R2s_th])
  
  as_sel <- as[[ps_sel]]
  ys_sel <- yls[,ps_sel]
  
  # Enforce continuity at x_th: compute left value at x_th (usando la escala del lado izquierdo)
  pi <- length(ai_sel)-1
  ps <- length(as_sel)-1
  x_mean_i <- mean(xi); x_sd_i <- sd(xi)
  t_th_i <- (x_th - x_mean_i)/x_sd_i
  e_i <- t_th_i^(0:pi)
  value_th_left <- as.numeric(crossprod(e_i, ai_sel))
  # Reajustar coeficientes del lado derecho imponiendo e_s^T a_s = value_th_left
  as_sel_cons <- r_mS_constrained(xs, ys, ps, x_th, value_th_left)
  ys_sel_cons <- s_mS(as_sel_cons, xs)
  
  yt <- s_mSt(ai_sel, as_sel_cons, x_th, x)
  R2 <- e_R2(y, yt)
  return(c(pi_sel, ai_sel, ps_sel, as_sel_cons, R2))
}

vr_mS <- Vectorize(r_mS, vectorize.args = "p")
vs_mS <- Vectorize(s_mS, vectorize.args = "a")
vr_mSo <- Vectorize(r_mSo, vectorize.args = "xi")
vr_mSth <- Vectorize(r_mSth, vectorize.args = "i_th")

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
i_th <- (p+1):(n-p-1)
md_th <- vr_mSth(x, y, p, i_th)
R2_th <- sapply(md_th, function(md_th) tail(md_th, 1))
i_sel <- which(R2_th==max(R2_th))
x_th <- x[i_sel+p]
md_sel <- md_th[[i_sel]]
ai_sel <- md_sel[2:(md_sel[1]+2)]
as_sel <- md_sel[md_sel[1]+(0:md_sel[(md_sel[1]+3)])+4]
y_th <- s_mSt(ai_sel, as_sel, x_th, x)

plot(x, y, col = "red", pch = 19)
lines(x, y_sel, col = "blue", type = "l", lwd = 4)
lines(x, y_th, col = "green", type = "l", lwd = 4)
grid()
