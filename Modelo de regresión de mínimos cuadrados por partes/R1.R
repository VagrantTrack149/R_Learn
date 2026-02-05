
##Lectura de archivos
path="D:/Reconocimiento de patrones/Codigo/Modelo de regresión de mínimos cuadrados por partes/"

filename_1="Datos_1_3.txt"
filename_2="Datos_1_4.txt"
filename_3="Datos_1_5.txt"


Data1= read.table(paste0(path,filename_1))
Data2= read.table(paste0(path,filename_2))
Data3= read.table(paste0(path,filename_3))

Datax1 = sort(as.numeric(Data1[1,]))
Datay1 = as.numeric(Data1[2,])


Datax2 = sort(as.numeric(Data2[1,]))
Datay2 = as.numeric(Data2[2,])

Datax3 = sort(as.numeric(Data3[1,]))
Datay3 = as.numeric(Data3[2,])

##GRAFICO 


#points(Datax1, Datay1, col = "red", pch = 16, main="Gráfico de Datos 1_3")
#points(Datax2, Datay2, col = "blue", pch = 16, main="Gráfico de Datos 1_4")
#points(Datax3, Datay3, col = "green", pch = 16, main="Gráfico de Datos 1_5")

#all_x = c(Datax1, Datax2, Datax3)
#all_y = c(Datay1, Datay2, Datay3)
#plot(Datax1, Datay1, col = "red", pch = 16, 
#     xlim = range(all_x), ylim = range(all_y),
#     main = "Comparación de Modelos", xlab = "X", ylab = "Y")
#grid()
#points(Datax2, Datay2, col = "blue", pch = 16)
#points(Datax3, Datay3, col = "green", pch = 16)

#legend("topleft", legend=c("Datos 1_3", "Datos 1_4", "Datos 1_5"),
#       col=c("red", "blue", "green"), pch=16)

#Tratamiento de datos
#REVISAAAAR COMPROBAR
#######################
p <- 18
n = length(Datax1)
ix = order(Datax1)
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
a <- vr_mS(Datax1, Datay1, vp)
yl <- vs_mS(a, Datax1)
R2 <- e_R2(Datay1, yl)

R2_th <- mean(R2) - qt(0.975, df = p-1) * sd(R2)/sqrt(p)
p_sel <- min(vp[R2>R2_th])

a_sel <- a[[p_sel]]
y_sel <- yl[,p_sel]

R2o <- vr_mSo(Datax1, Datay1, p_sel, Datax1)
R2o_th <- mean(R2o) + qt(0.995, df = n-1) * sd(R2o)/sqrt(n)
id_o <- which(R2o>R2o_th)

plot(Datax1, Datay1, col = "red", pch = 19)
lines(Datax1[ix], y_sel[ix], col = "blue", type = "l", lwd = 4)
points(Datax1[id_o], Datay1[id_o], col = "blue", pch = 1, cex = 2, lwd = 4)
grid()




