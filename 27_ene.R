# Archivo de la base de datos
filename = "../ReconocimientoPatrones/data/regresion/Datos_1_1.txt"

data = read.table(filename, sep="", header=FALSE)

x<-as.numeric(data[1,])
y<- as.numeric(data[2,])

n<- length(x)
Sx<- sum(x)
Sy<- sum(y)

Sxy <- sum(x*y)
Sx2 <- sum(x^2)
#m
a1<- (n*Sxy - Sx*Sy)/(n*Sx2 - Sx^2) 
#b
a0<-(Sy-a1*Sx)/n



yL<- a1*x+a0
x1<-seq(min(x),max(x), length.out=10)
y1<-a1*x1+a0

#error normal
SSE<- sum((y-yL)^2)
#varianza de Y
Vy=n*var(y)
#Indice para saber si esta bien o no
R2=1-SSE/Vy

plot(x,y, col= "red", pch=19)
lines(x1,y1, col="blue")

