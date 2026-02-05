require(graphics)
path_informacion="../ReconocimientoPatrones/data/a-pressure-map-dataset-for-in-bed-posture-classification-1.0.0/experiment-i/"
file_name="subject-info-i.csv"

fileinfo= read.csv(paste(path_informacion,file_name, sep=""))

index_paciente=1;
index_posicion=1;
v_max=1000;
nc=32
nr=64

n_frame=3;

file_pos= paste(path_informacion,"s",fileinfo$Subject.Number[index_paciente],"/",index_posicion,".txt",sep="")
print(file_pos)
data= read.table(file_pos,header = FALSE, sep = "\t")
dm= dim(data)
nf= dm[1]
data_sel=as.numeric(data[n_frame, 1:(nc*nr)])
frame= matrix(data_sel,nrow=nr, ncol=nc, byrow = TRUE)

image(frame, col=palette("R4"))
pos_outlier= which(frame>v_max)
pos_out_re= pos_outlier%%nr
pos_out_re[pos_out_re==0]=nr
pos_out_col=(pos_outlier-pos_out_re)/nr+1
x=frame[pos_out_re,pos_out_col+-3:3]
ns=length(x)
x= x[rep(c(TRUE,FALSE),4)]
pos_out_estimado=spline(x,n=ns)
i_=ceiling(ns/2)
