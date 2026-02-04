
filename= "../ReconocimientoPatrones/data/sesgo/caso_sesgo_genero_admision_2000_2024_programa_anio.csv"

Data<-read.csv(filename)
year<-unique(Data[1])
print(year)
Facultades<-unique(Data[2])
print(Facultades)
carreras<-unique(Data[3])
print(unique(Data[3]))

data_simplificada<-c(carreras,Facultades,year) #Lista

