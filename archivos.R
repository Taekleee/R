###################################################################################################################
#ARCHIVOS
#getwd indica el directorio en el cual se encuentra actualmente
directorio<-getwd()

#setwd cambia el directorio
nuevo.directorio<-setwd("carpeta")

#Lista los directorios
list.dirs()

#Lista los archivos presentes en el directorio
list.files()

#CSV
read.csv(m, header = TRUE, sep = ",",quote = "\"",,fill=T)
