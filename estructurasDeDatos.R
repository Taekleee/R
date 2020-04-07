###################################################################################################################
#DATA FRAME: estructura de datos de dos dimensiones que acepta diferentes tipos, a diferencia de las matrices
#que solo aceptan 1.
#
#FILAS: representan casos
#COLUMNAS: representan atributos
#

ejemplo_dataframe <- data.frame(
  "Edad" = c(20,21,22,23,24),
  "Peso" = c(100,80,90,23,32),
  "Estatura" = c(172,176,189,158,123)
)

#Para recorrer las columnas del dataframe se ocupa:

columna <- ejemplo_dataframe[,1]


#Para recorrer filas se ocupa:

fila <- ejemplo_dataframe[1,]

###################################################################################################################
#Vector
#Cualquier operación usada es aplicada a todos los elementos del vector de forma inmediata
ejemplo_vector<- c(1,2,3,4)
ejemplo2_vector<-c("Hola","Mundo")

###################################################################################################################
#Matrices 
#Se pueden crear de la manera en que sale abajo o como una unión de vectores.
#Para lo segundo es necesario usar cbind() o rbin(), en donde: 
#cbind() para unir vectores, usando cada uno como una columna.
#rbind() para unir vectores, usando cada uno como una fila.
#vectores y matrices pueden contener NA

ejemplo_matriz <- matrix(1:12, nrow = 3, ncol = 4)

vector_matriz<-c(1,2,3)
vector_matriz1<-c(3,4,5)
vector_matriz2<-c(5,6,7)

ejemplo_matriz1 <-cbind(vector_matriz,vector_matriz1,vector_matriz2)
ejemplo_matriz2 <-rbind(vector_matriz,vector_matriz1,vector_matriz2)

#Para transponer una matriz: 

ejemplo_transpuesta <-t(ejemplo_matriz)

###################################################################################################################
#LISTAS
#Son unidimensionales, pero pueden tener datos de distinto tipo y dimensiones.

ejemplo_lista <- list("lista_matriz" = ejemplo_matriz, "lista_vector"= ejemplo_vector)

###################################################################################################################
#"CASTEO" de estructuras de datos
#Función	Coerciona a	Coerciona exitosamente a
#as.vector()	   | Vector	  |  Matrices
#as.matrix()	   | Matrices	|  Vectores, Data frames
#as.data.frame()| Data     |  frame	Vectores, Matrices
#as.list()	     | Lista	  |   Vectores, Matrices, Data frames 

ejemplo_cast_vector_matriz<-as.vector(ejemplo_matriz)