#Ejemplo R
#ls() en la consola lista los objetos en memoria.
#La función  muestra algunos detalles de los objetos en memoria
#Para hacer print en solo un elemento de una estructura de datos se una nombreEstructura$nombreDeLaFilaOColumna

###################################################################################################################
#IF
nota <- 7
if(nota >= 4) {
  print("Aprobado")
} else {
  print("Reprobado")
}
#Si se usa if para una lista, vector, etc (estructura de datos mayor a 1) solo se evaluará el primer elemento
#es por esto que existe otra forma de realizarlo:
#ifelse(vector, valor_si_TRUE, valor_si_FALSE)

ifelse(ejemplo_vector>3,"TRUE","FALSE")


###################################################################################################################
#FOR y WHILE
#Los for y while no son muy utilizados en R debido a que muchas operaciones en vectores pueden ser realizados de forma 
#inmediata, por lo que dejan de ser necesarios.
#for(algo in algos){
#
#}
###################################################################################################################
#suma de los elementos
suma <- sum(ejemplo_vector) 

###################################################################################################################
#producto de los elementos de x
producto <- prod(ejemplo_vector) 

###################################################################################################################
#Valor max o min en el objeto
maximo <- max(ejemplo_vector) 
minimo <- min(ejemplo_vector) 

###################################################################################################################
#Índice del valor max o min
indice_max <- which.max(ejemplo_vector) 
indice_min <- which.min(ejemplo_vector) 

###################################################################################################################
#rango del objeto o c(min(x), max(x))
rango <- range(ejemplo_vector)

###################################################################################################################
#Cantidad de elementos del objeto
largo <- length(ejemplo_vector) 

###################################################################################################################
#promedio de los elementos de x
media <- mean(ejemplo_vector) 

###################################################################################################################
#mediana de los elementos de x
mediana <- median(ejemplo_vector) 

###################################################################################################################
#var(x) o cov(x) varianza de los elementos de x (calculada en n−1); si x es una matriz o un marco
#de datos, se calcula la matriz de varianza-covarianza

###################################################################################################################
#cor(x) matriz de correlacion de ´ x si es una matriz o un marco de datos (1 si x es un vector)

###################################################################################################################
#var(x, y) o cov(x, y) covarianza entre x y y, o entre las columnas de x y y si son matrices o marcos de
#datos

###################################################################################################################
#cor(x, y) correlacion lineal entre ´ x y y, o la matriz de correlacion si ´ x y y son matrices o
#marcos de datos
###################################################################################################################

###################################################################################################################

###################################################################################################################

###################################################################################################################

#Genera una lista de 100 valores aleatorios entre 0 y 20 (de una distribución uniforme)

distribucion_normal <- runif(100,0,20)

###################################################################################################################
#Summary entrega datos sobre el arreglo entregado (min, max, media, mediana, etc).
resultados <-summary(distribucion_normal)

