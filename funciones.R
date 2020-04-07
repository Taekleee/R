###################################################################################################################
#FUNCIONES
#Definir una función:
#nombre <- function(argumentos) {
#operaciones
#}
#No es necesario retornar en la función, ya que solo basta con poner el nombre de una variable como expresión final.
# La expresión return(invisible(x)) devuelve el contenido de la variable x pero no lo escribe en pantalla.ç
#Los argumentos de una función pueden ser agregados por sus nombres (ej: nombre = "nombre") o según la posición
#en la que se deben encontrar. 

ejemplo_funcion <- function(
  num1,
  num2
) {
  resultado <- num1 + num2
  print(resultado)
}

###################################################################################################################
#FUNCIÓN ANÓNIMA EN R
#Son útiles cuando solo se deben ocupar una sola vez y no tienen nombre
#Junto con sapply aplica de manera inmediata a todo el vector la función

funcion.anonima <- sapply(1:10, function(x) if(x < 5) x^2 else -x^2)
