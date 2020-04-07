#Para gráficos se utiliza la librería ggplot2
library(ggplot2)
#GGBARPLOT se utiliza cuando se analizan variables categóricas (una o más)

df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
print(df)
#Esta es la manera "simple" de representar el gráfico
grafico1<-ggbarplot(df,"dose","len")
show(grafico1)
#El resto de los parámetros que pueden ser agregados son:
#label = TRUE (muestra la frecuencia de cada variable sobre su respectiva barra)
#lab.pos = "in", lab.col = "white" Si está label como TRUE pone dentro de la barra a los valores y le asigna color
#width = entre 0 y 1, indica el ancho de las barras
#orientation = "horiz" Cambia la orientación de los ejes
#fill = color que toma la barra
#color = color de la orilla de la barra
#https://rpkgs.datanovia.com/ggpubr/reference/ggbarplot.html 




