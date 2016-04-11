
#setwd("C:/Users/WIN/Desktop/Tarea_3Clustering/AprendizajeNoSupervisado-master/AprendizajeNoSupervisado-master")

#Cargamos los datos
datos = read.csv(file = "h.csv", header = F, row.names = NULL)

#para visualizar la data
#datos

#Analizamos los datos dentro del dataset
head(datos)

#table(datos$V4)

plot(datos[,1:3], col = datos$V4)

scatterplot3d(datos$V1, datos$V2, datos$V3,color = datos$V4)

#Suponemos que son 7 clases debido a que se pueden observar 7 colores en las graficas

# Ejemplo de regla para asignar clases
definir_clase = function(numero){
  # Recuerde que, en el caso de R, cada número es relativo a un color.
  if(numero < 5.0)
    return(1) #clase1
  else if(numero < 6.0)
    return(2) #clase2
  else if(numero < 7.0)
    return(3) #clase3
  else if(numero < 8.0)
    return(4) #clase4
  else if(numero < 9.0)
    return(5) #clase5
  else if(numero < 10.0)
    return(6) #clase6
  else 
    return(7) #clase7
}

array_origen <- array(datos$V4)

datos$V4 = array_destino

# Copiamos el dataset en una variable nueva
entrada.num = datos

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada.num$V4 = NULL

# De DataFrame a Matrix
entrada.num = as.matrix(entrada.num)

# Matriz de distancia, ?dist para otras opciones distinta a norma 2
distancia = dist(entrada.num, diag = FALSE, upper = TRUE, p=6)

# Método por defecto es complete link
metodo = "complete"
cluster = hclust(distancia, method = metodo)

#graficamos el cluster
plot(cluster)
#graficamos el cluster si lo queremos con nombres de coordenadas distintos
plot(cluster,
     xlab = "Ramas",
     ylab = "Y",
     main = "Dendograma")


#obtenemos el arreglo de clases
for (i in 1:nrow(datos)) {
  array_destino[i] <- definir_clase(array_origen[i])
}

#si deseamos ver la clase destino imprimimos
#clase_destino
# Verifiquemos el dendrograma
dendrogram = as.dendrogram(cluster)

#si queremos observar la cantidad de cada clase
table(datos$V4)

#calculamos la matriz de confusión
corte = cutree(cluster, k=7)


matrizC=table(datos$V4,corte)
#matrizC

#Medimos la exactitud del metodo Complete:
complete = sum(diag(matrizC))/sum(matrizC)
#complete
                                  
#aplicamos cortes si deseamos ver menor cantidad de ramas
#en este caso se veran solo aquellas ramas superiores a 10
cortes = cut(dendrogram, h = 10)$upper
plot(cortes,
     xlab = "Ramas",
     ylab = "Y",
     main = "Dendograma")

