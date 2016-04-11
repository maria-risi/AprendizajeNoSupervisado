#setwd("C:/Users/WIN/Desktop/Tarea_3Clustering/AprendizajeNoSupervisado-master/AprendizajeNoSupervisado-master")


#Cargamos los datos
datos = read.csv(file = "help.csv", header = F, row.names = NULL)

#para visualizar la data
#datos

#Analizamos los datos dentro del dataset
head(datos)

table(datos$V4)

#los valores de datos$V$ van desde -4.711 a 14.13
#por esto defino clases en los rangos siguientes >0,>4,>8 y mayores =4 clases
#min(datos$V4)  
#max(datos$V4)



# Ejemplo de regla para asignar clases
definir_clase = function(numero){
  #suponemos 4 clases  de -4 a 15
  # Recuerde que, en el caso de R, cada número es relativo a un color.
  if(numero < -4.0)
    return(1)
  else if(numero < 0.0)
    return(2)
  else if(numero < 4.0)
    return(3)
  else if(numero <8.0)
    return(4)
  else
    return(5)

}


for (x in 1:nrow(datos)) {
  datos$V4[x] = definir_clase(datos$V4[x])
}

table(datos$V4)
plot(datos[,1:3], col = datos$V4)
scatterplot3d(datos$V1, datos$V2, datos$V3,color = datos$V4)
#----- F FUNCION DE ASIGNAR CLASES ---#

#----- KMEDIAS ---#
modelo_kmedias= kmeans(x = datos[, 1:3],
                      centers = 5)

#Matriz de Confusion
matrizK=table(modelo_kmedias$cluster, datos$V4)

#Exactitud del metodo kMEdias:
kmedias = sum(diag(matrizK))/sum(matrizK)

#________________________________________________
#Cluster Jerarquico:

# Copiamos el dataset en una variable nueva
entrada.num = datos

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada.num$V4 = NULL

# De DataFrame a Matrix
entrada.num = as.matrix(entrada.num)

# Matriz de distancia, ?dist para otras opciones distinta a norma 2
distancia = dist(entrada.num, diag = FALSE, upper = TRUE, p=6)

# Método Complete 
metodo = "complete"
cluster = hclust(distancia, method = metodo)

#graficamos el cluster
plot(cluster)
#graficamos el cluster si lo queremos con nombres de coordenadas distintos
plot(cluster,
     xlab = "Ramas",
     ylab = "Y",
     main = "Dendograma")

# Verifiquemos el dendrograma
dendrogram = as.dendrogram(cluster)

#si queremos observar la cantidad de cada clase
table(datos$V4)

#calculamos la matriz de confusión
corte = cutree(cluster, k=5)


matrizC=table(datos$V4,corte)
#matrizC

#Medimos la exactitud del metodo Complete:
complete = sum(diag(matrizC))/sum(matrizC)
#complete

#aplicamos cortes si deseamos ver menor cantidad de ramas
#en este caso se veran solo aquellas ramas superiores a 10
cortes = cut(dendrogram, h = 20)$upper
plot(cortes,
     xlab = "Ramas",
     ylab = "Y",
     main = "Dendograma")

#_____________

# Luego usamos en metodo single

metodo = "single"
cluster = hclust(distancia, method = metodo)
plot(cluster)
corte = cutree(cluster, k=5)
head(corte)

plot(datos[,1:3],
     col = corte)

matrizS=table(datos$V4,corte)
single= sum(diag(matrizS))/sum(matrizS)


#_____USando metodo average

metodo = "average"
cluster = hclust(distancia, method = metodo)
plot(cluster)
corte= cutree(cluster, k=5)
head(corte)

plot(datos[,1:3],
     col = corte)

matrizAvg =table(datos$V4,corte)
average = sum(diag(matrizAvg))/sum(matrizAvg)

kmedias
complete
single
average