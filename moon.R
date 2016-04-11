
#setwd("C:/Users/WIN/Desktop/Tarea_3Clustering/AprendizajeNoSupervisado-master/AprendizajeNoSupervisado-master")

#Cargamos los datos
datos = read.csv(file = "moon.csv",
                 header = F, 
                 row.names = NULL)

#_______________________________________________
#K-medias:
#Analizamos los datos dentro del dataset
head(datos)

#Queremos conocer cuantos individuos de cada clase tenemos
#en este caso tenemos 1000 de cada clase
#Por los datos se clasifican en 0.0 , 1.0 ó 2.0
table(datos$V3)

#Graficamos el dataset a.csv con colores para cada clase
plot(datos$V1,
     datos$V2,
     col = datos$V3,
     xlab = "X",
     ylab = "Y",
     main = "Clustering Rectangular")

#usando el modelo de k-medias con k=2 ya que en V3 solo estan los valores 1 ó 0
modelo.k.medias = kmeans(x = datos[, c("V1", "V2")],
                         centers = 2)

#datos originales con colores de cluster
plot(x = datos$V1,
     y = datos$V2,
     col = modelo.k.medias$cluster)

# Ahora graficamos los puntos
points(x = modelo.k.medias$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)

#Para ver el rendimientos del modelo Kmedia usamos la matriz de confusión
#Como se observa se clasificaron 1000->0, 1000->1, 999->2
matrizK= table(modelo.k.medias$cluster, datos$V3)
#matrizK

#Medimos la exactitud del metodo kmedias:
kmedias=sum(diag(matrizK))/sum(matrizK)
#kmedias


#_________________________________________________
#Cluster Jerarquico:
# Copiamos el dataset en una variable nueva
entrada = datos

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada$V3 = NULL

# De DataFrame a Matrix
entrada = as.matrix(entrada)

# Matriz de distancia
distancia = dist(entrada)

#Hclust:
# Primero usamos en metodo complete

metodo = "complete"
cluster = hclust(distancia, method = metodo)
plot(cluster)
corte = cutree(cluster, k=2)
head(corte)

plot(x = datos$V1,
     y = datos$V2,
     col = corte)

unique(corte)

matrizC=table(datos$V3,corte)
#matrizC

#Medimos la exactitud del metodo Complete:
complete = sum(diag(matrizC))/sum(matrizC)
#complete


#Usamos en metodo single
metodo = "single"
cluster = hclust(distancia, method = metodo)
plot(cluster)
corte = cutree(cluster, k=2)
head(corte)

plot(x = datos$V1,
     y = datos$V2,
     col = corte)

unique(corte)

matrizS=table(datos$V3,corte)
#matrizS

#Medimos la exactitud del metodosingle:
single = sum(diag(matrizS))/sum(matrizS)
#single


#Usamos en metodo Average
metodo = "average"
cluster = hclust(distancia, method = metodo)
plot(cluster)
corte = cutree(cluster, k=2)
head(corte)

plot(x = datos$V1,
     y = datos$V2,
     col = corte)

unique(corte)

matrizAvg=table(datos$V3,corte)
#matrizAvg

#Medimos la exactitud del metodo average:
average = sum(diag(matrizAvg))/sum(matrizAvg)
#average


#kmedias
#complete
#single
#average

#El que Clasificó a perfección el dataset fue el Metodo Sigle con exactitud de 100% 
# el que le sigue es Complete con 0.836, average y kmedias al contrario no lograron clasificar
# bien nisiquiera el 50% del data set con 0.244 y 0.266 respectivamente
