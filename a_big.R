
#setwd("C:/Users/WIN/Desktop/Tarea_3Clustering/AprendizajeNoSupervisado-master/AprendizajeNoSupervisado-master")

#Cargamos los datos 
datos = read.csv(file = "a_big.csv", header = F, row.names = NULL)

#para visualizar la data
#datos

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
     xlim = c(min(datos$V1-0.5), max(datos$V1+0.5)),
     ylim = c(min(datos$V2-0.5), max(datos$V2+0.5)),
     xlab = "X",
     ylab = "Y",
     main = "Clustering Rectangular")

#usando el modelo de k-medias
modelo.k.medias = kmeans(x = datos[, c("V1", "V2")],
                         centers = 2)

#datos originales con colores de cluster
plot(x = datos$V1,
     y = datos$V2,
     col = modelo.k.medias$cluster)

# Ahora graficamos los puntos
points(x = modelo.k.medias$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 2)


#Para ver el rendimientos del modelo usamos la matriz de confusión
#Como se observa se clasificaron 99405->0, 99401->1, 98745->2 
table(modelo.k.medias$cluster, datos$V3)
