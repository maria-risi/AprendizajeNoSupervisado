
#setwd("C:/Users/WIN/Desktop/Tarea_3Clustering/AprendizajeNoSupervisado-master/AprendizajeNoSupervisado-master")

#Cargar los datos
datos = read.csv(file = "good_luck.csv", header = F, row.names = NULL)


#Analizamos los datos dentro del dataset
head(datos)

# Copiamos el dataset en una variable nueva
entrada.num = datos

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada.num$V11 = NULL

# De DataFrame a Matrix
entrada.num = as.matrix(entrada.num)

#Calculamos la matriz de distancia
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

# Verifiquemos el dendrograma
dendrogram = as.dendrogram(cluster)

#si queremos observar la cantidad de cada clase
table(datos$V11)

#calculamos la matriz de confusión
corte = cutree(cluster, k=2)
table(datos$V11, corte)

#aplicamos cortes para ver las ramas superiores desde la rama 6
cortes = cut(dendrogram, h = 6)$upper
plot(cortes,
     xlab = "Ramas",
     ylab = "Y",
     main = "Dendograma")

