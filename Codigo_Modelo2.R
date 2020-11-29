
# install.packages("osmdata") #librerías de Open Street Map
# install.packages("ggmap") # Librería de GGPlot2 para pintar mapas
# install.packages("sp")  # Librerias espaciales
# install.packages("tspmeta")

library(ggplot2)
library(tidyverse)
library(dbplyr)


# Cambiamos el directorio
setwd("~/Documentos/A005 Ciencia de Datos/A002 Reto 2020 Datacup/reto-mexico-2020-main")

# Salvamos los datos
save.image("~/Documentos/A005 Ciencia de Datos/A002 Reto 2020 Datacup/data_info.RData")

# Comando para leer nuestro RData

load("~/Documentos/A005 Ciencia de Datos/A002 Reto 2020 Datacup/data_info.RData")

# Leamos el archivo
# data/ubicaciones.csv


data=read.csv(file = "data/Scaled_Data.csv",
              header = TRUE)




# Vamos a ver el detalle de la informacion
str(data) #Nos muestra los cambos y el tipo de informacion contiene
class(data)  # que tipo de data es 




# Aqui leamos la parte de los datos
head(data)


# estadistica basica
summary(data$Frecuencia)
summary(data$Vol_Entrega)


# Hacer un histograma
hist(data$Frecuencia,
     breaks = 10,
     col = "darkgray",
     main = "Histograma de Frecuencia",
     ylab = "Frecuencia",
     xlab = "Dato de Frecuencia")


hist(data$Vol_Entrega,
     breaks = 30,
     col = "darkgray",
     main = "Histograma de Vol_Entrega",
     ylab = "Frecuencia",
     xlab = "Dato de Vol_Entrega")


hist(data$Cart_x,
     breaks = 30,
     col = "darkgray",
     main = "Histograma de Cart_x",
     ylab = "Frecuencia",
     xlab = "Dato de Cart_X")

hist(data$Cart_y,
     breaks = 30,
     col = "darkgray",
     main = "Histograma de Cart_Y",
     ylab = "Frecuencia",
     xlab = "Dato de Cart_Y")




# Vamos a dividir la data para los clusters de acuerdo de la frecuencia
# Separo los segmentos para hacer el cluster

data_1 = data[,c(1,2,9,10) ]

data_2 = data[data$Frecuencia == 2  ,c(1,4,5,6)]

data_3 = data[data$Frecuencia == 3  ,c(1,4,5,6) ]

head(data_1,3)

# Primero vamos a validar

training.data1 = createDataPartition(data_1$Vol_Entrega, p = 0.8, list = F)
train = data[training.data1,]
val = data[-training.data1,]


# Se va a generar el numero de clusters que serian 6

head(data.training[,c(5,6)])


m_train=kmeans(trai[,c(5,6)],6)
m_val=kmeans(val[,c(5,6)],6)


errmat1 = table(m_val$cluster,m_train$cluster, dnn = c("Actual", "Predichos"))
errmat1

# Todos casi son iguales entonces hacemos lo siguiente


c_data1 = kmeans(data_1[,c(3,4)],6)
c_data2 = kmeans(data_2[,c(3,4)],6)
c_data3 = kmeans(data_3[,c(3,4)],6)

data1_total = cbind(data_1,c_data1$cluster)
data2_total = cbind(data_2,c_data2$cluster)
data3_total = cbind(data_3,c_data3$cluster)





# ya realizando los clusters estamos insertando los datos


names(data1_total)=c("Id_Cliente",
                     "Vol_Entrega",
                     "lat",
                     "lon",
                     "clusters")

names(data2_total)=c("Id_Cliente",
                     "Vol_Entrega",
                     "lat",
                     "lon",
                     "clusters")

names(data2_total)=c("Id_Cliente",
                     "Vol_Entrega",
                     "lat",
                     "lon",
                     "clusters")


Total_Datos = rbind(data1_total,data2_total,data2_total)

head(Total_Datos)


# Ya insertados los bloques y los clusters
# se anexa un indicador para poder generar la matriz

Total_Datos_1 = Total_Datos[,c(1,5)]

n=nrow(Total_Datos_1)

indicador = sample(1,size = n, replace = TRUE)

Total_Datos_1 = cbind(Total_Datos_1,indicador)


# Generamos los demas clusters
# aqui vamos a transponer la informacion

Total_Datos_1$Cluster_1 = ifelse(Total_Datos_1$clusters==1,1,0)
Total_Datos_1$Cluster_2 = ifelse(Total_Datos_1$clusters==2,1,0)
Total_Datos_1$Cluster_3 = ifelse(Total_Datos_1$clusters==3,1,0)
Total_Datos_1$Cluster_4 = ifelse(Total_Datos_1$clusters==4,1,0)
Total_Datos_1$Cluster_5 = ifelse(Total_Datos_1$clusters==5,1,0)
Total_Datos_1$Cluster_6 = ifelse(Total_Datos_1$clusters==6,1,0)


# Como toda la informacion esta dentro de Data
# solomamente hacemos referencia de las columnas que queremos exportar

head(Total_Datos_1)



T_datos = Total_Datos_1[,c(1,4,5,6,7,8,9)]
head(T_datos)


Data=T_datos %>% group_by(Id_Cliente) %>% 
        summarise(Cluster_1=sum(Cluster_1),Cluster_2=sum(Cluster_2),
                  Cluster_3=sum(Cluster_3),Cluster_4=sum(Cluster_4),
                  Cluster_5=sum(Cluster_5),Cluster_6=sum(Cluster_6)
                  )


# Aqui nos falto una limpieza

Data$Cluster_1 = ifelse(Data$Cluster_1>0,1,0)
Data$Cluster_2 = ifelse(Data$Cluster_2>0,1,0)
Data$Cluster_3 = ifelse(Data$Cluster_3>0,1,0)
Data$Cluster_4 = ifelse(Data$Cluster_4>0,1,0)
Data$Cluster_5 = ifelse(Data$Cluster_5>0,1,0)
Data$Cluster_6 = ifelse(Data$Cluster_6>0,1,0)


# Aqui vamos a exportar la informacion a cvs

write.csv(T_datos, file="data/Resultado_5.csv")


