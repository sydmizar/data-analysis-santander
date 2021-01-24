# Postwork Sesion 03
library(ggplot2)
# Cambiar directorio de trabajo haciendo uso de ?setwd
setwd("C:/Users/BALAMLAPTOP2/Documents/GitHub/data-analysis-santander/Sesion04/Postworks/")

# Actividad 1
# Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, 
# los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
football <- read.csv("SP1.csv")

# Actividad 2
# Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles 
# anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)

# FTHG = Full Time Home Team Goals
# FTAG = Full Time Away Team Goals

# Actividad 3
# Consulta cómo funciona la función table en R al ejecutar en la consola ?table

# Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

# Calculo de frecuencia relativa por variable
# La frecuencia relativa es una medida estadística que se calcula como el cociente de la frecuencia absoluta de algún valor de la población/muestra (fi) 
# entre el total de valores que componen la población/muestra (N).

# Frecuencia relativa del equipo local
pm.fthg <- table(football$FTHG)/length(football$FTHG)
for (i in 1:length(pm.fthg)){
  print(paste("Frecuencia relativa de que el equipo local que juega en casa anote ", i-1, " goles es igual a ", pm.fthg[i]))
}

# Frecuencia relativa del equipo visitante
pm.ftag <- table(football$FTAG)/length(football$FTAG)
for (i in 1:length(pm.ftag)){
  print(paste("Frecuencia relativa de que el equipo visitante anote ", i-1, " goles es igual a ", pm.ftag[i]))
}

# Se calcula una tabla de probabilidad conjunta (?table), la cual se usa para calcular las probabilidades marginales y condicionales.
data <- football[c('FTHG','FTAG')]

rft <- table(data)/nrow(data)

for (i in 1:dim(rft)[1]){
  for (j in 1:dim(rft)[2]){
    print(paste("Probabilidad conjunta de que el equipo que juega en casa anote ", i-1, " y el equipo que juega como visitante anote ", j-1, " es igual a ", rft[i,j]))
  }
}

# Suma de las probabilidades debe ser igual a 1
sum(rft)

# Para calcular la probabilidad marginal de X = 0 esta dada por
# P(X=0)=P(X=0???Y=0)+P(X=0???Y=1)+P(X=0???Y=2)+P(X=0???Y=3)+...+P(X=0???Y=N)
# Esta es la suma de todos los elementos en la primera fila de tabla de probabilidades y se repite para cada de las filas en la matriz. Lo mismo se repite para Y sumando las columnas. 

rft <- cbind(rft, px = rowSums(rft))
rft <- rbind(rft, py = colSums(rft))

for (i in 1:length(pm.fthg)){
  print(paste("Probabilidad marginal de que el equipo local que juega en casa anote ", i-1, " goles es igual a ", rft[i,7]))
}

for (i in 1:length(pm.ftag)){
  print(paste("Probabilidad marginal de que el equipo visitante anote ", i-1, " goles es igual a ", rft[8,i]))
}

# Postwork sesion 4
# Ahora investigarás la dependencia o independencia del número de goles anotados por el equipo de casa 
# y el número de goles anotados por el equipo visitante mediante un procedimiento denominado bootstrap, 
# revisa bibliografía en internet para que tengas nociones de este desarrollo.

# Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), 
# y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al 
# dividir estas probabilidades conjuntas por el producto de las probabilidades marginales correspondientes.
df_cocientes <- table(data)/nrow(data)
for (i in 1:dim(df_cocientes)[1]){
  for (j in 1:dim(df_cocientes)[2]){
    df_cocientes[i,j] <- df_cocientes[i,j]/(rft[i,7]*rft[8,j])
    # print(paste("val ",val," i ",i-1," j ", j-1))
  }
}

asdf <- as.data.frame(df_cocientes)
freq.mean = with(asdf, mean(Freq))
freq.mean

B = 5000
n = nrow(asdf)
boot.samples = matrix(sample(asdf$Freq, size = B * n, replace = TRUE), B, n)
boot.statistics = apply(boot.samples, 1, mean)

ggplot(data.frame(Cociente = boot.statistics),aes(x=Cociente)) +
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="red")


library(dispRity)
library(boot)

foo <- function(data, indices, cor.type){
  dt<-data[indices,]
  c(
    cor(dt[,1], dt[,2], method=cor.type),
    median(dt[,1]),
    median(dt[,2])
  )
}

fc <- function(d, i){
  d2 <- d[i,]
  return(cor(d2$write, d2$math))
}

set.seed(12345)
bootcorr <- boot(as.data.frame(df_cocientes), fc, R=500)
myBootstrap <- boot(as.data.frame(df_cocientes), foo, R=1000, cor.type='s')

xbarstar <- sapply(resamps, mean, simplify = TRUE)

results <- boot.matrix(as.data.frame(df_cocientes), bootstraps = 20)
# Mediante un procedimiento de boostrap, obtén más cocientes similares a los obtenidos en la tabla del punto 
# anterior. Esto para tener una idea de las distribuciones de la cual vienen los cocientes en la tabla anterior. 
# Menciona en cuáles casos le parece razonable suponer que los cocientes de la tabla en el punto 1, son iguales 
# a 1 (en tal caso tendríamos independencia de las variables aleatorias X y Y).

srs <- rnorm(25, mean = 3)