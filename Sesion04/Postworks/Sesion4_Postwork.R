# Postwork Sesion 03
library(ggplot2)
# Cambiar directorio de trabajo haciendo uso de ?setwd
setwd("C:/Users/BALAMLAPTOP2/Documents/GitHub/data-analysis-santander/Sesion04/Postworks/")

# Actividad 1
# Importa los datos de soccer de la temporada 2019/2020 de la primera divisi�n de la liga espa�ola a R, 
# los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
# football <- read.csv("SP1.csv")
u1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
u1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

download.file(url = u1718, destfile = "SP1-1718.csv", mode = "wb")
download.file(url = u1819, destfile = "SP1-1819.csv", mode = "wb")
download.file(url = u1920, destfile = "SP1-1920.csv", mode = "wb")

# Importamos los datos a R

rawdata <- lapply(list.files(pattern = "*.csv"), read.csv)


# Con la funci�n select del paquete dplyr selecciona �nicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; 
# esto para cada uno de los data frames. (Hint: tambi�n puedes usar lapply).

selecteddata <- lapply(rawdata, select, Date, HomeTeam:FTR)


# Aseg�rate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date 
# y mutate para arreglar las fechas). Con ayuda de la funci�n rbind forma un �nico data frame que contenga las seis columnas mencionadas 
# en el punto 3 (Hint 2: la funci�n do.call podr�a ser utilizada).
mutateddata <- lapply(selecteddata, mutate, 
                      Date = as.Date(Date, "%d/%m/%y"),
                      HomeTeam = as.factor(HomeTeam),
                      AwayTeam = as.factor(AwayTeam),
                      FTHG = as.numeric(FTHG), 
                      FTAG = as.numeric(FTAG), 
                      FTR = as.factor(FTR))

football <- do.call(rbind, mutateddata)


# Actividad 2
# Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los n�meros de goles 
# anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)

# FTHG = Full Time Home Team Goals
# FTAG = Full Time Away Team Goals

# Actividad 3
# Consulta c�mo funciona la funci�n table en R al ejecutar en la consola ?table

# Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

# Calculo de frecuencia relativa por variable
# La frecuencia relativa es una medida estad�stica que se calcula como el cociente de la frecuencia absoluta de alg�n valor de la poblaci�n/muestra (fi) 
# entre el total de valores que componen la poblaci�n/muestra (N).

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

# rft <- cbind(rft, px = rowSums(rft))
# rft <- rbind(rft, py = colSums(rft))
# 
# for (i in 1:length(pm.fthg)){
#   print(paste("Probabilidad marginal de que el equipo local que juega en casa anote ", i-1, " goles es igual a ", rft[i,7]))
# }
# 
# for (i in 1:length(pm.ftag)){
#   print(paste("Probabilidad marginal de que el equipo visitante anote ", i-1, " goles es igual a ", rft[8,i]))
# }

pmarg.gc <- rowSums(rft)
pmarg.gv <- colSums(rft)

for (i in 1:length(pm.fthg)){
  print(paste("Probabilidad marginal de que el equipo local que juega en casa anote ", i-1, " goles es igual a ", pmarg.gc[i]))
}

for (i in 1:length(pm.ftag)){
  print(paste("Probabilidad marginal de que el equipo visitante anote ", i-1, " goles es igual a ", pmarg.gv[i]))
}

# Postwork sesion 4
# Ahora investigar�s la dependencia o independencia del n�mero de goles anotados por el equipo de casa 
# y el n�mero de goles anotados por el equipo visitante mediante un procedimiento denominado bootstrap, 
# revisa bibliograf�a en internet para que tengas nociones de este desarrollo.

# Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), 
# y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obt�n una tabla de cocientes al 
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
# Mediante un procedimiento de boostrap, obt�n m�s cocientes similares a los obtenidos en la tabla del punto 
# anterior. Esto para tener una idea de las distribuciones de la cual vienen los cocientes en la tabla anterior. 
# Menciona en cu�les casos le parece razonable suponer que los cocientes de la tabla en el punto 1, son iguales 
# a 1 (en tal caso tendr�amos independencia de las variables aleatorias X y Y).

srs <- rnorm(25, mean = 3)