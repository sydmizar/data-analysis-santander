# Postwork Sesion 03
library(ggplot2)
# Cambiar directorio de trabajo haciendo uso de ?setwd
setwd("C:/Users/BALAMLAPTOP2/Documents/GitHub/data-analysis-santander/Sesion03/Postworks/")

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

# Frecuencia relativa del equipo local 0 - 6
pm.fthg <- table(football$FTHG)/length(football$FTHG)
for (i in 1:length(pm.fthg)){
  print(paste("Frecuencia relativa de que el equipo local que juega en casa anote ", i-1, " goles es igual a ", pm.fthg[i]))
}

# Frecuencia relativa del equipo visitante 0 - 5
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

pmarg.gc <- rowSums(rft)
pmarg.gv <- colSums(rft)

for (i in 1:length(pm.fthg)){
  print(paste("Probabilidad marginal de que el equipo local que juega en casa anote ", i-1, " goles es igual a ", pmarg.gc[i]))
}

for (i in 1:length(pm.ftag)){
  print(paste("Probabilidad marginal de que el equipo visitante anote ", i-1, " goles es igual a ", pmarg.gv[i]))
}

# Activades para este postwork

lgc <-length(pm.fthg) 
lgv <- length(pm.ftag)
# Un gr�fico de barras para las probabilidades marginales estimadas del n�mero de goles que anota el equipo de casa.
df <- data.frame(goals=seq(0,lgc-1,1), pmarg=rowSums(rft))
ggplot(data=df, aes(x=goals, y=pmarg)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=pmarg), vjust=-0.3, size=3.5)+
  theme_minimal()+labs(title= "Probabilidades marginales estimadas del n�mero de goles que anota el equipo de casa", y="Probabilidad marginal", x = "Goles equipo local")

# Un gr�fico de barras para las probabilidades marginales estimadas del n�mero de goles que anota el equipo visitante.
df <- data.frame(goals=seq(0,lgv-1,1), pmarg=colSums(rft))
ggplot(data=df, aes(x=goals, y=pmarg)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=pmarg), vjust=-0.3, size=2.5)+
  theme_minimal()+labs(title= "Probabilidades marginales estimadas del n�mero de goles que anota el equipo visitante", y="Probabilidad marginal", x = "Goles equipo visitante")

# Un HeatMap para las probabilidades conjuntas estimadas de los n�meros de goles que anotan el equipo de casa y el equipo visitante en un partido.

# rft <- table(data)/nrow(data)

rft2 <- as.data.frame(rft)

ggplot(as.data.frame(rft), aes(x = FTHG, y = FTAG, fill = Freq)) + geom_tile()+
  labs(title= "Probabilidades conjuntas estimadas del n�mero de goles que anota el equipo local y visitante", x="Goles equipo local", y = "Goles equipo visitante")

# References:
# https://economipedia.com/definiciones/frecuencia-relativa.html
# https://www.statisticshowto.com/marginal-distribution/#:~:text=their%20joint%20probability%20distribution%20at,of%20X%20and%20Y%20%2C%20respectively.&text=The%20distribution%20must%20be%20from,%2C%E2%80%9D%20like%20X%20and%20Y.
# https://rstudio-pubs-static.s3.amazonaws.com/209289_9f9ba331cccc4e8f8aabdb9273cc76af.html
# https://tinyheero.github.io/2016/03/20/basic-prob.html
