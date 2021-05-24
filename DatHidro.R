# datos hidrologicos ejercicio explorativo

#Leer el archivo con los datos hidrologicos
read.csv("C:/Users/Cristian Cabrera/OneDrive/Escritorio/Procesamiento de datos/Pr?ctica 1/FDC.csv")
inp <- read.csv("C:/Users/Cristian Cabrera/OneDrive/Escritorio/Procesamiento de datos/Pr?ctica 1/FDC.csv", na.strings = "")
inp <- read.csv("FDC.csv")

#Visualizar la estructura del archivo
head(inp)
dim(inp)

#Evaluacion de celdas vacias
inp[!complete.cases(inp),]

#Grafico de valores r?o Estrella y Banano
plot(inp[,2],
     main = "Volumen del agua",
     xlab = "Caudal en mm diario",
     ylab = "Fecha",
     type = "l", 
     col = "blue"
     )
lines(inp[,3], col="green")

legend(  
  x = "topleft",  
  inset = 0.05,  
  legend = c("Estrella", "Banano"),  
  fill = c("blue", "green"),  
  horiz = FALSE  
)


#Resumen de los datos de las columnas 2 y 3
summary(inp[,2:3])

#Histogramas del rio Estrella y Banano
hist(inp[,2],
main = "Estrella",
xlab = "Cantidad de agua daria",
ylab = "Fecruencia"
)

hist(inp[,3],
     main = "Banano",
     xlab = "Cantidad de agua daria",
     ylab = "Fecruencia"
)
#Asignacion de nombres
names(inp) <- c("fecha", "Estrella", "Banano")
attach(inp)
plot(Estrella)


#Series de tiempo
Tempdate <- strptime(inp[,1], format= "%d/%m/%Y")

#Promedios anuales de los caudales
MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN = sum)
MAQ_Banano <- tapply(Banano, format (Tempdate, format = "%Y"), FUN = sum)

#Exportar archivo MAQ.csv al direcctorio de trabajo
write.csv(rbind(MAQ_Estrella, MAQ_Banano), file = "MAQ.csv")

#Grafico promedios anuales de los caudal(comparaci?n entre caudales)
plot(
  MAQ_Banano, ylim = c(0, 3000),
  main = 'Valores anuales en mm por a?o',
  xlab = 'Fechas',
  ylab = 'Caudal anual en mm'
)
lines(MAQ_Estrella, col = 2)

legend(
  x = "topright",
  inset = 0.008,
  legend = c("Estrella", "Banano"),
  fill = c("red", "black"),
  horiz = FALSE
)

#Promedios mensuales de los caudales
MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN = sum)
MMQ_Banano <- tapply(Banano, format (Tempdate, format = "%m"), FUN = sum)

#Exportar archivo MMQ.csv al direcctorio de trabajo
write.csv(rbind(MMQ_Estrella, MMQ_Banano), file = "MMQ.csv")

#Analisis de correlacion
corinp <- cor(inp[,2:3],method = "spearman")

plot(Estrella, Banano,
     main = 'Correlacion entre la cuenca del Rio Estrella contra el Rio Banano',
)

#Modelo de regresi?n lineal
inp.lm <- lm(Estrella ~ Banano, data = inp)
summary(inp.lm)

#Graficos modelo de regresi?n lineal
plot(inp.lm)
