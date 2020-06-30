library(ggplot2)
library(dplyr)

#Se importan las diferentes tablas de la DB utilizada
circuits <- read.csv(file="./circuits.csv",header=TRUE)
drivers <- read.csv(file="./drivers.csv",header=TRUE)
standings <- read.csv(file="./driverStandings.csv",header=TRUE)
results <- read.csv("results.csv")
constructors <- read.csv("constructors.csv")
pitStops <- read.csv("pitStops.csv")
races <- read.csv("races.csv")

#Filtramos a los ganadores de cada carrera buscando unicamente los que hayan obtenido la posicion 1
filter_winner <- (results$positionText=="1")
winners <- results[filter_winner,]

#Filtramos a los participantes que hayan abandonado en una carrera
filter_abandons <- (results$positionText=="R")
abandons <- results[filter_abandons,]

#Creamos la tabla con los resultados en cada carrera
results.by.race <- merge(x=results[,c("raceId", "driverId","positionText")], y=races[,c("raceId","circuitId","name")],by="raceId")

#Creamos la tabla donde se filtran unicamente los corredores que hayan abandonado en las distintas carreras
raceAbandons.by.race <- merge(x=abandons[,c("raceId", "driverId","positionText")], y=races[,c("raceId","circuitId","name")],by="raceId")

#Agrupamos los abandonos y los resultados totales segun el circuito
abandons.by.circuit <- as.data.frame(summary(raceAbandons.by.race$name,1000))
results.by.circuit <- as.data.frame(summary(results.by.race$name,1000))

#Calculamos el porcentaje de abandonos segun el circuito
abandon.percentage.by.circuit <- as.data.frame(data.frame(abandons.by.race,results.by.circuit))

#Cambiamos el nombre de las columnas
colnames(abandon.percentage.by.circuit) <- c("Abandonos","Apariciones_totales")
abandon.percentage.by.circuit$Porcentaje <- 100*(abandon.percentage.by.circuit$Abandonos)/(abandon.percentage.by.circuit$Apariciones_totales)

#Realizamos el modelo de abandonos segun el porcentaje de abandonos p/ circuito
pairs(abandon.percentage.by.circuit, pch="•")

#Creamos las tablas de corredores que hayan ganado y otra con las carreras totales por corredor
results.by.driver <- merge(x=winners[,c("raceId", "driverId","position")], y=drivers[,c("driverId","driverRef","surname","nationality")], by="driverId")
races.by.driver <- merge(x=results[,c("raceId","driverId","position")], y=drivers[,c("driverId","driverRef","surname","nationality")], by="driverId")

#Creamos las tablas de fabricantes que hayan ganado carreras y otra con el total de participaciones p/ fabricante
results.by.constructor <- merge(x=winners[,c("raceId","constructorId","position")], y=constructors[,c("constructorId","constructorRef","name","nationality")], by="constructorId")
races.by.constructor <- merge(x=results[,c("raceId","constructorId","position")], y=constructors[,c("constructorId","constructorRef","name","nationality")], by="constructorId")

#Agrupamos las victorias p/ corredor
wins.by.winners <- as.data.frame(summary(results.by.driver$surname,1000))

#Agrupamos las victorias p/ nacionalidad
wins.by.nationality <- as.data.frame(summary(results.by.driver$nationality,1000))

# Creamos una tabla donde aparezcan todas las carreras segun la nacionalidad
races.by.nationality <- merge(x=results[,c("raceId","driverId","position")], y=drivers[,c("driverId","driverRef","surname","nationality")], by="driverId")

#Agrupamos las carreras p/ nacionalidad
groupedRacesByNationality <- as.data.frame(summary(races.by.nationality$nationality,1000))

#Creamos una tabla donde se muestre la eficacia segun la nacionalidad
win.percentage.by.nationality <- as.data.frame(data.frame(wins.by.nationality,groupedRacesByNationality))

#Cambiamos los nombres de las columnas
colnames(win.percentage.by.nationality) <- c("Victorias","Apariciones_totales")

#Calculamos el porcentaje de victorias segun la nacionalidad
win.percentage.by.nationality$Porcentaje <- 100*(win.percentage.by.nationality$Victorias)/(win.percentage.by.nationality$Apariciones_totales)

#Reducimos la tabla a los 10 primeros
top.nationalities <- win.percentage.by.nationality[order(win.percentage.by.nationality$Porcentaje,decreasing = T),]
top.nationalities <- head(top.nationalities,10)

#Graficamos el model de porcentajes de victoria segun la nacionalidad
pairs(win.percentage.by.nationality, pch="o")

#Asignamos el nombre del fabricante a cada fila
win.percentage.by.constructor$name <- rownames(win.percentage.by.constructor)

#Agrupamos las victorias p/ fabricante
wins.by.constructor <- as.data.frame(summary(results.by.constructor$name,1000))

#Agrupamos las carreras totales corridas p/ fabricante
races.by.constructorWinners <- as.data.frame(summary(races.by.constructor$name,1000))

#Cambiamos el nombre de las columnas
colnames(wins.by.winners) <- c("Victorias")
colnames(wins.by.constructor) <- c("Victorias")
colnames(wins.by.nationality) <- c("Victorias")

#Calculamos el porcentaje de victorias p/ fabricante
win.percentage.by.constructor <- as.data.frame(data.frame(wins.by.constructor,races.by.constructorWinners))
colnames(win.percentage.by.constructor) <- c("Victorias","Apariciones_totales")
win.percentage.by.constructor$percentage <- 100*(win.percentage.by.constructor$Victorias)/(win.percentage.by.constructor$Apariciones_totales)

#Establecemos a cada fila el nombre del fabricante
win.percentage.by.constructor$name <- rownames(win.percentage.by.constructor)

#Ordenamos de mayor a menor segun el porcentaje de eficacia
top.constructors <- win.percentage.by.constructor[order(win.percentage.by.constructor$percentage,decreasing = T),]

#Filtramos la tabla al top 10
top.constructors <- head(top.constructors,10)

#Establecemos los datos con los que se va a crear el grafico de los fabricantes con mayor eficacia
topConstructorsPlot <- ggplot(data=top.constructors, aes(x=reorder(name,-percentage), y=percentage))

#Establecemos los colores que tendran las diferentes barras del grafico
bar.colors <- c("Gold", "Gray", "#B35900", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699"
                )
#Creamos el grafico del top 10 fabricantes de la f1
topConstructorsPlot + geom_bar(stat="identity", fill=bar.colors, color="Black",alpha=0.75) + xlab("") +
  ylab("Proporcion de victorias (%)") + ggtitle("Proporcion de victorias Top 10 fabricantes") +
  scale_x_discrete(labels=c("Brawn", "Matra Ford", "Mercedes", 
                            "Vanwall", "Watson", "Epperly",
                            "Red Bull", "MCLaren",
                            "Ferrari", "Brabham-Repco")) +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25), 
        plot.title = element_text(hjust=0.5))

pitStopsDuration <- as.data.frame(pitStops$duration)

pitStopsDurationMean <- mean(as.numeric(pitStopsDuration$`pitStops$duration`))




