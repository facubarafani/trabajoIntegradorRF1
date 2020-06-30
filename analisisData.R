library(ggplot2)
library(dplyr)

circuits <- read.csv(file="./circuits.csv",header=TRUE)
drivers <- read.csv(file="./drivers.csv",header=TRUE)
standings <- read.csv(file="./driverStandings.csv",header=TRUE)
results <- read.csv("results.csv")
constructors <- read.csv("constructors.csv")
pitStops <- read.csv("pitStops.csv")
races <- read.csv("races.csv")

filter_winner <- (results$positionText=="1")
winners <- results[filter_winner,]

filter_abandons <- (results$positionText=="R")
abandons <- results[filter_abandons,]

results.by.race <- merge(x=results[,c("raceId", "driverId","positionText")], y=races[,c("raceId","circuitId","name")],by="raceId")
raceAbandons.by.race <- merge(x=abandons[,c("raceId", "driverId","positionText")], y=races[,c("raceId","circuitId","name")],by="raceId")
abandons.by.circuit <- as.data.frame(summary(raceAbandons.by.race$name,1000))
results.by.circuit <- as.data.frame(summary(results.by.race$name,1000))
abandon.percentage.by.circuit <- as.data.frame(data.frame(abandons.by.race,results.by.circuit))
colnames(abandon.percentage.by.circuit) <- c("Abandonos","Apariciones_totales")
abandon.percentage.by.circuit$Porcentaje <- 100*(abandon.percentage.by.circuit$Abandonos)/(abandon.percentage.by.circuit$Apariciones_totales)

results.by.driver <- merge(x=winners[,c("raceId", "driverId","position")], y=drivers[,c("driverId","driverRef","surname","nationality")], by="driverId")
races.by.driver <- merge(x=results[,c("raceId","driverId","position")], y=drivers[,c("driverId","driverRef","surname","nationality")], by="driverId")

results.by.constructor <- merge(x=winners[,c("raceId","constructorId","position")], y=constructors[,c("constructorId","constructorRef","name","nationality")], by="constructorId")
races.by.constructor <- merge(x=results[,c("raceId","constructorId","position")], y=constructors[,c("constructorId","constructorRef","name","nationality")], by="constructorId")

wins.by.winners <- as.data.frame(summary(results.by.driver$surname,1000))

wins.by.nationality <- as.data.frame(summary(results.by.driver$nationality,1000))

races.by.nationality <- merge(x=results[,c("raceId","driverId","position")], y=drivers[,c("driverId","driverRef","surname","nationality")], by="driverId")

groupedRacesByNationality <- as.data.frame(summary(races.by.nationality$nationality,1000))

win.percentage.by.nationality <- as.data.frame(data.frame(wins.by.nationality,groupedRacesByNationality))

colnames(win.percentage.by.nationality) <- c("Victorias","Apariciones_totales")

win.percentage.by.nationality$Porcentaje <- 100*(win.percentage.by.nationality$Victorias)/(win.percentage.by.nationality$Apariciones_totales)

top.nationalities <- win.percentage.by.nationality[order(win.percentage.by.nationality$Porcentaje,decreasing = T),]

top.nationalities <- head(top.nationalities,10)

pairs(win.percentage.by.nationality, pch="o")

win.percentage.by.constructor$name <- rownames(win.percentage.by.constructor)

wins.by.constructor <- as.data.frame(summary(results.by.constructor$name,1000))

races.by.constructorWinners <- as.data.frame(summary(races.by.constructor$name,1000))

colnames(wins.by.winners) <- c("Victorias")

colnames(wins.by.constructor) <- c("Victorias")

colnames(wins.by.nationality) <- c("Victorias")

win.percentage.by.constructor <- as.data.frame(data.frame(wins.by.constructor,races.by.constructorWinners))

colnames(win.percentage.by.constructor) <- c("Victorias","Apariciones_totales")

win.percentage.by.constructor$percentage <- 100*(win.percentage.by.constructor$Victorias)/(win.percentage.by.constructor$Apariciones_totales)

win.percentage.by.constructor$name <- rownames(win.percentage.by.constructor)

top.constructors <- win.percentage.by.constructor[order(win.percentage.by.constructor$percentage,decreasing = T),]

top.constructors <- head(top.constructors,10)

topConstructorsPlot <- ggplot(data=top.constructors, aes(x=reorder(name,-percentage), y=percentage))

bar.colors <- c("Gold", "Gray", "#B35900", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699"
                )

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




