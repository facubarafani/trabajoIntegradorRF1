library(ggplot2)
library(dplyr)

drivers <- read.csv(file="./drivers.csv",header=TRUE)
standings <- read.csv(file="./driverStandings.csv",header=TRUE)
results <- read.csv("results.csv")
constructors <- read.csv("constructors.csv")

filter_winner <- (results$positionText=="1")
winners <- results[filter_winner,]

results.by.driver <- merge(x=winners[,c("raceId", "driverId","position")], y=drivers[,c("driverId","driverRef","surname","nationality")], by="driverId")
races.by.driver <- merge(x=results[,c("raceId","driverId","position")], y=drivers[,c("driverId","driverRef","surname","nationality")], by="driverId")

results.by.constructor <- merge(x=winners[,c("raceId","constructorId","position")], y=constructors[,c("constructorId","constructorRef","name","nationality")], by="constructorId")
races.by.constructor <- merge(x=results[,c("raceId","constructorId","position")], y=constructors[,c("constructorId","constructorRef","name","nationality")], by="constructorId")

wins.by.winners <- as.data.frame(summary(results.by.driver$surname,1000))

wins.by.nationality <- as.data.frame(summary(results.by.driver$nationality,1000))

wins.by.constructor <- as.data.frame(summary(results.by.constructor$name,1000))

races.by.constructorWinners <- as.data.frame(summary(races.by.constructor$name,1000))

colnames(wins.by.winners) <- c("Victorias")

colnames(wins.by.constructor) <- c("Victorias")

colnames(wins.by.nationality) <- c("Victorias")

win.percentage.by.constructor <- as.data.frame(data.frame(wins.by.constructor,races.by.constructorWinners))

colnames(win.percentage.by.constructor) <- c("Victorias","Apariciones_totales")

win.percentage.by.constructor$percentage <- 100*(win.percentage.by.constructor$Victorias)/(win.percentage.by.constructor$Apariciones_totales)



