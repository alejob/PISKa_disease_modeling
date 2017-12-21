args = commandArgs(trailingOnly=TRUE)

Santiago<-read.table(paste(args[1],"/Santiago.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
Valparaiso <-read.table(paste(args[1],"/Valparaiso.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
Rancagua <-read.table(paste(args[1],"/Rancagua.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
La_Serena <-read.table(paste(args[1],"/La_Serena.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))

cities <- Santiago + Valparaiso + Rancagua + La_Serena
cities <- transform(cities, time = time/4)
write.table(cities,paste(args[1],"/totalCities.out", sep=""))

#Finding lambda
model <- lm( log( replace(cities$infected[2:40], cities$infected[2:40]==0,1)) ~ cities$time[2:40])
lambda <- model$coefficients[2]
expo <- exp(predict(model))

#Finding lambda by cities
modelSantiago <- lm( log(replace(Santiago$infected[2:40], Santiago$infected[2:40]==0, 1)) ~ Santiago$time[2:40])
lambdaSantiago <- modelSantiago$coefficients[2]
expoSantiago <- exp(predict(modelSantiago))

modelValparaiso <- lm( log( replace(Valparaiso$infected[2:40], Valparaiso$infected[2:40]==0, 1)) ~ Valparaiso$time[2:40])
lambdaValparaiso <- modelValparaiso$coefficients[2]
expoValparaiso <- exp(predict(modelValparaiso))

modelRancagua <- lm( log( replace(Rancagua$infected[2:40], Rancagua$infected[2:40]==0, 1)) ~ Rancagua$time[2:40])
lambdaRancagua <- modelRancagua$coefficients[2]
expoRancagua <- exp(predict(modelRancagua))

modelLa_Serena <- lm( log( replace(La_Serena$infected[2:40], La_Serena$infected[2:40]==0, 1)) ~ La_Serena$time[2:40])
lambdaLa_Serena <- modelLa_Serena$coefficients[2]
expoLa_Serena <- exp(predict(modelLa_Serena))



print(lambdaSantiago)
print(lambdaValparaiso)

#print(model)
print(lambda)

#SEIRD GRAPH
pdf("SEIRD_all_cities.pdf",7,7)
#plot(cities $time, cities $cases, ann=F,col="red")
plot(cities $time, cities $susceptible, ann=F, col="red", ylim=c(0,127000))
points(cities $time, cities $exposed, ann=F,col="blue")
points(cities $time, cities $infected, ann=F, col="black")
points(cities $time, cities $removed, ann=F, col="purple")
points(cities $time, cities $dead, ann=F, col="orange")


#legend("topleft", inset=.05, title="State all cities",c("E+I+R+D","susceptible","exposed","infected","removed","dead"), fill=c("red","green","blue","black","purple","orange"), horiz=FALSE)
legend("topleft", inset=.05, title="State all cities",c("susceptible","exposed","infected","removed","dead"), fill=c("red","blue","black","purple","orange"), horiz=FALSE)

options(scipen=1)
title(main="States", sub="time", ylab="cases")
#axis(side=2,at ,labels=format(cities$susceptible,scientific = TRUE))

#options(digits=2)


#INFECTED GRAPH
pdf("infected_all_cities.pdf",7,7)
#plot(cities $time, cities $infected, ann=F,col="black")
plot(cities $time, cities $infected,col="black")
lines(cities $time[2:40], expo)
title(main="Infected", sub="time [days]", ylab="infected")

dev.off()


pdf("each_city_infected.pdf",7,7)                                                                                    
old.par <-par(mfrow=c(2,2))                                                                                          

plot(Santiago $time, Santiago $infected, ann=F,col="black", main="Santiago")
lines(Santiago $time[2:40], expoSantiago)
legend("topleft", inset=.05, legend=round(lambdaSantiago, digits=4), title=expression("r"[0]))
title(main="Santiago", sub="time [days]", ylab="infected")

plot(Valparaiso $time, Valparaiso $infected, ann=F,col="black", main="Valparaiso")
lines(Valparaiso $time[2:40], expoValparaiso)
legend("topleft", inset=.05, legend=round(lambdaValparaiso, digits=4), title=expression("r"[0]))
title(main="Valparaiso", sub="time [days]", ylab="infected")

plot(La_Serena $time, La_Serena $infected, ann=F,col="black")
lines(La_Serena $time[2:40], expoLa_Serena)
legend("topleft", inset=.05, legend=round(lambdaLa_Serena, digits=4), title=expression("r"[0]))
title(main="La_Serena", sub="time [days]", ylab="infected")

plot(Rancagua $time, Rancagua $infected, ann=F,col="black")
lines(Rancagua $time[2:40], expoRancagua)
legend("topleft", inset=.05, legend=round(lambdaRancagua, digits=4), title=expression("r"[0]))
title(main="Rancagua", sub="time [days]", ylab="infected")

par(old.par)