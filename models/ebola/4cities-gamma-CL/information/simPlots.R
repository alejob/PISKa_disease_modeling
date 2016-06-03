args = commandArgs(trailingOnly=TRUE)

Santiago<-read.table(paste(args[1],"/Santiago.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
Valparaiso <-read.table(paste(args[1],"/Valparaiso.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
Rancagua <-read.table(paste(args[1],"/Rancagua.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
La_Serena <-read.table(paste(args[1],"/La_Serena.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))

cities <- Santiago + Valparaiso + Rancagua + La_Serena
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

print(lambdaSantiago)
print(lambdaValparaiso)

#print(model)
print(lambda)

#SEIRD GRAPH
pdf("SEIRD.pdf",7,7)
plot(cities $time, cities $cases, ann=F,col="red")
points(cities $time, cities $susceptible, ann=F, col="green")
points(cities $time, cities $exposed, ann=F,col="blue")
points(cities $time, cities $infected, ann=F, col="black")
points(cities $time, cities $removed, ann=F, col="purple")
points(cities $time, cities $dead, ann=F, col="orange")


legend("topleft", inset=.05, title="State",c("E+I+R+D","susceptible","exposed","infected","removed","dead"), fill=c("red","green","blue","black","purple","orange"), horiz=FALSE)

title(main="States", sub="time", ylab="cases")
#dev.off()

#INFECTED GRAPH
pdf("infected.pdf",7,7)
plot(cities $time, cities $infected, ann=F,col="black")
lines(cities $time[2:40], expo)
title(main="Infected", sub="time", ylab="infected")
