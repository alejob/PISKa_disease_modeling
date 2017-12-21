args = commandArgs(trailingOnly=TRUE)

if (length(args)==0){
stop("You must use one argument, the directory where your data is.")
}


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
#lm(y_data ~ x_data)

#modelSantiago <- lm( log(replace(Santiago$infected[2:40], Santiago$infected[2:40]==0, 1)) ~ Santiago$time[2:40])
#lambdaSantiago <- modelSantiago$coefficients[2]
#expoSantiago <- exp(predict(modelSantiago))

#Santiago<-read.table("base.simdir/Santiago.out", col.names=c("time", "cases", "susceptible", "exposed","infected", "removed", "dead"))


y<-replace(Santiago$infected[2:40], Santiago$infected[2:40]==0, 1)
t<- Santiago$time[2:40]
modelSantiago <- function(t, x0, lambda, b){(x0^b+lambda*t*b)^(1/b)}
plot(t,Santiago$infected[2:40])

#grafica para ajustar
#lines(t,modelSantiago(t,1,0.05,0.98))
lines(t,modelSantiago(t,1,0.09,0.58))
