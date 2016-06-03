args = commandArgs(trailingOnly=TRUE)
simdir <- list.dirs(path='simdir',recursive=FALSE)

lambdas_vector <- c()
lambdas_Santiago_vector <- c()

for (num_sim in simdir){

    print(num_sim)
    Santiago<-read.table(paste(num_sim,"/Santiago.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    Valparaiso <-read.table(paste(num_sim,"/Valparaiso.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    Rancagua <-read.table(paste(num_sim,"/Rancagua.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    La_Serena <-read.table(paste(num_sim,"/La_Serena.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))

    cities <- Santiago + Valparaiso + Rancagua + La_Serena
    write.table(cities,paste(num_sim,"/totalCities.out", sep=""))

    #Finding lambda
    model <- lm( log( replace(cities$infected[2:40], cities$infected[2:40]==0,1)) ~ cities$time[2:40])
    lambda <- model$coefficients[2]
    expo <- exp(predict(model))
    print(lambda)

    #Finding lambda by cities
    modelSantiago <- lm( log(replace(Santiago$infected[2:40], Santiago$infected[2:40]==0, 1)) ~ Santiago$time[2:40])
    lambdaSantiago <- modelSantiago$coefficients[2]
    expoSantiago <- exp(predict(modelSantiago))
    print(lambdaSantiago)

    #print(replace(Valparaiso$infected[2:40], Valparaiso$infected[2:40]==0, 1))
    #print(Valparaiso$infected[2:40])
    modelValparaiso <- lm( log( replace(Valparaiso$infected[2:40], Valparaiso$infected[2:40]==0, 1)) ~ Valparaiso$time[2:40])
    lambdaValparaiso <- modelValparaiso$coefficients[2]
    expoValparaiso <- exp(predict(modelValparaiso))
    print(lambdaValparaiso)

    lambdas_vector <- append(lambdas_vector, lambda)
    lambdas_Santiago_vector <- append(lambdas_Santiago_vector, lambdaSantiago)	


} 

print(lambdas_vector)
pdf("histogram_all_cities.pdf",7,7)
hist(lambdas_vector, freq=TRUE, breaks=15, main="Lambdas for many simulations",xlab=expression(lambda), col="orange")

pdf("histogram_Santiago.pdf",7,7)
hist(lambdas_Santiago_vector, freq=TRUE, breaks=14, main="Lambdas Santiago",xlab=expression(lambda), col="orange")
