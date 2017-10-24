args = commandArgs(trailingOnly=TRUE)
simdir_to_list <- args[1]

if (length(args)==0){
stop("You must use one argument, the directory where your data is.")
}

simdir <- list.dirs(path=simdir_to_list,recursive=FALSE)



lambdas_vector <- c()
lambdas_Santiago_vector <- c()
lambdas_Rancagua_vector <- c()
lambdas_Valparaiso_vector <- c()
lambdas_La_Serena_vector <- c()

for (num_sim in simdir){

    print(num_sim)
    Santiago<-read.table(paste(num_sim,"/Santiago.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    Valparaiso <-read.table(paste(num_sim,"/Valparaiso.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    Rancagua <-read.table(paste(num_sim,"/Rancagua.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    La_Serena <-read.table(paste(num_sim,"/La_Serena.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))

    cities <- Santiago + Valparaiso + Rancagua + La_Serena
    #the /4 depend on the time given in the simulation
    cities <- transform(cities, time = time/4)
    write.table(cities,paste(num_sim,"/totalCities.out", sep=""))

    #Finding lambda
    model <- lm( log( replace(cities$infected[2:40], cities$infected[2:40]==0,1)) ~ cities$time[2:40])
    lambda <- model$coefficients[2]
    expo <- exp(predict(model))
    #print(lambda)

    #Finding lambda by cities
    modelSantiago <- lm( log(replace(Santiago$infected[2:40], Santiago$infected[2:40]==0, 1)) ~ Santiago$time[2:40])
    lambdaSantiago <- modelSantiago$coefficients[2]
    expoSantiago <- exp(predict(modelSantiago))
    
    modelValparaiso <- lm( log( replace(Valparaiso$infected[2:40], Valparaiso$infected[2:40]==0, 1)) ~ Valparaiso$time[2:40])
    lambdaValparaiso <- modelValparaiso$coefficients[2]
    expoValparaiso <- exp(predict(modelValparaiso))
    #print(lambdaValparaiso)

    modelRancagua <- lm( log( replace(Rancagua$infected[2:40], Rancagua$infected[2:40]==0, 1)) ~ Rancagua$time[2:40])
    lambdaRancagua <- modelRancagua$coefficients[2]
    expoRancagua <- exp(predict(modelRancagua))

    modelLa_Serena <- lm( log( replace(La_Serena$infected[2:40], La_Serena$infected[2:40]==0, 1)) ~ La_Serena$time[2:40])
    lambdaLa_Serena <- modelLa_Serena$coefficients[2]
    expoLa_Serena <- exp(predict(modelLa_Serena))

    lambdas_vector <- append(lambdas_vector, lambda)
    lambdas_Santiago_vector <- append(lambdas_Santiago_vector, lambdaSantiago)	
    lambdas_Valparaiso_vector <- append(lambdas_Valparaiso_vector, lambdaValparaiso)	
    lambdas_Rancagua_vector <- append(lambdas_Rancagua_vector, lambdaRancagua)	
    lambdas_La_Serena_vector <- append(lambdas_La_Serena_vector, lambdaLa_Serena)		


} 

print(lambdas_vector)
pdf("histogram_all_cities.pdf",7,7)
hist(lambdas_vector[lambdas_vector>0.0], freq=TRUE, breaks=15, main="Grow rates for all cities",xlab=expression("r"[0]), col="orange")#, xlim=c(0.013,0.017))
dev.off()


pdf("each_city.pdf",7,7)
old.par <-par(mfrow=c(2,2))
#pdf("histogram_Santiago.pdf",7,7)
hist(lambdas_Santiago_vector[lambdas_Santiago_vector>0.0], freq=TRUE, breaks=14, main="Grow rates Santiago",xlab=expression("r"[0]), col="orange")

#pdf("histogram_Valparaiso.pdf",7,7)
hist(lambdas_Valparaiso_vector[lambdas_Valparaiso_vector>0.0], freq=TRUE, breaks=14, main="Grow rates Valparaiso",xlab=expression("r"[0]), col="orange")

#pdf("histogram_La_Serena.pdf",7,7)
hist(lambdas_La_Serena_vector[lambdas_La_Serena_vector>0.0], freq=TRUE, breaks=14, main="Grow rates La_Serena",xlab=expression("r"[0]), col="orange")

#pdf("histogram_Rancagua.pdf",7,7)
hist(lambdas_Rancagua_vector[lambdas_Rancagua_vector>0.0], freq=TRUE, breaks=14, main="Grow rates Rancagua",xlab=expression("r"[0]), col="orange")

par(old.par)
