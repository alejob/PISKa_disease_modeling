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

expo_vector <- c()
expo_Santiago_vector <- c()
expo_Valparaiso_vector <- c()
expo_Rancagua_vector <- c()
expo_La_Serena_vector <- c()

expo <- 0 
expoSantiago <- 0
expoValparaiso <- 0
expoRancagua <- 0
expoLa_Serena <- 0

cities_sum <- c(0,0,0,0)
Santiago_sum <- c(0,0,0,0)
Valparaiso_sum <- c(0,0,0,0)
Rancagua_sum <- c(0,0,0,0)
La_Serena_sum <- c(0,0,0,0)

for (num_sim in simdir){

    #print(num_sim)
    Santiago<-read.table(paste(num_sim,"/Santiago.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    Valparaiso <-read.table(paste(num_sim,"/Valparaiso.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    Rancagua <-read.table(paste(num_sim,"/Rancagua.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
    La_Serena <-read.table(paste(num_sim,"/La_Serena.out", sep=""), col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))

    cities <- Santiago + Valparaiso + Rancagua + La_Serena
    #the /4 depends on the time given in the simulation
    cities <- transform(cities, time = time/4)

    cities_sum <- cities_sum + cities
    Santiago_sum <- Santiago_sum + Santiago
    Valparaiso_sum <- Valparaiso_sum + Valparaiso
    Rancagua_sum <- Rancagua_sum + Rancagua
    La_Serena_sum <- La_Serena_sum + La_Serena
    
    #write.table(cities,paste(num_sim,"/totalCities.out", sep=""))

    #Finding lambda
    model <- lm( log( replace(cities$infected[5:28], cities$infected[5:28]==0,1)) ~ cities$time[5:28])
    lambda <- model$coefficients[2]
    expo <- expo + exp(predict(model))
    #print(lambda)
    #print(expo)

    #Finding lambda by cities
    modelSantiago <- lm( log(replace(Santiago$infected[5:28], Santiago$infected[5:28]==0, 1)) ~ Santiago$time[5:28])
    lambdaSantiago <- modelSantiago$coefficients[2]
    expoSantiago <- expoSantiago + exp(predict(modelSantiago))
    
    modelValparaiso <- lm( log( replace(Valparaiso$infected[5:28], Valparaiso$infected[5:28]==0, 1)) ~ Valparaiso$time[5:28])
    lambdaValparaiso <- modelValparaiso$coefficients[2]
    expoValparaiso <- expoValparaiso + exp(predict(modelValparaiso))
    
    modelRancagua <- lm( log( replace(Rancagua$infected[5:28], Rancagua$infected[5:28]==0, 1)) ~ Rancagua$time[5:28])
    lambdaRancagua <- modelRancagua$coefficients[2]
    expoRancagua <- expoRancagua + exp(predict(modelRancagua))

    modelLa_Serena <- lm( log( replace(La_Serena$infected[5:28], La_Serena$infected[5:28]==0, 1)) ~ La_Serena$time[5:28])
    lambdaLa_Serena <- modelLa_Serena$coefficients[2]
    expoLa_Serena <- expoLa_Serena + exp(predict(modelLa_Serena))

    lambdas_vector <- append(lambdas_vector, lambda)
    lambdas_Santiago_vector <- append(lambdas_Santiago_vector, lambdaSantiago)	
    lambdas_Valparaiso_vector <- append(lambdas_Valparaiso_vector, lambdaValparaiso)	
    lambdas_Rancagua_vector <- append(lambdas_Rancagua_vector, lambdaRancagua)	
    lambdas_La_Serena_vector <- append(lambdas_La_Serena_vector, lambdaLa_Serena)		
        
    #expo_vector <- append(expo_vector, expo)
    #expo_Santiago_vector <- append(expo_Santiago_vector, expoSantiago)
    #expo_Valparaiso_vector <- append(expo_Valparaiso_vector, expoValparaiso)
    #expo_Rancagua_vector <- append(expo_Rancagua_vector, expoRancagua)
    #expo_La_Serena_vector <- append(expo_La_Serena_vector, expoLa_Serena)
} 

expo <- expo/length(simdir)
expoSantiago <- expoSantiago/length(simdir)
expoValparaiso <- expoValparaiso/length(simdir)
expoRancagua <- expoRancagua/length(simdir)
expoLa_Serena <- expoLa_Serena/length(simdir)

cities_sum <- cities_sum/length(simdir)
Santiago_sum <- Santiago_sum/length(simdir)
Valparaiso_sum <- Valparaiso_sum/length(simdir)
Rancagua_sum <- Rancagua_sum/length(simdir)
La_Serena_sum <- La_Serena_sum/length(simdir)

lambda <- sum(lambdas_vector)/length(simdir)
lambdaSantiago <- sum(lambdas_Santiago_vector)/length(simdir)
lambdaValparaiso <- sum(lambdas_Valparaiso_vector)/length(simdir)
lambdaRancagua <- sum(lambdas_Rancagua_vector)/length(simdir)
lambdaLa_Serena <- sum(lambdas_La_Serena_vector)/length(simdir)


### PLOT INFECTED VS TIME AND EXPONENTIAL ADJUST ###
pdf("adjust_all_cities.pdf",7,7)
plot(cities_sum $time, cities_sum $infected, ann=F,col="black")
lines(cities_sum $time[5:28], expo)
legend("topleft", inset=.05, legend=round(lambda, digits=4), title=expression("r"[0]))
title(main="Infected", sub="time [days]", ylab="infected")

dev.off()

pdf("each_city_infected_all.pdf",7,7)
old.par <-par(mfrow=c(2,2))

plot(Santiago_sum $time, Santiago_sum $infected, ann=F,col="black", main="Santiago")
lines(Santiago_sum $time[5:28], expoSantiago)
legend("topleft", inset=.05, legend=round(lambdaSantiago, digits=4), title=expression("r"[0]))
title(main="Santiago", sub="time [days]", ylab="infected")

plot(Valparaiso_sum $time, Valparaiso_sum $infected, ann=F,col="black", main="Valparaiso")
lines(Valparaiso_sum $time[5:28], expoValparaiso)
legend("topleft", inset=.05, legend=round(lambdaValparaiso, digits=4), title=expression("r"[0]))
title(main="Valparaiso", sub="time [days]", ylab="infected")

plot(La_Serena_sum $time, La_Serena_sum $infected, ann=F,col="black")
lines(La_Serena_sum $time[5:28], expoLa_Serena)
legend("topleft", inset=.05, legend=round(lambdaLa_Serena, digits=4), title=expression("r"[0]))
title(main="La_Serena", sub="time [days]", ylab="infected")

plot(Rancagua_sum $time, Rancagua_sum $infected, ann=F,col="black")
lines(Rancagua_sum $time[5:28], expoRancagua)
legend("topleft", inset=.05, legend=round(lambdaRancagua, digits=4), title=expression("r"[0]))
title(main="Rancagua", sub="time [days]", ylab="infected")

par(old.par)


### PLOT HISTOGRAMS ###
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