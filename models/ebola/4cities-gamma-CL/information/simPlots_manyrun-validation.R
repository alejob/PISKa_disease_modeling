args = commandArgs(trailingOnly=TRUE)
simdir_to_list <- args[1]

if (length(args)==0){
stop("You must use one argument, the directory where your data is.")
}

simdir <- list.dirs(path=simdir_to_list,recursive=FALSE)

library(sfsmisc)

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
    #lm: fit linear models
    # x ~ y
    model <- lm( log( replace(cities$infected[2:40], cities$infected[2:40]==0,1)) ~ cities$time[2:40])
    lambda <- model$coefficients[2]
    expo <- expo + exp(predict(model))
    #print(model$coefficients)
    #print(lambda)
    #print(expo)

    #Finding lambda by cities
    modelSantiago <- lm( log(replace(Santiago$infected[2:40], Santiago$infected[2:40]==0, 1)) ~ Santiago$time[2:40])
    lambdaSantiago <- modelSantiago$coefficients[2]
    expoSantiago <- expoSantiago + exp(predict(modelSantiago))
    
    modelValparaiso <- lm( log( replace(Valparaiso$infected[2:40], Valparaiso$infected[2:40]==0, 1)) ~ Valparaiso$time[2:40])
    lambdaValparaiso <- modelValparaiso$coefficients[2]
    expoValparaiso <- expoValparaiso + exp(predict(modelValparaiso))
    
    modelRancagua <- lm( log( replace(Rancagua$infected[2:40], Rancagua$infected[2:40]==0, 1)) ~ Rancagua$time[2:40])
    lambdaRancagua <- modelRancagua$coefficients[2]
    expoRancagua <- expoRancagua + exp(predict(modelRancagua))

    modelLa_Serena <- lm( log( replace(La_Serena$infected[2:40], La_Serena$infected[2:40]==0, 1)) ~ La_Serena$time[2:40])
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

#expo_ode <- exp() 

### PLOT INFECTED VS TIME AND EXPONENTIAL ADJUST ###
pdf("adjust_all_citiesD.pdf",7,7)
#library(sfsmisc) ##############ESTAS LINEAS DEFINEN LA NOTACION CIENTIFICA DE AQUI HACIA ABAJO#######################
#options(scipen=-3) #to set scientific notation
#par(mar=c(5,6,4,2)+0.1) #to set more margin
plot(cities_sum $time, cities_sum $infected, ann=F,col="black", yaxt="n")
lines(cities_sum $time[2:40], expo)
lines(cities_sum $time[2:40], exp(cities_sum $time[2:40]*0.071), col="red")

legend("topleft", inset=.05, legend=pretty10exp(round(lambda, digits=4)), title=expression(lambda))
#legend("topleft", inset=.05, legend=round(lambda, digits=4), title=expression(lambda))

#title(main="Infected", sub="time [days]",ylab="infected",cex.lab=1.5)
title(main="Infected", xlab= "time [days]",ylab="",cex.lab=1.5, cex.axis=2.0)
mtext("infected",side=2,line=5,cex = 1.5) #to set the label in the margin

eaxis(1)  # x-axis
eaxis(2,drop.1 = FALSE,las = 1 ) # (eje, colocar x1, orientacion)

dev.off()



