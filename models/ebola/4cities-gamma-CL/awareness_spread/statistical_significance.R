#In this way I call al the data
commandArgs <- function(...) "simdir-1000-normal"
source("simPlots_manyrun-30points.R")

#Now the analysis of variance with Kruskall-Wallis
#Kruskall-Wallis is used to analize variance between not normal data.
allCities_lambda<-c(lambdas_Santiago_vector, lambdas_Valparaiso_vector, lambdas_La_Serena_vector, lambdas_Rancagua_vector)
kruskal.test(allCities_lambda ~ as.factor(names(allCities_lambda)))
#Santiago vs other cities
kruskal.test(c(lambdas_Santiago_vector, lambdas_Valparaiso_vector) ~ as.factor(c(names(lambdas_Santiago_vector), names(lambdas_Valparaiso_vector))))
kruskal.test(c(lambdas_Santiago_vector, lambdas_La_Serena_vector) ~ as.factor(c(names(lambdas_Santiago_vector), names(lambdas_La_Serena_vector))))
kruskal.test(c(lambdas_Santiago_vector, lambdas_Rancagua_vector) ~ as.factor(c(names(lambdas_Santiago_vector), names(lambdas_Rancagua_vector))))

#Valparaiso vs other cities
kruskal.test(c(lambdas_Valparaiso_vector, lambdas_La_Serena_vector) ~ as.factor(c(names(lambdas_Valparaiso_vector), names(lambdas_La_Serena_vector))))
kruskal.test(c(lambdas_Valparaiso_vector, lambdas_Rancagua_vector) ~ as.factor(c(names(lambdas_Valparaiso_vector), names(lambdas_Rancagua_vector))))

#La Serena vs other cities
kruskal.test(c(lambdas_La_Serena_vector, lambdas_Rancagua_vector) ~ as.factor(c(names(lambdas_La_Serena_vector), names(lambdas_Rancagua_vector))))

#violin plot
library(vioplot)
vioplot(lambdas_Santiago_vector, lambdas_Valparaiso_vector, lambdas_La_Serena_vector, lambdas_Rancagua_vector, col="gray", ylim=c(0.07, 0.15), pchMed=18, lty=5, lwd=1, h=0.0015)

#test normality
#if p-value < 0.05 => we reject the null hypothesis (H_0: the distribution is normal), ie. the distribution is NOT normal
#not so useful, based on stats overflow comments
shapiro.test(lambdas_Santiago_vector)
shapiro.test(lambdas_Valparaiso_vector)
shapiro.test(lambdas_La_Serena_vector)
shapiro.test(lambdas_Rancagua_vector)

if (FALSE){
#Fitting a curve with nls()
#Santiago
dfSantiago <- data.frame(x = Santiago_sum$time[1:35], y = Santiago_sum$infected[1:35])
fitSantiago <- nls(y ~ a*exp(x*b), data = dfSantiago, start=c(a=1, b=0.001))
plot(Santiago_sum$time[1:35], Santiago_sum$infected[1:35])
lines(Santiago_sum$time[1:35], predict(fitSantiago))

#Valparaiso
dfValparaiso<- data.frame(x = Valparaiso_sum$time[1:35], y = Valparaiso_sum$infected[1:35])
fitValparaiso <- nls(y ~ a*exp(x*b), data = dfValparaiso, start=c(a=10, b=0.001))
plot(Valparaiso_sum$time[1:35], Valparaiso_sum$infected[1:35])
lines(Valparaiso_sum$time[1:35], predict(fitValparaiso))

#Rancagua
dfRancagua<- data.frame(x = Rancagua_sum$time[1:35], y = Rancagua_sum$infected[1:35])
fitRancagua <- nls(y ~ a*exp(x*b), data = dfRancagua, start=c(a=10, b=0.001))
plot(Rancagua_sum$time[1:35], Rancagua_sum$infected[1:35])
lines(Rancagua_sum$time[1:35], predict(fitRancagua))

#La_Serena
dfLa_Serena<- data.frame(x = La_Serena_sum$time[1:35], y = La_Serena_sum$infected[1:35])
fitLa_Serena <- nls(y ~ a*exp(x*b), data = dfLa_Serena, start=c(a=10, b=0.001))
plot(La_Serena_sum$time[1:35], La_Serena_sum$infected[1:35])
lines(La_Serena_sum$time[1:35], predict(fitLa_Serena))
}

