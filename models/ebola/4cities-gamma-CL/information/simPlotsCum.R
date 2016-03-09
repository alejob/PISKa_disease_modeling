#city<-read.table("base.simdir/Santiago.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))


Santiago<-read.table("base.simdir/Santiago.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))                                
Valparaiso <-read.table("base.simdir/Valparaiso.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))                           
Rancagua <-read.table("base.simdir/Rancagua.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))                               
La_Serena <-read.table("base.simdir/La_Serena.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead")) 

cities <- Santiago + Valparaiso + Rancagua + La_Serena                              
write.table(cities,"totalCities.out")

library(data.table)
CIT <- data.table(cities)
#toPlot<-CIT[, list(time, deadDiff=diff(dead),removeDiff=diff(removed), infected=cumsum(infected),exposed=cumsum(exposed))]
toPlot<-CIT[, list(time, dead,removed,deadDiff=diff(dead),removeDiff=diff(removed), infectedDiff=diff(infected),exposedDiff=diff(exposed),cumIncidence=cumsum(diff(susceptible)*-1), aux2=cumsum(diff(exposed)) , aux3=diff(dead)+diff(removed)+diff(infected), susceptibleDiff=diff(susceptible)*-1, cases)]

#plot(toPlot $time, toPlot $removeDiff, ann=F,col="yellow" )
#points(toPlot $time, toPlot $deadDiff, ann=F, col="orange")
#plot(city $time, toPlot $exposed, ann=F,col="blue")
#points(city $time, toPlot $infected, ann=F, col="black")

#plot(city $time, toPlot $exposedDiff, ann=F,col="blue")
#points(city $time, toPlot $infectedDiff, ann=F, col="black")

plot(cities $time, toPlot $cumIncidence, ann=F,col="blue")
points(toPlot $time, toPlot $dead, ann=F, col="orange")
points(toPlot $time, toPlot $removed, ann=F, col="red")
#points(toPlot $time, toPlot $cases, ann=F, col="black")

#plot(city $time, toPlot $aux2, ann=F, col="black")

#points(city $time, toPlot $susceptibleDiff, ann=F, col="black")
#points(city $time, toPlot $aux3, ann=F, col="red")

#points(city $time, toPlot $aux1, ann=F,col="blue")
#points(city $time, toPlot $exposedDiff, ann=F, col="red")


legend("topleft", inset=.05, title="Cumulative",c("incidence","removed","dead"), fill=c("blue","red","orange"), horiz=FALSE)

title(main="Cumulative cases", sub="time", ylab="cases")

