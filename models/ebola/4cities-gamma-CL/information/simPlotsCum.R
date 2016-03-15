Santiago<-read.table("base.simdir/Santiago.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))                                
Valparaiso <-read.table("base.simdir/Valparaiso.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))                           
Rancagua <-read.table("base.simdir/Rancagua.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))                               
La_Serena <-read.table("base.simdir/La_Serena.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead")) 

cities <- Santiago + Valparaiso + Rancagua + La_Serena                              
write.table(cities,"base.simdir/totalCities.out")

library(data.table)
CIT <- data.table(cities)
toPlot<-CIT[, list(time, dead,removed,deadDiff=diff(dead),removeDiff=diff(removed), infectedDiff=diff(infected),exposedDiff=diff(exposed),cumIncidence=cumsum(diff(susceptible)*-1), aux2=cumsum(diff(exposed)) , aux3=diff(dead)+diff(removed)+diff(infected), susceptibleDiff=diff(susceptible)*-1, cases)]

plot(cities $time, toPlot $cumIncidence, ann=F,col="blue")
points(toPlot $time, toPlot $dead, ann=F, col="orange")
points(toPlot $time, toPlot $removed, ann=F, col="red")

legend("topleft", inset=.05, title="Cumulative",c("incidence","removed","dead"), fill=c("blue","red","orange"), horiz=FALSE)

title(main="Cumulative cases", sub="time", ylab="cases")

