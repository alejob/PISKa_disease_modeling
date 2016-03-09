city<-read.table("base.simdir/Santiago.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))

#plot(city $time, city $cases, ann=F,col="red")
#points(city $time, city $susceptible, ann=F, col="green")
#points(city $time, city $exposed, ann=F,col="blue")
plot(city $time, city $exposed, ann=F,col="blue")
points(city $time, city $infected, ann=F, col="black")
#points(city $time, city $removed, ann=F, col="yellow")
#points(city $time, city $dead, ann=F, col="orange")

library(data.table)
CIT <- data.table(city)
toPlot<-CIT[, list(time, deadDiff=diff(dead),removeDiff=diff(removed))]
points(toPlot $time, toPlot $removeDiff, col="yellow")
points(toPlot $time, toPlot $deadDiff, col="orange")


#legend("topleft", inset=.05, title="State",c("cases","susceptible","exposed","infected","removed","dead"), fill=terrain.colors(6), horiz=FALSE)
legend("topleft", inset=.05, title="State",c("cases","susceptible","exposed","infected","removed","dead"), fill=c("red","green","blue","black","yellow","orange"), horiz=FALSE)

title(main="Cases evolution", sub="time", ylab="cases")

