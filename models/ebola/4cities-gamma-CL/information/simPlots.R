Santiago<-read.table("base.simdir/Santiago.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
Valparaiso <-read.table("base.simdir/Valparaiso.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
Rancagua <-read.table("base.simdir/Rancagua.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))
La_Serena <-read.table("base.simdir/La_Serena.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))

cities <- Santiago + Valparaiso + Rancagua + La_Serena
write.table(cities,"base.simdir/totalCities.out")

plot(cities $time, cities $cases, ann=F,col="red")
points(cities $time, cities $susceptible, ann=F, col="green")
points(cities $time, cities $exposed, ann=F,col="blue")
points(cities $time, cities $infected, ann=F, col="black")
points(cities $time, cities $removed, ann=F, col="yellow")
points(cities $time, cities $dead, ann=F, col="orange")

legend("topleft", inset=.05, title="State",c("E+I+R+D","susceptible","exposed","infected","removed","dead"), fill=c("red","green","blue","black","yellow","orange"), horiz=FALSE)

title(main="States", sub="time", ylab="cases")

