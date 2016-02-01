city<-read.table("base.simdir/Santiago.out", col.names=c("time", "cases", "susceptible", "exposed", "infecte", "removed", "dead"))

plot(city $time, city $cases, ann=F)

title(main="Cases evolution", sub="time", ylab="cases")

