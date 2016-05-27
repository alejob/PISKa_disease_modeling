#read table and filter column "Cases"
read.table("dataTest.out", col.names=c("time", "cases", "susceptible", "exposed", "infected", "removed", "dead"))['cases']
