require(pomp)
require(ggplot2)
require(plyr)
require(reshape2)
require(magrittr)
theme_set(theme_bw())
library(Matrix)

parus.dat <- read.csv(text="
                      year,P
                      1960,148
                      1961,258
                      1962,185
                      1963,170
                      1964,267
                      1965,239
                      1966,196
                      1967,132
                      1968,167
                      1969,186
                      1970,128
                      1971,227
                      1972,174
                      1973,177
                      1974,137
                      1975,172
                      1976,119
                      1977,226
                      1978,166
                      1979,161
                      1980,199
                      1981,306
                      1982,206
                      1983,350
                      1984,214
                      1985,175
                      1986,211"
                      )

ggplot(data=parus.dat,mapping=aes(x=year,y=P))+ geom_line()+geom_point()+ expand_limits(y=0)+ theme_classic()

step.fun <- Csnippet("
  double dW = rnorm(0,sqrt(dt));
  N += r*N*(1-N/K)*dt+sigma*N*dW;
")

test.fun <- function(x,t,params,delta.t,...){ a<-t*t; return (a)}

parus <- pomp(data=parus.dat,time="year",t0=1959, rprocess=euler.sim(step.fun=step.fun,delta.t=1/365), statenames="N",paramnames=c("r","K","sigma"))

simStates <- simulate(parus,nsim=10,params=c(r=0.2,K=200,sigma=0.5,N.0=200),states=TRUE)

melt(simStates) %>% dcast(rep+time~variable) %>% ggplot(mapping=aes(x=time,y=N,group=rep,color=factor(rep)))+ geom_line()+guides(color=FALSE)+ theme_classic()


####function that return an array of rank-3 (dim(x)=3), with the numbers of elements demands by the example's data
test.fun <- function(xstart,times,params,...){ a<-seq(1,280,1.0) #if not 1.0, the numbers are integer
dim(a) <- c(1,10,28) #set dimension to tensor of rank-3
#dimnames(a)=list(1,seq(1,10),seq(1,28)) #gives name to rows
rownames(a)=c("row_name") #gives name to rows
return (a)}

parus <- pomp(data=parus.dat,time="year",t0=1959, rprocess=test.fun, statenames="N",paramnames=c("r","K","sigma"))




z<-seq(1,280)
dim(z)<-c(1,10,28)
