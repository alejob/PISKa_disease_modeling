require(pomp)
require(ggplot2)
require(plyr)
require(reshape2)
require(magrittr)
theme_set(theme_bw())
library(Matrix)

#pomp read "years" of the data
parus.dat <- read.csv(text="
                      year,P
                      1960,	148
1961,	258
1962,	185
1963,	170
1964,	267
1965,	239
1966,	196
1967,	132
1968,	167
1969,	186
1970,	128
1971,	227
1972,	174
1973,	177
1974,	137
1975,	172
1976,	119
1977,	226
1978,	166
1979,	161
1980,	199
1981,	306
1982,	206
1983,	350
1984,	214
1985,	175
1986,	211
1987,	212
1988,	213
1989,	214
1990,	215
1991,	216
1992,	217
1993,	218
1994,	219
1995,	220
1996,	221
1997,	222
1998,	223
1999,	224
2000,	225
2001,	226
2002,	227
2003,	228
2004,	229
2005,	230
2006,	231
2007,	232
2008,	233
2009,	234
2010,	235
2011,	236
2012,	237
2013,	238
2014,	239
2015,	240
2016,	241
2017,	242
2018,	243
2019,	244
2020,	245
2021,	246
2022,	247
2023,	248
2024,	249
2025,	250
2026,	251
2027,	252
2028,	253
2029,	254
2030,	255
2031,	256
2032,	257
2033,	258
2034,	259
2035,	260
2036,	261
2037,	262
2038,	263
2039,	264
2040,	265
2041,	266
2042,	267
2043,	268
2044,	269
2045,	270
2046,	271
2047,	272
2048,	273
2049,	274
2050,	275
2051,	276
2052,	277
2053,	278
2054,	279
2055,	280
2056,	281
2057,	282
2058,	283
2059,	284
"
                      )

parus.dat <- read.csv(text="
                      year,P
                      1960,0
                      1961,0
                      1962,0
                      1963,0
                      1964,0
                      1965,0
                      1966,0
                      1967,0
                      1968,0
                      1969,0
                      1970,0
                      1971,0
                      1972,0
                      1973,0
                      1974,0
                      1975,0
                      1976,0
                      1977,0
                      1978,0
                      1979,0
                      1980,0
                      1981,0
                      1982,0
                      1983,0
                      1984,0
                      1985,0
                      1986,0"
                      )
#this is not valid, because "year" is not increasing, pomp is using this data
parus.dat <- read.csv(text="
                      year,P
                      0000,0
                      0001,258
                      0002,185
                      0001,170
                      0001,207
                      0001,239
                      0000,000
                      0001,132
                      0001,107
                      0001,180"
                      )



ggplot(data=parus.dat,mapping=aes(x=year,y=P))+ geom_line()+geom_point()+ expand_limits(y=0)+ theme_classic()

step.fun <- Csnippet("
  double dW = rnorm(0,sqrt(dt));
  N += r*N*(1-N/K)*dt+sigma*N*dW;
")

#trying to reproduce in a R functino the above code
#test.fun <- function(c(1,2,3),10,c(5,10,15),0.3,...)
#test.fun <- function(xstart,times,params,delta.t,...)
test.fun <- function(x,t,params,delta.t,...)
 {dW <- rnorm(1, mean = 10, sd = 1)
 N <- N + r*N*(1-N/K)*dt+sigma*N*dW}

test.fun <- function(x,t,params,delta.t,...){ a<-t*t; return (a)}

parus <- pomp(data=parus.dat,time="year",t0=1959, rprocess=euler.sim(step.fun=step.fun,delta.t=1/305), statenames="N",paramnames=c("r","K","sigma"))

simStates <- simulate(parus,nsim=10,params=c(r=0.2,K=200,sigma=0.5,N.0=200),states=TRUE)

melt(simStates) %>% dcast(rep+time~variable) %>% ggplot(mapping=aes(x=time,y=N,group=rep,color=factor(rep)))+ geom_line()+guides(color=FALSE)+ theme_classic()


####function that return an array of rank-3 (dim(x)=3), with the numbers of elements demands by the example's data
test.fun <- function(xstart,times,params,...){ a<-seq(1,280,1.0) #if not 1.0, the numbers are integer
dim(a) <- c(1,10,28) #set dimension to tensor of rank-3
#dimnames(a)=list(1,seq(1,10),seq(1,28)) #gives name to rows
rownames(a)=c("row_name") #gives name to rows
return (a)}


#the statenames and paramnames are necessary only for csnippet
parus <- pomp(data=parus.dat,time="year",t0=1959, rprocess=test.fun, statenames="N",paramnames=c("r","K","sigma"))

parus <- pomp(data=parus.dat,time="year",t0=1959, rprocess=test.fun)

simStates <- simulate(parus,nsim=10,params=c(r=0.2,K=200,sigma=0.5,N.0=200),states=TRUE)

z<-seq(1,280)
dim(z)<-c(1,10,28)

#Creating function to run and call results inside R

test.fun <- function(xstart,times,params,...){
system("make")
 Santiago<<-read.table("Santiago.out", col.names=c("time", "cases", "susceptible", "exposed", "infected",
 "removed", "dead"))[c("infected")]
 nrow_stgo <- nrow(Santiago)
 #now I eliminate the dimension of the array
 Santiago <<- unlist(Santiago)
 #now I set the required dimension by rprocess
 dim(Santiago) <<- c(1,1,nrow_stgo)
 rownames(Santiago) <<-c("row_name") #gives name to rows
 return(Santiago)
 }

#testing output
test.fun <- function (x, t, params, delta.t, ... ) {
  dW <- rnorm(n = 1, mean = 0, sd = sqrt(delta.t))
  print(x["N"])
  N <- x["N"] + params["r"] * x["N"] * (1 - x["N"]/params["K"]) * delta.t + params["sigma"] * dW
  c(N=unname(N))
}

