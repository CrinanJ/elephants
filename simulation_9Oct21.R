#' ---
#' title: Elephant camera trap simulation
#' author: Crinan Jarrett
#' date: 9 Oct 21
#' ---

library(runjags)
library(ggplot2)
library(reshape2)

# 5000 triggers over 200 cameras
cells<-200 #grid cells containing a camera
timepoints<- 60 #survey occasions

# process model: rainfall (season) and distance to village affect elephant abundance
rainfall<-scale(seq(-29,30,1)^2,center=TRUE)
dist<-scale(rnorm(200,20,10),center=TRUE)
N0<-0.1

Nt<-matrix(nrow=200,ncol=60)
N<-matrix(nrow=200,ncol=60)
for(i in 1:cells){
  for(t in 1:timepoints){
  Nt[i,t]<-exp(N0 + rainfall[t]*dist[i]) #linear model for elephant abundance
  N[i,t]<-rpois(1,Nt[i,t])}}

plot_N<-as.data.frame(N)
plot_N$dist<-dist
plot_N<-melt(plot_N,id.vars = "dist")
plot_N$timepoint<-rep(seq(1,60,1),each=200)
plot_N$rainfall<-rep(rainfall,each=200)
colnames(plot_N)[3]<-"N"
plot_N<-plot_N[,-2]
ggplot(plot_N,aes(x=dist,y=N,col=rainfall))+
  geom_point()+
  theme_bw()

#observation model
pc0<--1.5 #baseline per capita capture rate
pc1<--0.2 #decline in capture rate with rainfall

for(t in 1:timepoints){
pc[t]<-exp(pc0+pc1*rainfall[t])} #change of capture rate with rainfall

nc<-matrix(nrow=200,ncol=60)
for(i in 1:cells){
  for(t in 1:timepoints){
    nc[i,t]<-rbinom(1,N[i,t],1-exp(-pc[t])) #convert Poisson rate into prob
  }
}

# Mark-recapture part
ids<-80 #number of individuals we have IDd

history<-matrix(nrow=80,ncol=60)
locs<-matrix(nrow=80,ncol=60)

for(i in 1:ids){
  for(t in 1:timepoints){
    for(z in 1:nc[i,t]){
      history[z,i,t]<-rbinom(1,1,1-exp(-pc[t]))
    }
    
    locs[i,t]<- ifelse(history[i,t]==0,0,)
    }
}

camdist<-matrix(nrow=200,ncol=200)
for(i in 1:cells){
  for(j in 1:cells){
    camdist[i,j]<-j-i #matrix of camera distances
  }
}
#pc<- pc0 + pc1 * camdist

