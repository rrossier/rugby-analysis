file="~/Documents/OPEN DATA/RUGBY/6 Nations.csv"
data<-read.csv(file,sep=";",header=TRUE)
data$Year<-substr(as.character(data$Match.Date),start=(nchar(as.character(data$Match.Date))-3),nchar(as.character(data$Match.Date)))
Wales<-data[data$Team=="Wales",]
Ireland<-data[data$Team=="Ireland",]
France<-data[data$Team=="France",]
England<-data[data$Team=="England",]
Italy<-data[data$Team=="Italy",]
Scotland<-data[data$Team=="Scotland",]

france.matchs<-NULL;england.matchs<-NULL;ireland.matchs<-NULL
wales.matchs<-NULL;scotland.matchs<-NULL;italy.matchs<-NULL
years<-unique(France$Year)
max.diff=max(data$Diff)
dev.off()
par(bg="grey90",mfcol=c(6,14),mar=c(0.5,0.2,0.5,0.2),oma=c(3,2,1,1))
for(i in seq_along(years)){
  france.matchs[[years[i]]]<-France[France$Year==years[i],]
  england.matchs[[years[i]]]<-England[England$Year==years[i],]
  ireland.matchs[[years[i]]]<-Ireland[Ireland$Year==years[i],]
  wales.matchs[[years[i]]]<-Wales[Wales$Year==years[i],]
  scotland.matchs[[years[i]]]<-Scotland[Scotland$Year==years[i],]
  italy.matchs[[years[i]]]<-Italy[Italy$Year==years[i],]
  if(i==1){
    par(xaxt="n",yaxt="s")
  }
  else{
    par(xaxt="n",yaxt="n",mgp=c(3,1,0),mar=c(0.5,0.2,0.5,0.2))
  }
  col<-ifelse(france.matchs[[years[i]]]$Diff>0,"green4","red")
  plot(france.matchs[[years[i]]]$Diff,type="p",pch=15,lwd=2,col=col,ylim=c(-max.diff,max.diff))
  abline(h=0,lwd=1,lty=2)
  col<-ifelse(england.matchs[[years[i]]]$Diff>0,"green4","red")
  plot(england.matchs[[years[i]]]$Diff,type="p",pch=15,lwd=2,col=col,ylim=c(-max.diff,max.diff))
  abline(h=0,lwd=1,lty=2)
  col<-ifelse(ireland.matchs[[years[i]]]$Diff>0,"green4","red")
  plot(ireland.matchs[[years[i]]]$Diff,type="p",pch=15,lwd=2,col=col,ylim=c(-max.diff,max.diff))
  abline(h=0,lwd=1,lty=2)
  col<-ifelse(wales.matchs[[years[i]]]$Diff>0,"green4","red")
  plot(wales.matchs[[years[i]]]$Diff,type="p",pch=15,lwd=2,col=col,ylim=c(-max.diff,max.diff))
  abline(h=0,lwd=1,lty=2)
  col<-ifelse(scotland.matchs[[years[i]]]$Diff>0,"green4","red")
  plot(scotland.matchs[[years[i]]]$Diff,type="p",pch=15,lwd=2,col=col,ylim=c(-max.diff,max.diff))
  abline(h=0,lwd=1,lty=2);par(mar=c(1,0.2,0.5,0.2),mgp=c(0,1,0))
  col<-ifelse(italy.matchs[[years[i]]]$Diff>0,"green4","red")
  plot(italy.matchs[[years[i]]]$Diff,type="p",pch=15,lwd=2,col=col,ylim=c(-max.diff,max.diff),xlab=years[i],ylab="")
  abline(h=0,lwd=1,lty=2)
}
plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n");text(0,0,"france",cex=2)
plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n");text(0,0,"england",cex=2)
plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n");text(0,0,"ireland",cex=2)
plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n");text(0,0,"wales",cex=2)
plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n");text(0,0,"scotland",cex=2)
plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n");text(0,0,"italy",cex=2)

#Nb d'essais cumulés
dev.off()
par(bg="grey80",mar=c(5,4,2,0.5),oma=c(0,0,1,1))
plot(cumsum(England$Tries),type="o",pch=18,main="Nombre d'essais cumulés par équipe",xlab="matchs",ylab="tries",xaxt="n",col="white",lwd=2)
abline(v=seq(5,65,5),lty=2,lwd=1)
abline(h=seq(50,200,25),lty=2,lwd=1)
points(cumsum(France$Tries),col="blue",lwd=2,pch=18)
lines(cumsum(France$Tries),col="blue",lwd=2,pch=18)
points(cumsum(Ireland$Tries),col="green4",pch=18)
lines(cumsum(Ireland$Tries),col="green4",lwd=2)
points(cumsum(Wales$Tries),col="red",pch=18)
lines(cumsum(Wales$Tries),col="red",lwd=2)
points(cumsum(Scotland$Tries),col="navyblue",pch=18)
lines(cumsum(Scotland$Tries),col="navyblue",lwd=2)
points(cumsum(Italy$Tries),col="turquoise3",pch=18)
lines(cumsum(Italy$Tries),col="turquoise3",lwd=2)
legend("bottomright",bg="grey80",cex=0.6,pt.cex=3,pch=18,bty="o",x.intersp=0.5,y.intersp=0.5,box.col="grey80",horiz=TRUE,col=c("blue","white","green4","red","navyblue","turquoise3"),legend=c("France","England","Ireland","Wales","Scotland","Italy"))
axis(1,at=seq(2,62,5),labels=seq(2000,2012),tick=FALSE)

#Nb de points marqués cumulés
dev.off()
par(bg="grey80",mar=c(5,4,2,0.5),oma=c(0,0,1,1))
plot(cumsum(England$For),type="o",pch=18,main="Nombre de points marqués cumulés par équipe",xlab="matchs",ylab="points",xaxt="n",col="white",lwd=2)
abline(v=seq(5,65,5),lty=2,lwd=1)
abline(h=seq(200,1800,200),lty=2,lwd=1)
points(cumsum(France$For),col="blue",lwd=2,pch=18)
lines(cumsum(France$For),col="blue",lwd=2,pch=18)
points(cumsum(Ireland$For),col="green4",pch=18)
lines(cumsum(Ireland$For),col="green4",lwd=2)
points(cumsum(Wales$For),col="red",pch=18)
lines(cumsum(Wales$For),col="red",lwd=2)
points(cumsum(Scotland$For),col="navyblue",pch=18)
lines(cumsum(Scotland$For),col="navyblue",lwd=2)
points(cumsum(Italy$For),col="turquoise3",pch=18)
lines(cumsum(Italy$For),col="turquoise3",lwd=2)
legend("bottomright",bg="grey80",cex=0.6,pt.cex=3,pch=18,bty="o",x.intersp=0.5,y.intersp=0.5,box.col="grey80",horiz=TRUE,col=c("blue","white","green4","red","navyblue","turquoise3"),legend=c("France","England","Ireland","Wales","Scotland","Italy"))
axis(1,at=seq(2,62,5),labels=seq(2000,2012),tick=FALSE)

#Nb de points encaissés cumulés
dev.off()
par(bg="grey80",mar=c(5,4,2,0.5),oma=c(0,0,1,1))
plot(cumsum(Italy$Aga),type="o",pch=18,main="Nombre de points encaissés cumulés par équipe",xlab="matchs",ylab="points",xaxt="n",col="turquoise3",lwd=2)
abline(v=seq(5,65,5),lty=2,lwd=1)
abline(h=seq(200,2000,200),lty=2,lwd=1)
points(cumsum(England$Aga),col="white",lwd=2,pch=18)
lines(cumsum(England$Aga),col="white",lwd=2,pch=18)
points(cumsum(Ireland$Aga),col="green4",pch=18)
lines(cumsum(Ireland$Aga),col="green4",lwd=2)
points(cumsum(Wales$Aga),col="red",pch=18)
lines(cumsum(Wales$Aga),col="red",lwd=2)
points(cumsum(Scotland$Aga),col="navyblue",pch=18)
lines(cumsum(Scotland$Aga),col="navyblue",lwd=2)
points(cumsum(France$Aga),col="blue",pch=18)
lines(cumsum(France$Aga),col="blue",lwd=2)
legend("bottomright",bg="grey80",cex=0.6,pt.cex=3,pch=18,bty="o",x.intersp=0.5,y.intersp=0.5,box.col="grey80",horiz=TRUE,col=c("blue","white","green4","red","navyblue","turquoise3"),legend=c("France","England","Ireland","Wales","Scotland","Italy"))
axis(1,at=seq(3,63,5),labels=seq(2000,2012),tick=FALSE)

#Nb de points marqués cumulés histogramme
dev.off()
par(bg="grey80",mar=c(5,4,2,0.5),oma=c(0,0,1,1))
t<-cumsum(France$For)[seq(5,65,5)]
plot(t-c(0,t[1:12]),type="o",pch=18,main="Nombre de points marqués par équipe à chaque tournoi",xlab="tournois",ylab="points",xaxt="n",col="blue",lwd=2,ylim=c(42,250))
abline(v=seq(1,13),lty=2,lwd=1)
t<-cumsum(England$For)[seq(5,65,5)]
points(t-c(0,t[1:12]),col="white",lwd=2,pch=18)
lines(t-c(0,t[1:12]),col="white",lwd=2,pch=18)
t<-cumsum(Ireland$For)[seq(5,65,5)]
points(t-c(0,t[1:12]),col="green4",pch=18)
lines(t-c(0,t[1:12]),col="green4",lwd=2)
t<-cumsum(Wales$For)[seq(5,65,5)]
points(t-c(0,t[1:12]),col="red",pch=18)
lines(t-c(0,t[1:12]),col="red",lwd=2)
t<-cumsum(Scotland$For)[seq(5,65,5)]
points(t-c(0,t[1:12]),col="navyblue",pch=18)
lines(t-c(0,t[1:12]),col="navyblue",lwd=2)
t<-cumsum(Italy$For)[seq(5,65,5)]
points(t-c(0,t[1:12]),col="turquoise3",pch=18)
lines(t-c(0,t[1:12]),col="turquoise3",lwd=2)
legend("bottomright",bg="grey80",cex=0.6,pt.cex=3,pch=18,bty="n",x.intersp=0.5,y.intersp=0.5,box.col="grey80",horiz=TRUE,col=c("blue","white","green4","red","navyblue","turquoise3"),legend=c("France","England","Ireland","Wales","Scotland","Italy"))
axis(1,at=seq(1,13),labels=seq(2000,2012),tick=FALSE)

#Nb de points marqués cumulés histogramme
dev.off()
par(bg="grey80",mar=c(5,4,2,0.5),oma=c(0,0,1,1))
abline(h=seq(25,250,25),lty=2,lwd=1)
t<-cumsum(France$For)[seq(5,65,5)]
tFre<-t-c(0,t[1:12])
t<-cumsum(England$For)[seq(5,65,5)]
tEng<-t-c(0,t[1:12])
t<-cumsum(Wales$For)[seq(5,65,5)]
tWal<-t-c(0,t[1:12])
t<-cumsum(Scotland$For)[seq(5,65,5)]
tSco<-t-c(0,t[1:12])
t<-cumsum(Ireland$For)[seq(5,65,5)]
tIre<-t-c(0,t[1:12])
t<-cumsum(Italy$For)[seq(5,65,5)]
tIta<-t-c(0,t[1:12])
tdata<-matrix(c(tFre,tEng,tIre,tWal,tSco,tIta),nrow=6,byrow=TRUE)
barplot(tdata,beside=TRUE,col=c("blue","white","green4","red","navyblue","turquoise3"))
legend("topright",bg="grey80",cex=0.6,pt.cex=3,pch=18,bty="n",x.intersp=0.5,y.intersp=0.5,box.col="grey80",horiz=TRUE,col=c("blue","white","green4","red","navyblue","turquoise3"),legend=c("France","England","Ireland","Wales","Scotland","Italy"))
axis(1,at=seq(4,88,7),labels=seq(2000,2012),tick=FALSE)

