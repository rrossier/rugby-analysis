file="~/Documents/OPEN DATA/RUGBY/6 Nations.csv"
data<-read.csv(file,sep=";",header=TRUE)
Wales<-data[data$Team=="Wales",]
Ireland<-data[data$Team=="Ireland",]
France<-data[data$Team=="France",]
England<-data[data$Team=="England",]
Italy<-data[data$Team=="Italy",]
Scotland<-data[data$Team=="Scotland",]