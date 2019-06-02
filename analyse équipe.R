library(RCurl)
library(XML)

id.player<-function(str){
  url="http://stats.espnscrum.com/statsguru/rugby/stats/analysis.html"
  url.fetch<-paste(url,"?search=",str,sep="")
  txt1=getURL(url.fetch)
  id="gurusearch_player"
  txt2<-strsplit(txt1,id)[[1]]
  txt3<-txt2[length(txt2)]
  start=gregexpr("<table",txt3)[[1]][1]
  end=gregexpr("</table>",txt3)[[1]][1]
  txt4=substr(txt3,start,end)
  start=gregexpr("/statsguru/rugby/player/",txt4)[[1]][1]+nchar("/statsguru/rugby/player/")
  end=gregexpr(".html",txt4)[[1]][1]-1
  txt5=substr(txt4,start,end)
  return(txt5)
}

id.player("kelleher")

scrap.url<-function(url,start=16){
  options(RCurlOptions = list(useragent = "zzzz"))
  webpage <- getURL(url)
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
  # Extract table header and contents
  tablehead <- xpathSApply(pagetree, "//*/table/thead/tr[@class='headlinks']/th", xmlValue)
  results <- xpathSApply(pagetree, "//*/table/tbody/tr[@class='data1']/td", xmlValue)
  # Convert character vector to dataframe
  if(start==16){
    start=grep("Profile",results,value=FALSE)+1
  }
  content <- as.data.frame(matrix(results[start:length(results)], ncol = 12, byrow = TRUE))
  # Clean up the results
  names(content) <- tablehead
  content<-content[,-c(8,12)]
  return(content)
}
all.matchs.id<-function(id){
  base.url="http://www.espnscrum.com/statsguru/rugby/player/"
  params="?class=1;template=results;type=player;view=match"
  url=paste(base.url,id,".html",params,sep="")
  data<-scrap.url(url)
  return(data)
}
tournoi.six.matchs.id<-function(id){
  base.url="http://www.espnscrum.com/statsguru/rugby/player/"
  params="?class=1;template=results;trophy=2;type=player;view=match"
  url=paste(base.url,id,".html",params,sep="")
  data<-scrap.url(url,start=31)
  return(data)
}

all.matchs.player<-function(str){
  id=id.player(str)
  matchs<-all.matchs.id(id)
  return(matchs)
}

tournoi.six.matchs.player<-function(str){
  id=id.player(str)
  matchs<-tournoi.six.matchs.id(id)
  return(matchs)
}

matchs.6.dusautoir<-tournoi.six.matchs.player("dusautoir")
matchs.dusautoir<-all.matchs.player("dusautoir")
nb.essais.all<-sum(as.numeric(matchs.dusautoir$Tries)-1)

#########
#england team=1
#wales team=4
#france team=9
#ireland team=3
#scotland team=2
#italy team=20
positions<-c(seq(15,9,-1),seq(1,8),seq(16,23))
team.wales<-read.csv("~/Documents/OPEN DATA/RUGBY/6 Nations/Wales.csv",sep=";",header=TRUE)
team.wales$pos=positions
setwd("~/Documents/OPEN DATA/RUGBY/6 Nations/")
for(i in 1:nrow(team.wales)){
  name=paste(team.wales[i,"firstname"],team.wales[i,"surname"],sep="+")
  data<-all.matchs.player(name)
  write.csv(data,paste("Wales/",team.wales[i,"surname"],".csv",sep=""))
}