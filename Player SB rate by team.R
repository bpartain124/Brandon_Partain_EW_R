setwd("~/R/lahman/core")
Batting<-read.csv("Batting.csv")
people<-read.csv("People.csv")
##next I'm going to trim down the Batting data 
##to remove unused stats and pick one team
Batting.Team<-subset(Batting[c(
  "playerID","yearID","teamID","SB","CS")], 
  teamID=="HOU")
Team<-unique(Batting.Team$playerID)
SB.total<-function(pid){
     d<-subset(Batting.Team, playerID==pid)
     sum(d$SB)}
CS.total<-function(pid){
     e<-subset(Batting.Team,playerID==pid)
     sum(e$CS)}
SB.prct<-function(pid){
  f<-subset(Batting.Team,playerID==pid)
  sum(f$SB)/sum(sum(f$SB),sum(f$CS))}
SB.tot<-sapply(Team, SB.total)
CS.tot<-sapply(Team, CS.total)
SB.rate<-sapply(Team,SB.prct)
results=FALSE
options(digits=3)
M<-data.frame(Player=Team, SB=SB.tot, CS=CS.tot, SB_rate=SB.rate)
M<-merge(M, people[,c("playerID","nameFirst","nameLast","finalGame")] , 
         by.x = "Player", by.y = "playerID")
M<-subset(M,SB_rate<1)
mean(M$SB)
M<-subset(M,SB>(mean(SB)+0))
M<-M[order(M$SB_rate, decreasing=FALSE),]
results=TRUE
head(M)
