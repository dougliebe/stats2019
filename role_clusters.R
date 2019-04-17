library(httr)
library(jsonlite)
library(lubridate)
library(RCurl)
library(dplyr)
library(googlesheets)
setwd("C:/Users/JP/Downloads/")

###############Pro League###########

url  <- "https://stats.gammaray.io/api/v1/report/pro-league-2019/playermatches/"

raw <- getURL(url = url)
datapl <- read.csv (text = raw)
datapl$team <- ifelse(datapl$team == "ExcelerateGG", "Excelerate", as.character(datapl$team))
data_firstgame <- read.csv('head.csv')
data_firstgame$match.id <- as.factor(data_firstgame$match.id)
datapl <- rbind(datapl, data_firstgame)

datapl$team <- replace(as.character(datapl$team), datapl$team == "Thieves", "100 Thieves")
datapl$player <- replace(as.character(datapl$player), datapl$player == "Felony", "Felo")
datapl$player <- replace(as.character(datapl$player), datapl$player == "Priestah", "Priestahh")

#list sheets
gs_ls()

# get stats sheet
pull <- gs_title('Pro League Score Damage')

# list worksheets
gs_ws_ls(pull)

#get data just from Sheet1
data_dmgscore <- as.data.frame(gs_read(ss=pull, ws = "Sheet2"))
data_dmgscore$match.id <- as.factor(data_dmgscore$match.id)

# data$match.id <- as.numeric(data$match.id)
datapl <- merge(datapl, data_dmgscore, by = c('match.id', 'player', 'team'), all.x = T)
datapl <- datapl%>%
  select(-score1)%>%
  mutate(damage.dealt=ifelse(is.na(damage),damage.dealt,damage))%>%
  select(-damage)


############Fort Worth############

url  <- "https://stats.gammaray.io/api/v1/report/cwl-fortworth-2019/playermatches/"

raw <- getURL(url = url)
datafw <- read.csv (text = raw)

data_firstgame <- read.csv('head_fw.csv')
data_firstgame$match.id <- as.factor(data_firstgame$match.id)
datafw <- rbind(datafw, data_firstgame)
datafw$team <- replace(as.character(datafw$team), datafw$team == "Thieves", "100 Thieves")
datafw$player <- replace(as.character(datafw$player), datafw$player == "Felony", "Felo")
datafw$player <- replace(as.character(datafw$player), datafw$player == "Priestah", "Priestahh")



#merge all data

data<-rbind(datapl,datafw)


#merge mapfactor
mapfactor <- read.csv('mapfactor.csv')[,-1]
data<-merge(data,mapfactor,by=c('mode', 'map'),all.x = TRUE)


#return functions
returnOpp <- function(id, team) {
  data <- data #change this second variable to whatever your dataframe is called
  opp <- unique(data[data$match.id == id & data$team != team, ]$team)[1]
  return(opp)
}

returnOppScore <- function(id, team) {
  data <- data #change this second variable to whatever your dataframe is called
  score <- unique(data[data$match.id == id & data$team != team, ]$score)[1]
  return(score)
}

returnOppKills <- function(id, team) {
  data <- data #change this second variable to whatever your dataframe is called
  kills <- sum(data[data$match.id == id & data$team != team, ]$kills)
  return(kills)
}

data$opponent <- mapply(returnOpp,data$match.id, data$team)
data$opp.score <- mapply(returnOppScore,data$match.id, data$team)
data$opp.kills <- mapply(returnOppKills,data$match.id, data$team)

returnOppDamage <-function(id, team) {
  data <- data #change this second variable to whatever your dataframe is called
  damage <- sum(data[data$match.id == id & data$team != team, ]$damage)
  return(damage)
}

data$opp.damage <- mapply(returnOppDamage,data$match.id, data$team)
data <- data %>%
  mutate(damage.dealt = ifelse(damage.dealt==0, NA, damage.dealt))


Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam) & length(ta)>1)
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod[1])
}

#merge mf#merge mf
#mapfactor <- read.csv('mapfactor.csv')[,-1]
#data<-merge(data,mapfactor,by=c('mode', 'map'),all.x = TRUE)
#Cluster KNN -----------
hpdata <- data %>%
  filter(!is.na(assists))%>%
  filter(mode=="Hardpoint")%>%
  mutate(EPM=((kills+assists+deaths)/mf*100)*60/(duration..s.),
         DPM = (damage.dealt*60)/(duration..s.),
         SPM = (player.score*60)/(duration..s.),
         DeathPM = (deaths*60)/(duration..s.),
         HillTime=(hill.time..s.*60)/(duration..s.),
         APM = (assists*60)/(duration..s.),
         KPM = (kills*60)/(duration..s.))%>%
  select(player,EPM,DPM,SPM,DeathPM,HillTime,APM,KPM)

#Clustering Kmeans------
hpcluster<-data%>%
  filter(mode=="Hardpoint")%>%
  group_by(player,team) %>%
  summarise(EPM = round(sum(EngPlus, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            SPM = round(sum(player.score, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DPM = round(sum(damage.dealt, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DeathPM = round(sum(deaths, na.rm = T)/
                              (sum(duration..s.,na.rm =T))*60,6),
            HillTime=round(mean(hill.time..s.,na.rm = T),1),
            APM=round(sum(assists,na.rm = T)/(sum(duration..s.,na.rm =T))*60,6),
            KPM=round(sum(kills,na.rm=T)/(sum(duration..s.,na.rm =T))*60,6))%>%
  select(SPM,DPM,EPM,DeathPM,HillTime,APM,KPM)%>%
  arrange(desc(SPM))%>%
  data.frame()

hpcluster<-hpcluster[,-1]
hpclusterScaled<-scale(hpcluster)

fitK<-kmeans(hpclusterScaled,5)
fitK
plot(hpcluster,col=fitK$cluster)

k<-list()
for(i in 1:10){
  k[[i]]<-kmeans(hpclusterScaled,i)
}

betweenss_totss<-list()
for(i in 1:10){
  betweenss_totss[[i]]<-k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10,betweenss_totss,type="b",ylab="between ss/total ss",xlab="clusters (k)")
