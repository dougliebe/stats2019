library(httr)
library(jsonlite)
library(lubridate)
library(RCurl)
library(dplyr)
library(googlesheets)
setwd("C:/Users/JP/Downloads/")

######WWII Events#####
setwd('C:/Users/JP/Documents/codstats/wwii_structured/cwl-data/data/wwii_events')
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)
print(filenames)

output <- data.frame()

for (i in 1:(length(filenames))) {
  print(filenames[i])
  data <- read.csv(filenames[i])
  output <- rbind(output, data)
}
datawwii<-output%>%
  group_by(player,mode)%>%
  summarise(kills=sum(kills,na.rm = T),deaths=sum(deaths,na.rm=T))
datawwii$player <- replace(as.character(datawwii$player), datawwii$player == "Felony", "Felo")
datawwii$player <- replace(as.character(datawwii$player), datawwii$player == "Priestah", "Priestahh")
datawwii$player <- replace(as.character(datawwii$player), datawwii$player == "Alex", "Alexx")

#################VEGAS############
#list sheets
setwd("C:/Users/JP/Downloads/")

gs_ls()

# get stats sheet
pull <- gs_title('Stats Inputting CSV Vegas')

# list worksheets
gs_ws_ls(pull)

#get data just from Sheet1
datavegas <- as.data.frame(gs_read(ss=pull, ws = "Sheet1"))
datavegas<-datavegas%>%
  mutate(mode=ifelse(mode=="HP","Hardpoint",ifelse(mode=="SND","Search & Destroy","Control")))%>%
  group_by(player,mode)%>%
  summarise(kills=sum(kills,na.rm = T),deaths=sum(deaths,na.rm=T))


#############PLQ###############

url  <- "https://stats.gammaray.io/api/v1/report/pro-league-2019-qualifier/playermatches/"
# pro-league-2019-qualifier

raw <- getURL(url = url)
dataplq <- read.csv(text = raw)


###############Pro League###########

url  <- "https://stats.gammaray.io/api/v1/report/pro-league-2019/playermatches/"

raw <- getURL(url = url)
datapl <- read.csv (text = raw)
datapl$team <- ifelse(datapl$team == "ExcelerateGG", "Excelerate", as.character(datapl$team))
data_firstgame <- read.csv('head.csv')
data_firstgame$match.id <- as.factor(data_firstgame$match.id)
datapl <- rbind(datapl, data_firstgame)

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
  mutate(damage.dealt=damage)%>%
  select(-damage)
datapl<-datapl%>%
  group_by(player,mode)%>%
  summarise(kills=sum(kills,na.rm = T),deaths=sum(deaths,na.rm=T))
datapl$player <- replace(as.character(datapl$player), datapl$player == "Felony", "Felo")
datapl$player <- replace(as.character(datapl$player), datapl$player == "Priestah", "Priestahh")
datapl$player <- replace(as.character(datapl$player), datapl$player == "Alex", "Alexx")


############Fort Worth############

url  <- "https://stats.gammaray.io/api/v1/report/cwl-fortworth-2019/playermatches/"

raw <- getURL(url = url)
datafw <- read.csv (text = raw)

data_firstgame <- read.csv('head_fw.csv')
data_firstgame$match.id <- as.factor(data_firstgame$match.id)
datafw <- rbind(datafw, data_firstgame)



############merge tourney data############# 

data<-rbind(dataplq,datafw)
dataplqfw<-data%>%
  group_by(player,mode)%>%
  summarise(kills=sum(kills,na.rm = T),deaths=sum(deaths,na.rm=T))

dataplqfw$player <- replace(as.character(dataplqfw$player), dataplqfw$player == "Felony", "Felo")
dataplqfw$player <- replace(as.character(dataplqfw$player), dataplqfw$player == "Priestah", "Priestahh")
dataplqfw$player <- replace(as.character(dataplqfw$player), dataplqfw$player == "Alex", "Alexx")

tourney<-merge(datawwii,datavegas,by=c("player","mode"),all=T)
tourney[is.na(tourney)]<-0
tourney<-tourney%>%
  mutate(kills=kills.x+kills.y,deaths=deaths.x+deaths.y)%>%
  select(player,mode,kills,deaths)
tourney<-merge(tourney,dataplqfw,by=c("player","mode"),all=T)
tourney[is.na(tourney)]<-0
tourney<-tourney%>%
  mutate(kills=kills.x+kills.y,deaths=deaths.x+deaths.y)%>%
  select(player,mode,kills,deaths)

tourney<-tourney %>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(player) %>%
  mutate(KD = sum(kills, na.rm = T)/sum(deaths, na.rm = T),
            aKD = sum(hek, na.rm = T)/sum(hed, na.rm = T))%>%
  arrange(desc(player))


#########WWII League##########
setwd('C:/Users/JP/Documents/codstats/wwii_structured/cwl-data/data/wwii_proleague')
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)
print(filenames)

output <- data.frame()

for (i in 1:(length(filenames))) {
  print(filenames[i])
  data <- read.csv(filenames[i])
  output <- rbind(output, data)
}
datawwiipl<-output%>%
  group_by(player,mode)%>%
  summarise(kills=sum(kills,na.rm = T),deaths=sum(deaths,na.rm=T))
datawwiipl$player <- replace(as.character(datawwiipl$player), datawwiipl$player == "Felony", "Felo")
datawwiipl$player <- replace(as.character(datawwiipl$player), datawwiipl$player == "Priestah", "Priestahh")
datawwiipl$player <- replace(as.character(datawwiipl$player), datawwiipl$player == "Alex", "Alexx")
wwiipl<-datawwiipl%>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  mutate(KD = sum(kills, na.rm = T)/sum(deaths, na.rm = T),
         aKD = sum(hek, na.rm = T)/sum(hed, na.rm = T))%>%
  select(player,aKD)%>%
  arrange(desc(player))
wwiipl<-unique(wwiipl[1:2])
###################merge league data#############
league<-merge(datawwiipl,datapl,by=c("player","mode"),all=T)
league[is.na(league)]<-0
league<-league%>%
  mutate(kills=kills.x+kills.y,deaths=deaths.x+deaths.y)%>%
  select(player,mode,kills,deaths)

league<-league%>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(player) %>%
  mutate(KD = sum(kills, na.rm = T)/sum(deaths, na.rm = T),
         aKD = sum(hek, na.rm = T)/sum(hed, na.rm = T))%>%
  arrange(desc(player))


##################merge both##############
tourneyleague<-merge(tourney,league,by=c("player","mode"),all=T)
tourneyleague<-tourneyleague%>%
  mutate(diff=aKD.x-aKD.y)%>%
  select(player,diff)%>%
  arrange(desc(diff))
tourneyleague<-unique(tourneyleague[1:2])



###All Events##########
setwd('C:/Users/JP/Documents/codstats/wwii_structured/cwl-data/data/')
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)
print(filenames)

output <- data.frame()

for (i in 1:(length(filenames))) {
  print(filenames[i])
  data <- read.csv(filenames[i])
  data$event<-filenames[i]
  output <- rbind(output, data)
}
output$player <- replace(as.character(output$player), output$player == "Felony", "Felo")
output$player <- replace(as.character(output$player), output$player == "Priestah", "Priestahh")
output$player <- replace(as.character(output$player), output$player == "Alex", "Alexx")

output<-merge(output,wwiipl)
output<-output%>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  filter(!startsWith(as.character(series.id),'pro1-a')|!startsWith(as.character(series.id),'pro2-a')|
          !startsWith(as.character(series.id),'pro1-b')|!startsWith(as.character(series.id),'pro2-b'))%>%
  group_by(player,event) %>%
  mutate(aKD2 = sum(hek, na.rm = T)/sum(hed, na.rm = T),
         plusminus=aKD-aKD2)%>%
  arrange(desc(player))

output%>%
  group_by(player)%>%
  summarise(pmavg=round(mean(plusminus),2))%>%
  arrange(desc(pmavg))%>%
  data.frame()