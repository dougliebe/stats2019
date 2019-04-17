library(httr)
library(jsonlite)
library(lubridate)
library(RCurl)
library(dplyr)
library(googlesheets)
setwd("C:/Users/JP/Downloads/")


#################VEGAS############
#list sheets
gs_ls()

# get stats sheet
pull <- gs_title('Stats Inputting CSV Vegas')

# list worksheets
gs_ws_ls(pull)

#get data just from Sheet1
datavegas <- as.data.frame(gs_read(ss=pull, ws = "Sheet1"))
datavegas<-datavegas%>%
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

#data<-rbind(dataplq,datapl)
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

data <- data %>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  mutate(EngPlus=(kills+ifelse(!is.na(assists),assists,0)+deaths)/mf*100,
         APlus=ifelse(!is.na(assists),assists,0)/mf*100,
         dmgpr = damage.dealt/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         spr = player.score/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         dpr = deaths/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         apr = assists/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         kpr = kills/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         kdr = kills/deaths,
         fbpr = snd.firstbloods/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         fdpr = snd.firstdeaths/ifelse(snd.rounds == 0,duration..s.,snd.rounds))  %>%
  group_by(player) %>%
  mutate(cumaKD = cumsum(hek)/cumsum(hed)) %>% data.frame()

hpARPred <- readRDS('hpAR.rds')
sndARPred <- readRDS('sndAR.rds')
ctlARPred <- readRDS('ctlAR.rds')
hpSUBPred <- readRDS('hpSUB.rds')
sndSUBPred <- readRDS('sndSUB.rds')
ctlSUBPred <- readRDS('ctlSUB.rds')

ind.win <- function(mode, dmgpr, spr, dpr, apr, kpr, kdr, weapon){
  pred = ifelse(weapon == "Saug 9mm",ifelse(mode == "Hardpoint", predict(hpSUBPred, data.frame(apr, dpr, dmgpr), type = "response", allow.new.levels = T),
                                            ifelse(mode == "Search & Destroy", predict(sndSUBPred, data.frame(dmgpr,  kpr, apr), type = "response", allow.new.levels = T),
                                                   predict(ctlSUBPred, data.frame(kdr, spr, kpr), type = "response", allow.new.levels = T))),
                ifelse(mode == "Hardpoint", predict(hpARPred, data.frame(dpr, spr), type = "response", allow.new.levels = T),
                       ifelse(mode == "Search & Destroy", predict(sndARPred, data.frame(dmgpr,  kpr, apr,spr), type = "response", allow.new.levels = T),
                              predict(ctlARPred, data.frame(spr, dpr), type = "response", allow.new.levels = T))))
  return(pred)
}

data$pred <- mapply(ind.win, data$mode,data$dmgpr, data$spr, data$dpr, data$apr, data$kpr, data$kdr, data$fave.weapon)

# rating
data %>%
  # filter(mode == "Search & Destroy") %>%
  # filter(mode == "Hardpoint") %>%
  # filter(mode == "Control") %>%
  group_by(player, team) %>%
  summarise(Rating = mean(pred, na.rm = T)/0.5, maps = n()) %>%
  arrange(desc(Rating)) %>%
  data.frame()


#correct all engagements
# data<-data%>%
#   mutate(EngPlus=(kills+ifelse(!is.na(assists),assists,0)+deaths)/mf*100,
#          dmgpr = damage/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
#          spr = score1/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
#          dpr = deaths/ifelse(snd.rounds == 0,duration..s.,snd.rounds))
# 
# ind.win <- function(mode,dmgpr, spr, dpr){
#   pred = ifelse(mode == "Hardpoint", predict(hpPred, data.frame(spr, dpr), type = "response", allow.new.levels = T),
#                 ifelse(mode == "Search & Destroy", predict(sndPred, data.frame(dmgpr, dpr), type = "response", allow.new.levels = T),
#                        predict(ctlPred, data.frame(spr, dpr), type = "response", allow.new.levels = T)))
#   return(pred)
# }
# 
# data$pred <- mapply(ind.win, data$mode,data$damage, data$spr, data$dpr)
# m1 <- readRDS('overallWin.rds')
# data <- data %>%
#   group_by(match.id, team) %>%
#   mutate(teamwo = ((mean(pred)*5)-pred)/4,
#          team.min = min(pred),
#          teamsd = sd(pred),
#          wins_added = predict(m1, data.frame(pred, teamwo, teamsd),type = "response", allow.new.levels = T)-
#            predict(m1,data.frame(pred = 0.25, teamwo, teamsd),type = "response", allow.new.levels = T),
#          wins_over_team = predict(m1, data.frame(pred, teamwo, teamsd),type = "response", allow.new.levels = T)-
#            predict(m1,data.frame(pred = teamwo, teamwo, teamsd),type = "response", allow.new.levels = T),
#          rating_over_team = pred-teamwo) %>%
#   data.frame()
# 


respawnlb<-data%>%
  filter(mode=="Hardpoint"|mode=="Control")%>%
  group_by(match.id, team) %>%
  mutate(ptd = deaths/sum(deaths, na.rm = T)) %>%
  ungroup() %>%
  group_by(player,team) %>%
  dplyr::summarise(EPM = round(sum(EngPlus, na.rm = T)/
                                 (sum(duration..s.,na.rm =T))*60,6),
                   SPM = round(sum(player.score, na.rm = T)/
                                 (sum(duration..s.,na.rm =T))*60,6),
                   DPM = round(sum(damage.dealt, na.rm = T)/
                                 (sum(duration..s.,na.rm =T))*60,6),
                   DeathPM = round(sum(deaths, na.rm = T)/
                                     (sum(duration..s.,na.rm =T))*60,6),
                   RespawnKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T), 
                   PTD = mean(ptd, na.rm =T),
                   RespawnRating = mean(pred, na.rm = T)/0.5,
                   RespawnMaps=n())%>%
  dplyr::select(player,team,RespawnKD,SPM, DPM, EPM,PTD,DeathPM,RespawnRating,RespawnMaps)%>%
  arrange(desc(RespawnKD))%>%
  data.frame() 

hplb<-data%>%
  filter(mode=="Hardpoint")%>%
  group_by(match.id, team) %>%
  mutate(ptd = deaths/sum(deaths, na.rm = T)) %>%
  ungroup() %>%
  group_by(player,team) %>%
  summarise(EPM = round(sum(EngPlus, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            SPM = round(sum(player.score, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DPM = round(sum(damage.dealt, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DeathPM = round(sum(deaths, na.rm = T)/
                              (sum(duration..s.,na.rm =T))*60,6),
            HPKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T), 
            HPMaps=n(), 
            HPRating = mean(pred, na.rm = T)/0.5,
            PTD = mean(ptd, na.rm =T),
            HillTime=round(mean(hill.time..s.,na.rm = T),1))%>%
  select(player,team,HPKD,SPM,DPM,EPM,PTD,DeathPM,HillTime,HPRating,HPMaps)%>%
  arrange(desc(HPKD))%>%
  data.frame()

sndlb<-data%>%
  filter(mode=="Search & Destroy")%>%
  group_by(player,team) %>%
  summarise(EPR = round(sum(c(kills,assists,deaths), na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            SPR = round(sum(player.score, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            DPR = round(sum(damage.dealt, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            DeathPR = round(sum(deaths, na.rm = T)/
                              (sum(snd.rounds,na.rm =T)),6),
            SnDKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T),
            SnDFB=round(sum(snd.firstbloods, na.rm = T)/sum(snd.rounds, na.rm =T),6),
            SnDFBFD=paste(sum(snd.firstbloods, na.rm = T),sum(snd.firstdeaths, na.rm = T),sep = ":"),
            SnDRating = mean(pred, na.rm = T)/0.5,
            SnDMaps=n())%>%
  select(player,team,SnDKD,SPR,DPR, EPR, DeathPR, SnDFB, SnDFBFD,SnDRating,SnDMaps) %>%
  arrange(desc(SnDKD)) %>%
  data.frame()

controllb <-data%>%
  filter(mode=="Control")%>%
  group_by(match.id, team) %>%
  mutate(ptd = deaths/sum(deaths, na.rm = T)) %>%
  ungroup() %>%
  group_by(player,team) %>%
  summarise(EPM = round(sum(EngPlus, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            SPM = round(sum(player.score, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DPM = round(sum(damage.dealt, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DeathPM = round(sum(deaths, na.rm = T)/
                              (sum(duration..s.,na.rm =T))*60,6),
            ControlKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T), 
            ControlMaps=n(),
            ControlRating = mean(pred, na.rm = T)/0.5,
            CLU = mean(ptd, na.rm = T)) %>%
  select(player,team,ControlKD,SPM, DPM,EPM,CLU, DeathPM,ControlRating,ControlMaps) %>%
  arrange(desc(ControlKD)) %>%
  data.frame()


overalllb <- data %>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(player,team) %>%
  summarise(KD = sum(kills, na.rm = T)/sum(deaths, na.rm = T),
            aKD = sum(hek, na.rm = T)/sum(hed, na.rm = T),
            Rating = mean(pred, na.rm = T)/0.5,
            Xfactor = sum(ifelse(win. == "W",hek,NA), na.rm = T)/
              sum(ifelse(win. == "W",hed,NA), na.rm = T)-
              sum(ifelse(win. == "L",hek,NA), na.rm = T)/
              sum(ifelse(win. == "L",hed,NA), na.rm = T),
            TotalMaps = n()) %>%
  select(player, team, KD, Rating,  aKD, Xfactor, TotalMaps) %>%
  arrange(desc(Rating))



playerlb<-respawnlb
playerlb<-merge(playerlb,sndlb,by = c("player","team"))
playerlb<-merge(playerlb,hplb,by=c("player","team"))
playerlb<-merge(playerlb,controllb,c("player","team"))
playerlb<-merge(playerlb,overalllb,c("player","team"))
playerlb<-playerlb%>%
  arrange(desc(aKD))


playerlb<-setNames(playerlb,c("Player","team",
                              "RespawnKD",
                              "RespawnSPM",
                              "RespawnDPM",
                              "RespawnEPM",
                              "RespawnPTD",
                              "RespawnDeathPM",
                              "RespawnRating",
                              "RespawnMaps",
                              "SnDKD",
                              "SnDSPR",
                              "SnDDPR",
                              "SnDEPR",
                              "SnDDeathPR",
                              "SnDFBPct",
                              'SnDFBFD',
                              'SnDRating',
                              "SnDMaps",
                              "HPKD",
                              "HPSPM",
                              "HPDPM",
                              "HPEPM",
                              "HPPTD",
                              "HPDeathPM",
                              "HillTime",
                              'HPRating',
                              'HPMaps',
                              'ControlKD',
                              'ControlSPM',
                              'ControlDPM',
                              'ControlEPM',
                              'CLU',
                              'ControlDeathPM',
                              "ControlRating",
                              "ControlMaps",
                              # "FavSpecialist",
                              "OverallKD",
                              "OverallRating",
                              "aKD",
                              "Xfactor",
                              "OverallMaps"))

playerlb %>%
  select(Player, team, HPRating,HPSPM, HPDeathPM, ControlRating ,ControlSPM, ControlDeathPM,
         SnDRating,SnDDPR, SnDSPR, OverallRating, aKD) %>%
  mutate(HPSPM = percent_rank(HPSPM),
         HPDeathPM = percent_rank(-HPDeathPM),
         ControlSPM = percent_rank(ControlSPM),
         ControlDeathPM = percent_rank(-ControlDeathPM),
         SnDDPR=percent_rank(SnDDPR),
         SnDSPR=percent_rank(SnDSPR)) %>%
  filter(Player == "Nagafen" | Player == "Sukry") %>%
  arrange(desc(OverallRating)) %>% head()


overallrecordlb<-data%>%
  group_by(match.id, team)%>%
  summarise(wins = ifelse(sum(win. == "W") > 1, 1, 0)) %>%
  ungroup() %>%
  group_by(team) %>%
  summarise(W = sum(wins), L = n()-W,
            wins = W/(W+L),
            WL=paste(W,"-",L," (",round((W/(W+L)),3),")",sep="")) %>%
  arrange(desc(wins))%>%
  select(team,WL)%>%
  data.frame()%>%
  na.omit()

hprecordlb<-data%>%
  filter(mode=="Hardpoint")%>%
  group_by(match.id, team)%>%
  summarise(wins = ifelse(sum(win. == "W") > 1, 1, 0)) %>%
  ungroup() %>%
  group_by(team) %>%
  summarise(W = sum(wins), L = n()-W,
            wins = W/(W+L),
            HPWL=paste(W,"-",L," (",round((W/(W+L)),3),")",sep="")) %>%
  arrange(desc(wins))%>%
  select(team,HPWL)%>%
  data.frame()

sndrecordlb<-data%>%
  filter(mode=="Search & Destroy")%>%
  group_by(match.id, team)%>%
  summarise(wins = ifelse(sum(win. == "W") > 1, 1, 0)) %>%
  ungroup() %>%
  group_by(team) %>%
  summarise(W = sum(wins), L = n()-W,
            wins = W/(W+L),
            SnDWL=paste(W,"-",L," (",round((W/(W+L)),3),")",sep="")) %>%
  arrange(desc(wins))%>%
  select(team,SnDWL)%>%
  data.frame()

controlrecordlb<-data%>%
  filter(mode=="Control")%>%
  group_by(match.id, team)%>%
  summarise(wins = ifelse(sum(win. == "W") > 1, 1, 0)) %>%
  ungroup() %>%
  group_by(team) %>%
  summarise(W = sum(wins), L = n()-W,
            wins = W/(W+L),
            ControlWL=paste(W,"-",L," (",round((W/(W+L)),3),")",sep="")) %>%
  arrange(desc(wins))%>%
  select(team,ControlWL)%>%
  data.frame()


teamhardpointlb<-data %>%
  filter(mode=="Hardpoint")%>%
  group_by(team, match.id) %>%
  summarise(total.kills=sum(kills,na.rm = T),total.deaths=sum(deaths,na.rm = T),
            total.score=sum(player.score,na.rm = T),
            total.damage=sum(damage.dealt,na.rm = T),
            total.points = mean(score, na.rm = T),
            duration=mean(duration..s.,na.rm = T))%>%
  ungroup() %>% 
  filter(total.damage != 0) %>%
  group_by(team) %>%
  summarise(HPKD = sum(total.kills)/sum(total.deaths),
            HPSPM = sum(total.score)/ sum(duration)*60,
            HPDPM = sum(total.damage)/sum(duration)*60,
            KillEff = (sum(total.points)/sum(total.kills))*5,
            HPMaps = n()) %>%
  arrange(desc(KillEff))%>%
  data.frame()

teamcontrollb<-data %>%
  filter(mode=="Control")%>%
  group_by(team, match.id) %>%
  summarise(total.kills=sum(kills,na.rm = T),total.deaths=sum(deaths,na.rm = T),
            total.score=sum(player.score,na.rm = T),
            total.damage=sum(damage.dealt,na.rm = T),
            total.points = mean(score, na.rm = T),
            total.rounds = mean(ctrl.rounds, na.rm = T),
            duration=mean(duration..s.,na.rm = T))%>%
  ungroup() %>% 
  filter(total.damage != 0) %>% group_by(team) %>%
  summarise(CTLRoundWin = sum(total.points)/sum(total.rounds),
            CTLKD = sum(total.kills)/sum(total.deaths),
            CTLSPM = sum(total.score)/ sum(duration)*60,
            CTLDPM = sum(total.damage)/sum(duration)*60,
            CTLRounds = sum(total.rounds)) %>%
  arrange(desc(CTLKD))%>%
  data.frame()




teamsndlb<-data %>%
  filter(mode=="Search & Destroy")%>%
  group_by(team, match.id) %>%
  summarise(opp.kills = max(opp.kills),
            opp.damage = max(opp.damage),
            total.kills=sum(kills,na.rm = T),
            total.deaths=sum(deaths,na.rm = T),
            total.score=sum(player.score,na.rm = T),
            total.damage=sum(damage.dealt,na.rm = T),
            total.points = mean(score, na.rm = T),
            total.rounds = mean(snd.rounds, na.rm = T),
            first.bloods = sum(snd.firstbloods, na.rm = T),
            first.deaths = sum(snd.firstdeaths, na.rm = T),
            duration=mean(duration..s.,na.rm = T))%>%
  ungroup() %>%
  filter(total.damage != 0) %>% group_by(team) %>%
  summarise(SNDRoundWin = sum(total.points)/sum(total.rounds),
            SNDKD = sum(total.kills)/sum(total.deaths),
            SNDSPR = sum(total.score)/ sum(duration)*60,
            SNDDPR = sum(total.damage)/sum(duration)*60,
            SNDFBRate = sum(first.bloods)/sum(total.rounds),
            SNDRounds = sum(total.rounds),
            SNDHeal=(sum(opp.damage)-sum(opp.kills)*150)/sum(opp.damage)) %>%
  arrange(desc(SNDRoundWin))%>%
  data.frame()


teamlb<-data.frame()
teamlb<-merge(overallrecordlb,hprecordlb,by="team")
teamlb<-merge(teamlb,sndrecordlb,by="team")
teamlb<-merge(teamlb,controlrecordlb,by="team")
teamlb<-merge(teamlb,teamhardpointlb,by="team")
teamlb<-merge(teamlb,teamcontrollb,by="team")
teamlb<-merge(teamlb,teamsndlb,by="team")
teamlb<-teamlb %>% rename(team = 'team')




playerlb$team <- replace(as.character(playerlb$team), playerlb$team == "Thieves", "100 Thieves")
playerlb$Player <- replace(as.character(playerlb$Player), playerlb$Player == "Felony", "Felo")
playerlb$Player <- replace(as.character(playerlb$Player), playerlb$Player == "Priestah", "Priestahh")



write.csv(playerlb, file = "TestPlayersFullDate.csv")
write.csv(teamlb, file = "TestteamFullDate.csv")


bracket_kd<-data%>%
  filter(startsWith(as.character(series.id),"champs")|startsWith(as.character(series.id),"plq-bracket"))%>%
  group_by(player,mode)%>%
  summarise(Kills=sum(kills, na.rm = T),Deaths=sum(deaths, na.rm = T),maps=n()-sum(is.na(Kills)))%>%
  arrange(desc(Kills))%>%
  data.frame()
pool_kd<-data%>%
  filter(startsWith(as.character(series.id),"pool"))%>%
  group_by(player,mode)%>%
  summarise(Kills=sum(kills, na.rm = T),Deaths=sum(deaths, na.rm = T),maps=n()-sum(is.na(Kills)))%>%
  arrange(desc(Kills))%>%
  data.frame()
bracket_kd<-merge(bracket_kd,vegas_bracket_kd,by.x=c("player","mode"),by.y=c("Player","Mode"))
pool_kd<-merge(pool_kd,vegas_pool_kd,by.x=c("player","mode"),by.y=c("Player","Mode"))

bracket_kd<-bracket_kd%>%
  mutate(Kills=Kills.x+Kills.y,Deaths=Deaths.x+Deaths.y,maps=maps.x+maps.y)%>%
  rename(kills=Kills,deaths=Deaths)%>%
  select(player,mode,kills,deaths,maps)%>%
  mutate(bracketkd=round(kills/deaths,2))

pool_kd<-pool_kd%>%
  mutate(Kills=Kills.x+Kills.y,Deaths=Deaths.x+Deaths.y,maps=maps.x+maps.y)%>%
  rename(kills=Kills,deaths=Deaths)%>%
  select(player,mode,kills,deaths,maps)%>%
  mutate(poolkd=round(kills/deaths,2))

lan_kd<-merge(bracket_kd,pool_kd,by=c("player","mode"))
lan_kd<-lan_kd%>%
  mutate(diff=bracketkd-poolkd)%>%
  select(player,mode,bracketkd,maps.x,poolkd,maps.y,diff)%>%
  arrange(desc(diff))%>%
  data.frame()


data%>%
  filter(mode=="Search & Destroy")%>%
  group_by(player)%>%
  summarize(seventeen=sum(player.score>=1700,na.rm=T),n=n()-sum(is.na(player.score)),pct=seventeen/n)%>%
  arrange(desc(seventeen))%>%
  data.frame()

data%>%
 #filter(startsWith(as.character(series.id),'pro-w5') | startsWith(as.character(series.id),'pro-w6'))%>%
  mutate(fave.specialist=ifelse(fave.specialist=="",NA,as.character(fave.specialist)))%>%
  filter(!is.na(fave.specialist))%>%
  group_by(player,team)%>%
  summarise(spec=as.character(Mode(fave.specialist)))%>%
  arrange(desc(spec))%>%
  data.frame()

data%>%
  filter(startsWith(as.character(series.id),'pro-'))%>%
  group_by(player)%>%
  summarise(Specialist=Mode(specialist),TK=sum(team.kills))%>%
  arrange(desc(TK))




# KNN Clustering ------
hplb<-hplb[,-c(1:2)]

hplbScaled<-scale(hplb)
fitK<-kmeans(hplbScaled,5)
fitK
plot(hplb,col=fitK$cluster)

k<-list()
for(i in 1:10){
  k[[i]]<-kmeans(hplbScaled,i)
}

betweenss_totss<-list()
for(i in 1:10){
  betweenss_totss[[i]]<-k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10,betweenss_totss,type="b",ylab="between ss/total ss",xlab="clusters (k)")