library(httr)
library(jsonlite)
library(lubridate)
library(RCurl)
library(dplyr)
library(googlesheets)
setwd("C:/Users/JP/Downloads/")

url  <- "https://stats.gammaray.io/api/v1/report/cwl-fortworth-2019/playermatches/"

raw <- getURL(url = url)
data <- read.csv (text = raw)

data_firstgame <- read.csv('head_fw.csv')
data_firstgame$match.id <- as.factor(data_firstgame$match.id)
data <- rbind(data, data_firstgame)

data<-merge(data,mapfactor,by=c('mode', 'map'),all.x = TRUE)

data<-data%>%
  mutate(EngPlus=(kills+ifelse(!is.na(assists),assists,0)+deaths)/mf*100)%>%
#  mutate(APlus=ifelse(!is.na(assists),assists,0)/mf*100)

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
  damage.dealt <- sum(data[data$match.id == id & data$team != team, ]$damage.dealt)
  return(damage.dealt)
}

data$opp.damage <- mapply(returnOppDamage,data$match.id, data$team)

data$player <- replace(as.character(data$player), data$player == "Priestahh", "Priestah")


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
         dmgpr = damage.dealt/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         spr = player.score/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         dpr = deaths/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         fbpr = snd.firstbloods/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         fdpr = snd.firstdeaths/ifelse(snd.rounds == 0,duration..s.,snd.rounds))  %>%
  group_by(player) %>%
  mutate(cumaKD = cumsum(hek)/cumsum(hed)) %>% data.frame()

hpPred <- readRDS('hpPred.rds')
sndPred <- readRDS('sndPred.rds')
ctlPred <- readRDS('ctlPred.rds')

ind.win <- function(mode, dmgpr, spr, dpr, fbpr, fdpr){
  pred = ifelse(mode == "Hardpoint", predict(hpPred, data.frame(spr, dpr), type = "response", allow.new.levels = T),
                ifelse(mode == "Search & Destroy", predict(sndPred, data.frame(dmgpr,  fbpr, fdpr), type = "response", allow.new.levels = T),
                       predict(ctlPred, data.frame(spr, dpr), type = "response", allow.new.levels = T)))
  return(pred)
}

data$pred <- mapply(ind.win, data$mode,data$dmgpr, data$spr, data$dpr, data$fbpr, data$fdpr)

# rating
data %>%
  # filter(mode == "Search & Destroy") %>%
  # filter(mode == "Hardpoint") %>%
  # filter(mode == "Control") %>%
  group_by(player, team) %>%
  summarise(Rating = mean(pred)/0.5, maps = n()) %>%
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
                                 (sum(duration..s.,na.rm =T))*60),
                   DPM = round(sum(damage.dealt, na.rm = T)/
                                 (sum(duration..s.,na.rm =T))*60),
                   DeathPM = round(sum(deaths, na.rm = T)/
                                     (sum(duration..s.,na.rm =T))*60,6),
                   RespawnKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T), 
                   PTD = mean(ptd, na.rm =T),
                   RespawnRating = median(pred)/0.5,
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
                          (sum(duration..s.,na.rm =T))*60),
            DPM = round(sum(damage.dealt, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60),
            DeathPM = round(sum(deaths, na.rm = T)/
                              (sum(duration..s.,na.rm =T))*60,6),
            HPKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T), 
            HPMaps=n(), 
            HPRating = median(pred)/0.5,
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
                          (sum(snd.rounds,na.rm =T))),
            DPR = round(sum(damage.dealt, na.rm = T)/
                          (sum(snd.rounds,na.rm =T))),
            DeathPR = round(sum(deaths, na.rm = T)/
                              (sum(snd.rounds,na.rm =T)),6),
            SnDKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T),
            SnDFB=round(sum(snd.firstbloods, na.rm = T)/sum(snd.rounds, na.rm =T),6),
            SnDFBFD=paste(sum(snd.firstbloods, na.rm = T),sum(snd.firstdeaths, na.rm = T),sep = ":"),
            SnDRating = median(pred)/0.5,
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
                          (sum(duration..s.,na.rm =T))*60,2),
            SPM = round(sum(player.score, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60),
            DPM = round(sum(damage.dealt, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60),
            DeathPM = round(sum(deaths, na.rm = T)/
                              (sum(duration..s.,na.rm =T))*60,6),
            ControlKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T), 
            ControlMaps=n(),
            ControlRating = median(pred)/0.5,
            CLU = mean(ptd, na.rm = T)) %>%
  select(player,team,ControlKD,SPM, DPM,EPM,CLU, DeathPM,ControlRating,ControlMaps) %>%
  arrange(desc(ControlKD)) %>%
  data.frame()


overalllb <- data %>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(player, team) %>%
  summarise(KD = sum(kills, na.rm = T)/sum(deaths, na.rm = T),
            aKD = sum(hek, na.rm = T)/sum(hed, na.rm = T),
            Rating = mean(pred)/0.5,
            Xfactor = sum(ifelse(win. == "W",hek,NA), na.rm = T)/
              sum(ifelse(win. == "W",hed,NA), na.rm = T)-
              sum(ifelse(win. == "L",hek,NA), na.rm = T)/
              sum(ifelse(win. == "L",hed,NA), na.rm = T),
            TotalMaps = n()) %>%
  select(player, team, KD, Rating,  aKD, Xfactor, TotalMaps) %>%
  arrange((Rating))

overalllb <- merge(overalllb, xfactor, by = c('player', 'team')) %>%
  select(player, team, KD, Rating,  aKD, Xfactor, TotalMaps)


playerlb<-respawnlb
playerlb<-merge(playerlb,sndlb,by = c("player","team"))
playerlb<-merge(playerlb,hplb,by=c("player","team"))
playerlb<-merge(playerlb,controllb,c("player","team"))
playerlb<-merge(playerlb,overalllb,c("player","team"))
playerlb<-playerlb%>%
  arrange(desc(aKD))


playerlb<-setNames(playerlb,c("Player","Team",
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
  select(Player, Team, HPRating,HPSPM, HPDeathPM, ControlRating ,ControlSPM, ControlDeathPM,
         SnDRating,SnDDPR, SnDFBPct, OverallRating, aKD) %>%
  mutate(HPSPM = percent_rank(HPSPM),
         HPDeathPM = percent_rank(-HPDeathPM),
         ControlSPM = percent_rank(ControlSPM),
         ControlDeathPM = percent_rank(-ControlDeathPM),
         SnDDPR=percent_rank(SnDDPR),
         SnDFBPct=percent_rank(SnDFBPct)) %>%
  # filter(Team == "Reciprocity" | Team == "Splyce" | Team == "Splyce") %>%
  arrange(desc(OverallRating))


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
  ungroup() %>% group_by(team) %>%
  summarise(HPKD = sum(total.kills)/sum(total.deaths),
            HPSPM = sum(total.score)/ sum(duration)*60,
            HPDPM = sum(total.damage)/sum(duration)*60,
            KillEff = (sum(total.points)/sum(total.kills))*5,
            HPMaps = n()) %>%
  arrange(desc(HPKD))%>%
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
  ungroup() %>% group_by(team) %>%
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
  ungroup() %>% group_by(team) %>%
  summarise(SNDRoundWin = sum(total.points)/sum(total.rounds),
            SNDKD = sum(total.kills)/sum(total.deaths),
            SNDSPR = sum(total.score)/ sum(duration)*60,
            SNDDPR = sum(total.damage)/sum(duration)*60,
            SNDFBRate = sum(first.bloods)/sum(total.rounds),
            SNDRounds = sum(total.rounds),
            SNDHeal=(sum(opp.damage)-sum(opp.kills)*150)/sum(opp.damage)) %>%
  arrange(desc(SNDKD))%>%
  data.frame()


teamlb<-data.frame()
teamlb<-merge(overallrecordlb,hprecordlb,by="team")
teamlb<-merge(teamlb,sndrecordlb,by="team")
teamlb<-merge(teamlb,controlrecordlb,by="team")
teamlb<-merge(teamlb,teamhardpointlb,by="team")
teamlb<-merge(teamlb,teamcontrollb,by="team")
teamlb<-merge(teamlb,teamsndlb,by="team")
teamlb<-teamlb %>% rename(Team = 'team')



playerlb$Team <- replace(as.character(playerlb$Team), playerlb$Team == "Thieves", "100 Thieves")
playerlb$Player <- replace(as.character(playerlb$Player), playerlb$Player == "Felony", "Felo")
playerlb$Player <- replace(as.character(playerlb$Player), playerlb$Player == "Priestah", "Priestahh")


write.csv(playerlb, file = "TestPlayerLeaderboardFW.csv")
write.csv(teamlb, file = "TestTeamLeaderboardFW.csv")


respawnlb%>%
  #filter(team!="Gen.G"&team!="Midnight"&team!="OpTic Gaming"&team!="Red Reserve"&team!="UYU"&team!="Luminosity"&team!="Reciprocity"&team!="Evil Geniuses")%>%
  select(player,team,DPM,RespawnKD,RespawnMaps)%>%
  arrange(desc(DPM))

respawnlb%>%
  select(player,team,SPM,RespawnKD,RespawnMaps)%>%
  arrange(desc(SPM))

respawnlb%>%
  select(player,team,DPM,RespawnKD,RespawnMaps)%>%
  arrange(desc(RespawnKD))

hplb%>%
  select(player,team,DPM,HPKD,HPMaps)%>%
  arrange(desc(DPM))

hplb%>%
  select(player,team,SPM,HPKD,HPMaps)%>%
  arrange(desc(SPM))

hplb%>%
  select(player,team,DPM,HPKD,HPMaps)%>%
  arrange(desc(HPKD))

controllb%>%
  select(player,team,DPM,ControlKD,ControlMaps)%>%
  arrange(desc(ControlKD))

controllb%>%
  select(player,team,SPM,ControlKD,ControlMaps)%>%
  arrange(desc(SPM))

sndlb%>%
  # filter(team!="Gen.G"&team!="Midnight"&team!="OpTic Gaming"&team!="Red Reserve"&team!="UYU"&team!="Luminosity"&team!="Reciprocity"&team!="Evil Geniuses")%>%
  select(player,team,DPR,SnDKD,SnDMaps)%>%
  arrange(desc(DPR))

sndlb%>%
  select(player,team,SPR,SnDKD,SnDMaps)%>%
  arrange(desc(SPR))

sndlb%>%
  select(player,team,DPR,SnDKD,SnDMaps)%>%
  arrange(desc(SnDKD))


#single map damage
data%>%
  filter(mode=="Hardpoint")%>%
  group_by(series.id,match.id,player,team,mode,map,opponent,damage.dealt)%>%
  #summarise(damage=damage.dealt*60/duration..s.)%>%
  select(player,team,opponent,mode,map,damage.dealt)%>%
  arrange(desc(damage.dealt))
#kill record
data%>%
  group_by(match.id,player,team,opponent,kills)%>%
  summarise(k=kills)%>%
  arrange(desc(k))

#single series aKD
data%>%
  group_by(series.id,player,team,opponent)%>%
mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(player, team,opponent) %>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),
            aKD = round(sum(hek, na.rm = T)/sum(hed, na.rm = T),2),
            EPMov = round(sum(EngPlus, na.rm = T)/sum(duration..s., na.rm = T), 3),
            TotalMaps = n()) %>%
  arrange(desc(aKD))



fwaKD<-data %>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(player, team) %>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),
            aKD = round(sum(hek, na.rm = T)/sum(hed, na.rm = T),2),
            EPMov = round(sum(EngPlus, na.rm = T)/sum(duration..s., na.rm = T), 3),
            TotalMaps = n()) %>%
  arrange(desc(aKD))

aKDdiff<-merge(plaKD,fwaKD,by = c("player","team"))
aKDdiff%>%
  mutate(diff=aKD.y-aKD.x)%>%
  arrange(desc(diff))%>%
  data.frame()


data%>%
  #filter(fave.weapon=="Rampart 17")%>%
 # filter(startsWith(as.character(series.id),'pro-w5'))%>%
  filter(mode=="Search & Destroy")%>%
  group_by(fave.weapon)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),kills=sum(kills, na.rm = T),deaths=sum(deaths, na.rm = T),map=n())%>%
  # summarise(DPM = round(sum(damage.dealt, na.rm = T)/
  # (sum(duration..s.,na.rm =T))*60),duration=sum(duration..s.))%>%
  arrange(desc(KD))%>%
  data.frame



data %>%
  filter(player=="Classic") %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  filter(mode!="Search & Destroy")%>%
 # filter(X...>=0) %>%
  group_by(player,fave.weapon) %>%
  summarise(wins = mean(win), W = sum(win), L = (n())-W, n = n()) %>%
  arrange((wins))%>%
  data.frame

data %>%
  #filter(startsWith(as.character(series.id),"champs"))%>%
  filter(X...>=0) %>%
 filter(mode!="Search & Destroy")%>%
  group_by(player) %>%
  summarise(n = n()) %>%
  arrange((n))%>%
  data.frame

data %>%
  #filter(startsWith(as.character(series.id),"champs"))%>%
 filter(mode!="Search & Destroy")%>%
  group_by(player) %>%
  summarise(n = n()) %>%
  arrange((n))%>%
  data.frame

data%>%
  filter(team=="Team Envy")%>%
  filter(opponent=="Heretics")%>%
  group_by(player,team,opponent,mode)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2))%>%
  arrange(desc(KD))%>%
    data.frame

data%>%
  filter(team=="Luminosity")%>%
  filter(mode!="Search & Destroy")%>%
  group_by(player,team,fave.weapon)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),map=n())%>%
  arrange(desc(KD))%>%
  data.frame

data%>%
  filter(team=="100 Thieves")%>%
  group_by(player,team,map)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),maps=n())%>%
  arrange(desc(KD))%>%
  data.frame

data %>%
  filter(team=="Luminosity")%>%
  filter(startsWith(as.character(series.id),"champs"))%>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  group_by(team,mode) %>%
  summarise(wins = mean(win), W = sum(win)/5, L = (n()/5)-W, n = n()/5) %>%
  # filter(n>5)%>%
  arrange(desc(wins))%>%
  data.frame

data%>%
  filter(team=="Team Envy")%>%
  group_by(player,team,mode)%>%
  summarise(Kills=sum(kills, na.rm = T),Deaths=sum(deaths, na.rm = T))%>%
  arrange(desc(Kills))%>%
  data.frame()



data%>%
  filter(startsWith(as.character(series.id),"champs"),mode!="Search & Destroy")%>%
  group_by(player,team)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),maps=n())%>%
  arrange(desc(KD))%>%
  data.frame

data%>%
  filter(fave.weapon=="Rampart 17")%>%
  filter(mode!="Search & Destroy")%>%
  group_by(player,team,fave.weapon)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),map=n())%>%
  arrange(desc(KD))%>%
  data.frame

data %>% 
  filter(mode == "Hardpoint", win. == "W") %>% 
  group_by(match.id, team,opponent) %>% 
  summarise(team.kills = sum(kills), opp.kills = max(opp.kills), outslay = team.kills > opp.kills) %>% 
  ungroup() %>% 
  summarise(mean = mean(outslay), wins = n())

data %>% 
  filter(mode == "Hardpoint", win. == "W") %>% 
  group_by(match.id, team,opponent) %>% 
  summarise(team.kills = sum(kills), opp.kills = max(opp.kills), outslay = team.kills > opp.kills) %>% 
  filter(outslay==FALSE)%>%
  data.frame
summary(lm(RespawnKD~SPM,data=respawnlb))

ggplot(respawnlb,aes(RespawnKD,SPM))+geom_point(color='blue')+geom_smooth(method = "lm", se = FALSE)


data%>%
  filter(team=="Team Envy")%>%
  filter(match.id=="12668752780186447872")%>%
  group_by(player)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),kills=sum(kills, na.rm = T),
            deaths=sum(deaths, na.rm = T),fb=sum(snd.firstbloods, na.rm = T),fd=sum(snd.firstdeaths, na.rm = T),map=n())%>%
  arrange(desc(KD))%>%
  data.frame