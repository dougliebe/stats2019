library(httr)
library(jsonlite)
library(lubridate)
library(RCurl)
library(dplyr)
library(googlesheets)
setwd("C:/Users/JP/Downloads/")

url  <- "https://stats.gammaray.io/api/v1/report/pro-league-2019/playermatches/"

raw <- getURL(url = url)
data <- read.csv(text = raw)
# tail(data) %>% 
#   mutate(match.id = format(match.id, scientific = F))

data_firstgame <- read.csv('head.csv')
data_firstgame$match.id <- as.factor(data_firstgame$match.id)
data <- rbind(data, data_firstgame)

# data <- data %>%
#   filter(!startsWith(as.character(match.id),'missing'))

data$date <- as_datetime(data$end.time)

### Edit for team name changes
data$team <- replace(data$team, data$team=="ExcelerateGG", "Excelerate")
data$player <- as.factor(ifelse(data$player=="Priestah", "Priestahh", as.character(data$player)))
data$player <- as.factor(ifelse(data$player=="Felony", "Felo", as.character(data$player)))
data$player <- as.factor(ifelse(data$player=="Jurnii", "JurNii", as.character(data$player)))
data$player <- as.factor(ifelse(data$player=="Methodzsick", "MethodZsick", as.character(data$player)))
data$player <- as.factor(ifelse(data$player=="Mruiz", "MRuiz", as.character(data$player)))

# # Authenticate
# #list sheets
gs_ls()

# get stats sheet
pull <- gs_title('Pro League Score Damage')

# list worksheets
gs_ws_ls(pull)

#get data just from Sheet1
data_dmgscore <- as.data.frame(gs_read(ss=pull, ws = "Sheet2"))
data_dmgscore$match.id <- as.factor(data_dmgscore$match.id)

# data$match.id <- as.numeric(data$match.id)
data <- merge(data, data_dmgscore, by = c('match.id', 'player', 'team'), all.x = T)

#row <- data[1,]

data <- data%>%
  mutate(damage.dealt=ifelse(damage.dealt==0,damage,damage.dealt))



# Authenticate
#list sheets
gs_ls()

# get stats sheet
pull <- gs_title('Rosters')

# list worksheets
gs_ws_ls(pull)

#get data just from Sheet1
rosters <- as.data.frame(gs_read(ss=pull, ws = "Sheet1"))


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
returnOppDamage <- function(id, team) {
  data <- data #change this second variable to whatever your dataframe is called
  damage <- sum(data[data$match.id == id & data$team != team, ]$damage.dealt)
  return(damage)
}

data$opponent <- mapply(returnOpp,data$match.id, data$team)
data$opp.score <- mapply(returnOppScore,data$match.id, data$team)
data$opp.kills <- mapply(returnOppKills,data$match.id, data$team)
data$opp.damage <- mapply(returnOppDamage,data$match.id, data$team)





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
mapfactor <- read.csv('mapfactor.csv')[,-1]
data<-merge(data,mapfactor,by=c('mode', 'map'),all.x = TRUE)

data <- data %>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  mutate(EngPlus=(kills+ifelse(!is.na(assists),assists,0)+deaths)/mf*100,
         dmgpr = damage.dealt/ifelse(mode != "Search & Destroy",duration..s.,snd.rounds),
         spr = player.score/ifelse(mode != "Search & Destroy",duration..s.,snd.rounds),
         dpr = deaths/ifelse(mode != "Search & Destroy",duration..s.,snd.rounds),
         apr = assists/ifelse(mode != "Search & Destroy",duration..s.,snd.rounds),
         kpr = kills/ifelse(mode != "Search & Destroy",duration..s.,snd.rounds),
         kdr = kills/deaths,
         fbpr = snd.firstbloods/ifelse(mode != "Search & Destroy",duration..s.,snd.rounds),
         fdpr = snd.firstdeaths/ifelse(mode != "Search & Destroy",duration..s.,snd.rounds))  %>%
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
                       ifelse(mode == "Search & Destroy", predict(sndARPred, data.frame(dmgpr,  apr, spr,kpr), type = "response", allow.new.levels = T),
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
  summarise(Rating = round(mean(pred, na.rm = T)/0.5,3), maps = n()) %>%
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

# data <- data %>%
#   filter(startsWith(as.character(series.id),'pro-w5'))



respawnlb<-data%>%
  filter(mode=="Hardpoint"|mode=="Control")%>%
  group_by(match.id, team) %>%
  mutate(ptd = deaths/sum(deaths, na.rm = T)) %>%
  ungroup() %>%
  group_by(player) %>%
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
  dplyr::select(player,RespawnKD,SPM, DPM, EPM,PTD,DeathPM,RespawnRating,RespawnMaps)%>%
  arrange(desc(RespawnKD))%>%
  data.frame() 

hplb<-data%>%
  filter(mode=="Hardpoint")%>%
  group_by(match.id, team) %>%
  mutate(ptd = deaths/sum(deaths, na.rm = T)) %>%
  ungroup() %>%
  group_by(player) %>%
  summarise(EPM = round(sum(EngPlus, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            KPM = round(sum(kills, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            SPM = round(sum(player.score, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DPM = round(sum(damage.dealt, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DeathPM = round(sum(deaths, na.rm = T)/
                              (sum(duration..s.,na.rm =T))*60,6),
            APM = round(sum(assists, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            HPKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T), 
            HPMaps=n(), 
            weapon = Mode(fave.weapon),
            HPRating = ifelse(weapon == "Saug 9mm",
                              predict(hpSUBPred, data.frame(apr = APM/60, dpr = DeathPM/60, dmgpr=DPM/60), type = "response", allow.new.levels = T),
                              predict(hpARPred, data.frame(dpr= DeathPM/60, spr = SPM/60), type = "response", allow.new.levels = T))/0.5,
            PTD = mean(ptd, na.rm =T),
            HillTime=round(mean(hill.time..s.,na.rm = T),1))%>%
  select(player,HPKD,SPM,DPM,EPM,PTD,DeathPM,HillTime,HPRating,HPMaps)%>%
  arrange(desc(HPRating))%>%
  data.frame()

sndlb<-data%>%
  filter(mode=="Search & Destroy")%>%
  filter(snd.rounds != 0) %>%
  group_by(player) %>%
  summarise(EPR = round(sum(c(kills,assists,deaths), na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            SPR = round(sum(player.score, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            DPR = round(sum(damage.dealt, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            KPR = round(sum(kills, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            APR = round(sum(assists, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            DeathPR = round(sum(deaths, na.rm = T)/
                              (sum(snd.rounds,na.rm =T)),6),
            SnDKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T),
            SnDFB=round(sum(snd.firstbloods, na.rm = T)/sum(snd.rounds, na.rm =T),6),
            SnDFBFD=paste(sum(snd.firstbloods, na.rm = T),sum(snd.firstdeaths, na.rm = T),sep = ":"),
            weapon = Mode(fave.weapon),
            SnDRating = ifelse(weapon == "Saug 9mm",
                               predict(sndSUBPred, data.frame(apr = APR, kpr = KPR, dmgpr=DPR), type = "response", allow.new.levels = T),
                               predict(sndARPred, data.frame(kpr= KPR,apr = APR, spr = SPR,dmgpr = DPR), type = "response", allow.new.levels = T))/0.5,
            SnDMaps=n())%>%
  # filter(weapon == "Saug 9mm") %>%
  select(player,SnDKD,SPR,DPR, EPR, DeathPR, SnDFB,SnDFBFD,SnDRating,SnDMaps) %>%
  arrange(desc(SnDRating)) %>%
  data.frame()

controllb <-data%>%
  filter(mode=="Control")%>%
  group_by(match.id, team) %>%
  mutate(ptd = deaths/sum(deaths, na.rm = T)) %>%
  ungroup() %>%
  group_by(player) %>%
  summarise(EPM = round(sum(EngPlus, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            KD = sum(kills, na.rm = T)/sum(deaths, na.rm = T),
            SPM = round(sum(player.score, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DPM = round(sum(damage.dealt, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            KPM = round(sum(kills, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            DeathPM = round(sum(deaths, na.rm = T)/
                              (sum(duration..s.,na.rm =T))*60,6),
            ControlKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T), 
            ControlMaps=n(),
            weapon = Mode(fave.weapon),
            ControlRating = ifelse(weapon == "Saug 9mm",
                                   predict(ctlSUBPred, data.frame(kpr = KPM/60, spr = SPM/60, kdr=KD), type = "response", allow.new.levels = T),
                                   predict(ctlARPred, data.frame(dpr= DeathPM/60, spr = SPM/60), type = "response", allow.new.levels = T))/0.5,
            CLU = mean(ptd, na.rm = T)) %>%
  select(player,ControlKD,SPM, DPM,EPM,CLU, DeathPM,ControlRating,ControlMaps) %>%
  arrange(desc(ControlRating)) %>%
  data.frame()



overalllb <- data %>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(player) %>%
  summarise(KD = sum(kills, na.rm = T)/sum(deaths, na.rm = T),
            aKD = sum(hek, na.rm = T)/sum(hed, na.rm = T),
            # Rating = mean(pred, na.rm = T)/0.5,
            Xfactor = sum(ifelse(win. == "W",hek,NA), na.rm = T)/
              sum(ifelse(win. == "W",hed,NA), na.rm = T)-
              sum(ifelse(win. == "L",hek,NA), na.rm = T)/
              sum(ifelse(win. == "L",hed,NA), na.rm = T),
            TotalMaps = n()) %>%
  select(player, KD, aKD, Xfactor, TotalMaps) %>%
  arrange(desc(aKD))



playerlb<-respawnlb
playerlb<-merge(playerlb,sndlb,by = "player")
playerlb<-merge(playerlb,hplb,by="player")
playerlb<-merge(playerlb,controllb,"player")
playerlb<-merge(playerlb,overalllb,"player")
playerlb<-playerlb%>%
  arrange(desc(aKD))
playerlb$RespawnRating <- playerlb$HPRating*0.6191951+playerlb$ControlRating*0.3808049
playerlb$Rating <- playerlb$HPRating*0.4168114+
  playerlb$ControlRating*0.256339+
  playerlb$SnDRating*0.3268496

playerlb <- merge(playerlb, rosters, by.x = 'player', by.y = 'Player', all.x = T)
playerlb$Team <- ifelse(is.na(playerlb$Team), "F/A", as.character(playerlb$Team))

playerlb<-setNames(playerlb,c("Player",
                              "RespawnKD",
                              "RespawnSPM",
                              "RespawnDPM",
                              "RespawnEPM",
                              "RespawnPTD",
                              "RespawnDeathPM",
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
                              "aKD",
                              "Xfactor",
                              "OverallMaps",
                              "RespawnRating",
                              "OverallRating","Team"))

playerlb %>%
  select(Player, Team, HPRating,HPSPM, HPDeathPM, ControlRating ,ControlSPM, ControlDeathPM,
         SnDRating,SnDDPR, SnDSPR, OverallRating, aKD) %>%
  mutate(HPSPM = percent_rank(HPSPM),
         HPDeathPM = percent_rank(-HPDeathPM),
         ControlSPM = percent_rank(ControlSPM),
         ControlDeathPM = percent_rank(-ControlDeathPM),
         SnDDPR=percent_rank(SnDDPR),
         SnDSPR=percent_rank(SnDSPR)) %>%
  filter(Player == "Silly" | Player == "Sukry") %>%
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
  ungroup() %>% group_by(team) %>%
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
            SNDHeal=(sum(opp.damage, na.rm = T)-sum(ifelse(is.na(opp.damage),NA,opp.kills), na.rm = T)*150)/sum(opp.damage, na.rm = T)) %>%
  arrange(desc(SNDRoundWin))%>%
  data.frame()


teamlb<-data.frame()
teamlb<-merge(overallrecordlb,hprecordlb,by="team")
teamlb<-merge(teamlb,sndrecordlb,by="team")
teamlb<-merge(teamlb,controlrecordlb,by="team")
teamlb<-merge(teamlb,teamhardpointlb,by="team")
teamlb<-merge(teamlb,teamcontrollb,by="team")
teamlb<-merge(teamlb,teamsndlb,by="team")
teamlb<-teamlb %>% rename(Team = 'team')



# 
# playerlb$Team <- replace(as.character(playerlb$Team), playerlb$Team == "Thieves", "100 Thieves")
# playerlb$Player <- replace(as.character(playerlb$Player), playerlb$Player == "Felony", "Felo")
# playerlb$Player <- replace(as.character(playerlb$Player), playerlb$Player == "Priestah", "Priestahh")
# 

write.csv(playerlb, file = "TestPlayerLeaderboardPL.csv")
write.csv(teamlb, file = "TestTeamLeaderboardPL.csv")


################################################################################


data%>%
  #filter(startsWith(as.character(series.id),'pro-w5') | startsWith(as.character(series.id),'pro-w6'))%>%
  #filter(startsWith(as.character(series.id),'pro-w6'))%>%
  filter(match.id!="9175610164306702268")%>%
  #filter(player=="Temp")%>%
  filter(map=="Frequency",mode=="Control")%>%
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
                   KillsPM = round(sum(kills, na.rm = T)/
                                    (sum(duration..s.,na.rm =T))*60,6),
                   RespawnKD=round(sum(kills,na.rm = T)/sum(deaths,na.rm = T),2), 
                   PTD = mean(ptd, na.rm =T),
                   RespawnRating = median(pred)/0.5,
                   RespawnMaps=n())%>%
  dplyr::select(player,team,RespawnKD,SPM, DPM, EPM,PTD,DeathPM,KillsPM,RespawnRating,RespawnMaps)%>%
  #dplyr::select(player,team,RespawnKD,SPM, DPM, EPM,PTD,DeathPM,RespawnMaps)%>%
  data.frame() %>%  
  #select(player,team,DPM,RespawnKD,RespawnMaps)%>%
  select(player,RespawnKD,SPM,DPM,RespawnMaps)%>%
  arrange(desc(RespawnKD))

data%>%
  filter(startsWith(as.character(series.id),'pro-w6'))%>%
  filter(mode=="Search & Destroy")%>%
  filter(snd.rounds != 0) %>%
  group_by(player,team) %>%
  summarise(EPR = round(sum(c(kills,assists,deaths), na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            SPR = round(sum(player.score, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            DPR = round(sum(damage.dealt, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            KPR = round(sum(kills, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            APR = round(sum(assists, na.rm = T)/
                          (sum(snd.rounds,na.rm =T)),6),
            DeathPR = round(sum(deaths, na.rm = T)/
                              (sum(snd.rounds,na.rm =T)),6),
            SnDKD=sum(kills,na.rm = T)/sum(deaths,na.rm = T),
            SnDFB=round(sum(snd.firstbloods, na.rm = T)/sum(snd.rounds, na.rm =T),6),
            SnDFBFD=paste(sum(snd.firstbloods, na.rm = T),sum(snd.firstdeaths, na.rm = T),sep = ":"),
            weapon = Mode(fave.weapon),
            SnDRating = ifelse(weapon == "Saug 9mm",
                               predict(sndSUBPred, data.frame(apr = APR, kpr = KPR, dmgpr=DPR), type = "response", allow.new.levels = T),
                               predict(sndARPred, data.frame(kpr= KPR,apr = APR, spr = SPR,dmgpr = DPR), type = "response", allow.new.levels = T))/0.5,
            SnDMaps=n())%>%
  # filter(weapon == "Saug 9mm") %>%
  select(player,team,SnDKD,SnDMaps) %>%
  arrange(desc(SnDKD)) %>%
  data.frame()

data %>%
  filter(startsWith(as.character(series.id),'pro-w5'))%>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(player, team) %>%
  summarise(KD = sum(kills, na.rm = T)/sum(deaths, na.rm = T),
            aKD = sum(hek, na.rm = T)/sum(hed, na.rm = T),
            # Rating = mean(pred, na.rm = T)/0.5,
            Xfactor = sum(ifelse(win. == "W",hek,NA), na.rm = T)/
              sum(ifelse(win. == "W",hed,NA), na.rm = T)-
              sum(ifelse(win. == "L",hek,NA), na.rm = T)/
              sum(ifelse(win. == "L",hed,NA), na.rm = T),
            TotalMaps = n()) %>%
  select(player, team, KD, aKD, Xfactor, TotalMaps) %>%
  arrange(desc(aKD)) %>%
  data.frame()

data%>%
  filter(startsWith(as.character(series.id),'pro-w5'))%>%
 # filter(fave.weapon=="Rampart 17")%>%
  filter(mode!="Search & Destroy")%>%
  group_by(fave.weapon)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),map=n())%>%
  arrange(desc(KD))%>%
  data.frame


data %>%
  #filter(team=="100 Thieves")%>%
  filter(startsWith(as.character(series.id),"pro-w6"))%>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  group_by(team,mode) %>%
  summarise(wins = mean(win), W = sum(win)/5, L = (n()/5)-W, n = n()/5) %>%
  # filter(n>5)%>%
  arrange(desc(wins))%>%
  data.frame

data%>%
  #filter(fave.weapon=="Rampart 17")%>%
  filter(startsWith(as.character(series.id),'pro-w5'))%>%
  filter(mode=="Search & Destroy")%>%
  group_by(fave.weapon)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),kills=sum(kills, na.rm = T),deaths=sum(deaths, na.rm = T),assists=sum(assists,na.rm=T),map=n(),
            DPM = round(sum(damage.dealt, na.rm = T)/(sum(duration..s.,na.rm =T))*60),damage=sum(damage.dealt, na.rm = T),duration=sum(duration..s.),
            DPR = round(sum(damage.dealt, na.rm = T)/(sum(snd.rounds,na.rm =T)),6),rounds=sum(snd.rounds,na.rm =T),
            killef=(kills*150+assists*75)/damage)%>%
  arrange(desc(killef))%>%
  data.frame

data%>%
  filter(mode=="Control")%>%
  dplyr::summarise(SPM = round(sum(player.score, na.rm = T)/
                               (sum(duration..s.,na.rm =T))*60))%>%
  arrange(desc(SPM))

data%>%
  filter(mode=="Search & Destroy")%>%
  dplyr::summarise(SPR = round(sum(player.score, na.rm = T)/
              (sum(snd.rounds,na.rm =T)),6))%>%
  arrange(desc(SPR))



data%>%
  #filter(startsWith(as.character(series.id),'pro-w5'))%>%
  filter(mode=="Search & Destroy")%>%
  group_by(player,team)%>%
  summarise(KD = round(sum(kills, na.rm = T)/sum(deaths, na.rm = T),2),kills=sum(kills, na.rm = T),deaths=sum(deaths, na.rm = T),assists=sum(assists,na.rm=T),map=n(),
            DPM = round(sum(damage.dealt, na.rm = T)/(sum(duration..s.,na.rm =T))*60),damage=sum(damage.dealt, na.rm = T),duration=sum(duration..s.),
            DPR = round(sum(damage.dealt, na.rm = T)/(sum(snd.rounds,na.rm =T)),6),rounds=sum(snd.rounds,na.rm =T),
            killef=(kills*150+assists*75)/damage)%>%
  arrange(desc(killef))%>%
  data.frame

data%>%
  filter(startsWith(as.character(series.id),'pro-w5'))%>%
  filter(team=="Excelerate")%>%
  group_by(player,team,mode)%>%
  summarise(Kills=sum(kills, na.rm = T),Deaths=sum(deaths, na.rm = T))%>%
  arrange(desc(Kills))%>%
  data.frame()

data %>%
  #filter(player=="Classic") %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  filter(mode!="Search & Destroy")%>%
  filter(X...>=0) %>%
  group_by(player) %>%
  summarise(wins = mean(win), W = sum(win), L = (n())-W, n = n()) %>%
  arrange((wins))%>%
  data.frame

rspwnsnd<-merge(respawnlb,sndlb,by=c("player"))
rspwnsnd<-rspwnsnd%>%
  mutate(diff=SnDKD-RespawnKD,rspwnpos=ifelse(RespawnKD>=1,1,0),sndpos=ifelse(SnDKD>=1,1,0),
         pospos=ifelse(rspwnpos==1&sndpos==1,1,0),posneg=ifelse(rspwnpos==1&sndpos==0,1,0),
         negpos=ifelse(rspwnpos==0&sndpos==1,1,0),negneg=(ifelse(rspwnpos==0&sndpos==0,1,0)))#%>%
  #summarise(rspwnpos=sum(rspwnpos),sndpos=sum(sndpos),pospos=sum(pospos),posneg=sum(posneg),negpos=sum(negpos),negneg=sum(negneg))
rspwnsnd%>%
  filter(posneg==1)%>%
  select(player)%>%
  data.frame

rspwnsnd<-rspwnsnd%>%
  mutate(diff=SnDRating-RespawnRating,rspwnpos=ifelse(RespawnRating>=1,1,0),sndpos=ifelse(SnDRating>=1,1,0),
         pospos=ifelse(rspwnpos==1&sndpos==1,1,0),posneg=ifelse(rspwnpos==1&sndpos==0,1,0),
         negpos=ifelse(rspwnpos==0&sndpos==1,1,0),negneg=(ifelse(rspwnpos==0&sndpos==0,1,0)))#%>%
#summarise(rspwnpos=sum(rspwnpos),sndpos=sum(sndpos),pospos=sum(pospos),posneg=sum(posneg),negpos=sum(negpos),negneg=sum(negneg))
rspwnsnd%>%
  filter(posneg==1)%>%
  select(player)%>%
  data.frame

data%>%
  filter(startsWith(as.character(series.id),'pro-w5') | startsWith(as.character(series.id),'pro-w6'))%>%
  filter(team=="Reciprocity")%>%
  group_by(player,team)%>%
  summarise(Kills=sum(kills, na.rm = T),Deaths=sum(deaths, na.rm = T))%>%
  arrange(desc(Kills))%>%
  data.frame()

data%>%
  filter(map=="Frequency",mode=="Control")%>%
  group_by(player)%>%
  summarise(Kills=sum(kills, na.rm = T),Deaths=sum(deaths, na.rm = T))%>%
  arrange(desc(Kills))%>%
  data.frame()

data%>%
  filter(mode=="Hardpoint")%>%
  filter(fave.weapon=="Saug 9mm")%>%
  mutate(EPM=EngPlus/duration..s.*60)%>%
  filter(EPM>5.00)%>%
  mutate(win = ifelse(win. == "W",1,0))%>%
summarise(wins = mean(win), W = sum(win), L = (n())-W, n = n()) 

data%>%
  filter(mode=="Hardpoint")%>%
  group_by(player)%>%
  summarise(multi=(sum(X2.piece,na.rm=T)*2+sum(X3.piece,na.rm=T)*3+sum(X4.piece,na.rm=T)*4)/sum(duration..s.)*60)%>%
  arrange(desc(multi))
