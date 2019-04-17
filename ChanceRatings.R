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

# data_firstgame <- read.csv('head_fw.csv')
# data_firstgame$match.id <- as.factor(data_firstgame$match.id)
# data <- rbind(data, data_firstgame)

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
         dmgpr = (damage.dealt/ifelse(mode != "Search & Destroy",duration..s.,snd.rounds)),
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


output <- data %>%
  mutate(Rating = pred/0.5) %>%
  select(date, player, team, opponent,mode, Rating, dpr, kpr, dmgpr, spr) %>%
  mutate(dmgpr=ifelse(dmgpr<1,dmgpr/60,dmgpr))%>%
  group_by(mode) %>%
  mutate(SPM = percent_rank(spr),
         DeathPM = percent_rank(-dpr),
         DPM=percent_rank(dmgpr),
         KPM=percent_rank(kpr)) %>%
  select(date,team, opponent, mode, player, Rating, SPM, DeathPM, DPM, KPM) %>%
  arrange(desc(date),team, desc(Rating)) %>%
  data.frame() %>% head(50)


data %>%
  # mutate(dmgpr=ifelse(dmgpr<1,dmgpr/60,dmgpr))%>%%>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(series.id, player, mode) %>% 
  summarise(dmgpr = sum(damage.dealt)/sum(ifelse(mode != "Search & Destroy",duration..s.,snd.rounds)),
            spr = sum(player.score)/sum(ifelse(mode != "Search & Destroy",duration..s.,snd.rounds)),
            dpr = sum(deaths)/sum(ifelse(mode != "Search & Destroy",duration..s.,snd.rounds)),
            apr = sum(assists)/sum(ifelse(mode != "Search & Destroy",duration..s.,snd.rounds)),
            kpr = sum(kills)/sum(ifelse(mode != "Search & Destroy",duration..s.,snd.rounds)),
            kdr = sum(kills)/sum(deaths),
            weapon = Mode(fave.weapon),
            ModeRating = ind.win(mode,  dmgpr, spr, dpr, apr, kpr, kdr, weapon)/0.5,
            hek = sum(hek), hed = sum(hed),
            kills = sum(kills), deaths = sum(deaths),
            date = max(date)
  ) %>%
  ungroup() %>%
  mutate(wModeRating = ifelse(mode == "Hardpoint", ModeRating*0.4264151,
                              ifelse(mode == "Search & Destroy", ModeRating*0.3283019, ModeRating*0.245283))) %>%
  group_by(series.id, player) %>%
  summarise(SeriesRating = sum(wModeRating),
            date = max(date),
            aKD = sum(hek, na.rm = T)/sum(hed, na.rm = T),
            KD = paste(sum(kills), sum(deaths), sep = "-")) %>%
  arrange(desc(date)) %>%
  select(series.id, player, SeriesRating, aKD, KD) %>%
  data.frame() %>% 
  head(10) %>% 
  arrange(desc(SeriesRating))
# Authenticate
#list sheets
gs_ls()
# get stats sheet
playerSheet <- gs_title('Game Ratings')

# number of rows in existing data

playerMasterSheet <- as.data.frame(gs_read(ss=playerSheet, ws = "Sheet1"))
#playerMasterSheet$match.id <- as.numeric(playerMasterSheet$match.id)
rows <- nrow(playerMasterSheet)+1


playerlb <- output #%>%
   #filter(startsWith(as.character(series.id),'pro-w5'))

gs_edit_cells(ss = playerSheet, ws = "Sheet1", anchor = "A3",
              input = playerlb, byrow = TRUE,col_names = F)
