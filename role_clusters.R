library(httr)
library(jsonlite)
library(lubridate)
library(RCurl)
library(dplyr)
library(googlesheets)
library(flexclust)
library(reshape2)

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
  mutate(player.score=ifelse(is.na(score1),player.score,score1))%>%
  mutate(damage.dealt=ifelse(is.na(damage),damage.dealt,damage))%>%
  select(-score1)%>%
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
#Clustering Kmeans------
data <- data %>%
  mutate(hek = ifelse(mode == "Hardpoint", kills, ifelse(mode == "Search & Destroy", kills*3.63, kills*1.23))) %>%
  mutate(hed = ifelse(mode == "Hardpoint", deaths, ifelse(mode == "Search & Destroy", deaths*3.63, deaths*1.23))) %>%
  group_by(mode) %>%
  mutate(EngPlus=(kills+ifelse(!is.na(assists),assists,0)+deaths)/mf*100,
         APlus=ifelse(!is.na(assists),assists,0)/mf*100,
         ar = ifelse(fave.weapon == "Saug 9mm" | fave.weapon == "Spitfire", 0,1),
         eng.adj = ifelse(ar == 1,
                          (EngPlus*60/duration..s.)/(pmin(k.d,2)*0.48+3.72),
                          (EngPlus*60/duration..s.)/(pmin(k.d,2)*0.65+3.71)),
         EngAdj = eng.adj*(mean(EngPlus*60/duration..s., na.rm = T))*(duration..s./60),
         EngAdj60 = eng.adj*(mean(EngPlus, na.rm = T)*60/duration..s.),
         APE = assists/(kills+assists),
         dmgpr = damage.dealt/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         spr = player.score/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         dpr = deaths/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         apr = assists/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         kpr = kills/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         kdr = kills/deaths,
         killeff = ((kills*150+assists*75)-damage.dealt)/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         fbpr = snd.firstbloods/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
         fdpr = snd.firstdeaths/ifelse(snd.rounds == 0,duration..s.,snd.rounds)) %>%
  data.frame()



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


# adj_eng_ar <- lm(EngPlus/duration..s.*60 ~ k.d, data %>% filter(mode == "Hardpoint") %>%
#                   mutate(ar = ifelse(fave.weapon == "Saug 9mm" | fave.weapon == "Spitfire", 0,1)) %>%
#                   filter(ar == 1, k.d < 1.5, k.d>0.5))
# summary(adj_eng_ar)
# adj_eng_sub <- lm(EngPlus/duration..s.*60 ~ k.d, data %>% filter(mode == "Hardpoint") %>%
#                    mutate(ar = ifelse(fave.weapon == "Saug 9mm" | fave.weapon == "Spitfire", 0,1)) %>%
#                    filter(ar == 0, k.d < 1.5, k.d>0.5))
# summary(adj_eng_sub)

#merge mf#merge mf
#mapfactor <- read.csv('mapfactor.csv')[,-1]
#data<-merge(data,mapfactor,by=c('mode', 'map'),all.x = TRUE)
#Cluster KNN -----------
hpdata <- data %>%
  filter(!is.na(assists))%>%
  filter(mode=="Hardpoint") %>%
  # mutate(EPM=((kills+assists+deaths)/mf*100)*60/(duration..s.),
  #        APE=(assists)/(kills+assists),
  #       # DPM = (damage.dealt*60)/(duration..s.),
  #        SPM = (player.score*60)/(duration..s.),
  #        DeathPM = (deaths*60)/(duration..s.),
  #        HillTime=(hill.time..s.*60)/(duration..s.),
  #       killeff = ((kills*150+assists*75)-damage.dealt)/ifelse(snd.rounds == 0,duration..s.,snd.rounds),
  #        APM = (assists*60)/(duration..s.))%>%
  #       # KPM = (kills*60)/(duration..s.))%>%
  select(player,team, fave.weapon,EngAdj,APE)


player_group<-data%>%
  filter(mode=="Control")%>%
  group_by(player) %>%
  summarise(EPM = sum(EngAdj, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,
            SPM = round(sum(player.score, na.rm = T)/
                          (sum(duration..s.,na.rm =T))*60,6),
            APE = sum(assists, na.rm = T)/sum(ifelse(is.na(assists),NA, assists+kills), na.rm = T),
           # DPM = round(sum(damage.dealt, na.rm = T)/
           #               (sum(duration..s.,na.rm =T))*60,6),
            DeathPM = round(sum(deaths, na.rm = T)/
                              (sum(duration..s.,na.rm =T))*60,6),
            HillTime=round(mean(hill.time..s.,na.rm = T),1),
           weapon = Mode(fave.weapon),
           kd = sum(kills)/sum(deaths),
            APM=round(sum(assists,na.rm = T)/(sum(duration..s.,na.rm =T))*60,6))%>%
           # KPM=round(sum(kills,na.rm=T)/(sum(duration..s.,na.rm =T))*60,6))%>%
  select(player, weapon,EPM,APE)%>%
  # arrange(desc(SPM))%>%
  data.frame()

hpcluster<-player_group %>%
  filter( weapon != "Saug 9mm" , weapon != "Spitfire") %>%
  # filter( weapon == "Saug 9mm" | weapon == "Spitfire") %>%
  select(-player, -weapon) %>%
  data.frame()
hpclusterScaled<-scale(hpcluster)
set.seed(1)
fitK<-kmeans(hpclusterScaled,3)
fitK
plot(hpcluster, col = fitK$cluster)

k<-list()
for(i in 1:10){
  k[[i]]<-kmeans(hpclusterScaled,i)
}

betweenss_totss<-list()
for(i in 1:10){
  betweenss_totss[[i]]<-k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10,betweenss_totss,type="b",ylab="between ss/total ss",xlab="clusters (k)")

player_group$group<-predict(as.kcca(fitK, data = hpclusterScaled), scale(player_group[c(-1,-2)]))

plot(player_group %>%
       filter(weapon == "Saug 9mm" | weapon == "Spitfire") %>%
       select(-player, -weapon), col = player_group$group)

#add overall group to data
data <- merge(data, player_group[,c('player','group')], by = 'player')
data$group <- as.factor(data$group)

library(lme4)
library(lmerTest)
library(ggplot2)
total <- lme4::glmer(win. ~ scale(spr):mode + 
                          scale(dpr):mode + 
                          scale(dmgpr):mode +
                          scale(apr):mode + 
                          # scale(kpr):mode + 
                          (mode|group),
                        data %>% filter(!is.na(spr)), family = binomial(link='logit'))
summary(total)

#look at random effects by mode
exp(ranef(total)$group)

#compare binary error loss
data %>%
  mutate(pred = predict(total, data, type = "response")) %>%
  mutate(correct = ifelse((pred < 0.5 & win. == "L") | (pred > 0.5 & win. == "W"),1,0 )) %>%
  group_by(mode) %>%
  summarise(p = mean(correct, na.rm = T))


  # data.frame() %>%
  # # ggplot(aes(pred, color = mode))+geom_density()
  # 




#list sheets
gs_ls()
# get stats sheet
pull <- gs_title('Rosters')
# list worksheets
gs_ws_ls(pull)
#get data just from Sheet1
roster <- as.data.frame(gs_read(ss=pull, ws = "Sheet1"))

final <- merge(player_group, roster[-3], by.x = "player", by.y = "Player", all.x = T)
library(reshape2)
final %>%
  group_by(Team, group) %>%
  summarise(count = n()) %>%
  dcast(Team~group)

final %>%
  group_by(group) %>%
  filter(group == 4) %>% 
  select(player)
  summarise(count = n())

groups<-player_group%>%
  arrange(desc(group))%>%
  select(player,group)

data<-merge(data,groups,by="player")