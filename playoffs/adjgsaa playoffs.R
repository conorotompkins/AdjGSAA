setwd("C:/Users/conor/githubfolder/AdjGSAA/playoffs")

library(ggthemes)
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(tidyr)
library(cowplot)
library(forcats)


###ok let's load the data into R###
goalie_data_playoffs <- read.csv("goalie.playoffs.csv") ###this file has to be in the R directory you set previously###
goalie_data_playoffs$season <- as.character(goalie_data_playoffs$season) ###turn the season variable into a factor###
goalie_data_playoffs$Date <- mdy(goalie_data_playoffs$Date) ###turn the date of the game into a format R can work with###
goalie_data_playoffs$Team <- as.character(goalie_data_playoffs$Team) ###turn the Team variable into a character string so we can modify it###
goalie_data_playoffs$game_type <- "playoff_game"


goalie.playoff.data <- goalie.playoff.data %>% ###modify some variables so they are easier to work with
  mutate(svH = Sv.H / 100, ###turn the sv% variables into decimals (99 -> .99)
         svM = Sv.M / 100,
         svL = Sv.L / 100,
         saH = G.M + S.H, ###goals + shots = saves###
         saM = G.M + S.M,
         saL = G.L + S.L, 
         Name = as.character(Name))

goalie.playoff.data <- goalie.playoff.data %>% ###select the variables we want and omit any rows with NA (blank) values###
  select(game_type, Team, Name, Age, season, Date, TOI, svH, svM, svL, saH, saM, saL, G.H, G.M, G.L)


###combine Atlanta/Winnipeg and Phoenix/Arizona Team names
goalie.playoff.data$Team[goalie.playoff.data$Team == "WPG"]<-"ATL-WPG"
goalie.playoff.data$Team[goalie.playoff.data$Team == "ATL"]<-"ATL-WPG"
goalie.playoff.data$Team[goalie.playoff.data$Team == "PHX"]<-"PHX-ARI"
goalie.playoff.data$Team[goalie.playoff.data$Team == "ARI"]<-"PHX-ARI"

season.averages <- read.csv("mercad.season.averages.csv") %>%
  mutate(season = as.character(season))

###Create Playoff Goalie Statistics###
playoff.games <- goalie.playoff.data %>% ###create a new object###
  select(game_type, Team, Name, season, Date, TOI, svH, svM, svL, saH, saM, saL, G.H, G.M, G.L) ###select the variables we want###

goalie.join.playoff.games <- inner_join(playoff.games, season.averages, by = "season") ###join the individual goalie data with the season average data###

df.playoff.games <- goalie.join.playoff.games %>% ###create a new object###
  mutate(lgsaa = (saL *(1 - la.svL) - G.L), ###create new variables. LGSAA = (low danger shots against * (1 - league average low danger save %) - low danger goals against)###
         mgsaa = (saM *(1 - la.svM) - G.M),
         hgsaa = (saH *(1 - la.svH) - G.H),
         adjgsaa60 = ((lgsaa + mgsaa + hgsaa) / TOI) * 60, ###create adjGSAA60###
         adjgsaa = lgsaa + mgsaa + hgsaa) ###create adjGSAA###

df.playoff.games <- df.playoff.games %>%
  select(-X) %>%
  group_by(Name) %>% ###group the rows by goalie name###
  arrange(Date) %>% ###arrange the rows by date###
  mutate(Game.Number = dense_rank(Date),
         season = as.character(season)) %>% ###create the "game number" variable"###
  ungroup()

player.summary <- df.playoff.games %>% ###create a new object for player summary data###
  filter(TOI > 2) %>%
  group_by(Name) %>% ###group by goalie name###
  summarize(adjgsaa60 = mean(adjgsaa60), ###sum up all the variables###
            lgsaa = sum(lgsaa),
            mgsaa = sum(mgsaa),
            hgsaa = sum(hgsaa),
            toi = sum(TOI)) %>%
  arrange(desc(adjgsaa60))

write.csv(df.playoff.games, "playoff.games.mercad.csv") ###export to csv. take out the preceding #

goalie <- "Roberto.Luongo"
ggplot(filter(df.playoff.games, Name == goalie), aes(Game.Number, adjgsaa60, color = season, fill = season)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  labs(y = "Adjusted Goals Saved Above Average Per 60", title = goalie) +
  theme_bw()
warnings()

ggplot(player.summary, aes(toi, adjgsaa60, label = Name, alpha = toi)) +
  geom_smooth() +
  geom_text() +
  theme_bw()

