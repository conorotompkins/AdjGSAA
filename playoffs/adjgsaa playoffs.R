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


goalie_data_playoffs <- goalie_data_playoffs %>% ###modify some variables so they are easier to work with
  mutate(svH = Sv.H / 100, ###turn the sv% variables into decimals (99 -> .99)
         svM = Sv.M / 100,
         svL = Sv.L / 100,
         saH = G.M + S.H, ###goals + shots = saves###
         saM = G.M + S.M,
         saL = G.L + S.L, 
         Name = as.character(Name)) %>%
  rename(team = Team, 
         date = Date, 
         name = Name, 
         toi = TOI,
         age = Age,
         salary = Salary)

goalie_data_playoffs <- goalie_data_playoffs %>% ###select the variables we want and omit any rows with NA (blank) values###
  select(game_type, team, name, age, salary, season, date, toi, svH, svM, svL, saH, saM, saL, G.H, G.M, G.L)


###combine Atlanta/Winnipeg and Phoenix/Arizona Team names
goalie_data_playoffs$team[goalie_data_playoffs$team == "WPG"]<-"ATL-WPG"
goalie_data_playoffs$team[goalie_data_playoffs$team == "ATL"]<-"ATL-WPG"
goalie_data_playoffs$team[goalie_data_playoffs$team == "PHX"]<-"PHX-ARI"
goalie_data_playoffs$team[goalie_data_playoffs$team == "ARI"]<-"PHX-ARI"

season_averages <- read.csv("adjgsaa.season.averages.csv") %>%
  mutate(season = as.character(season))

###Create Playoff Goalie Statistics###
playoff_games <- goalie_data_playoffs %>% ###create a new object###
  select(game_type, team, name, age, salary, season, date, toi, svH, svM, svL, saH, saM, saL, G.H, G.M, G.L)

goalie_join_playoff_games <- inner_join(playoff_games, season_averages, by = "season") ###join the individual goalie data with the season average data###

df_playoff_games <- goalie_join_playoff_games %>% ###create a new object###
  mutate(lgsaa = (saL *(1 - la.svL) - G.L), ###create new variables. LGSAA = (low danger shots against * (1 - league average low danger save %) - low danger goals against)###
         mgsaa = (saM *(1 - la.svM) - G.M),
         hgsaa = (saH *(1 - la.svH) - G.H),
         adjgsaa60 = ((lgsaa + mgsaa + hgsaa) / toi) * 60, ###create adjGSAA60###
         adjgsaa = lgsaa + mgsaa + hgsaa) ###create adjGSAA###

df_playoff_games <- df_playoff_games %>%
  select(-X) %>%
  group_by(name) %>% ###group the rows by goalie name###
  arrange(date) %>% ###arrange the rows by date###
  mutate(game_number = dense_rank(date),
         season = as.character(season)) %>% ###create the "game number" variable"###
  ungroup()

player_summary <- df_playoff_games %>% ###create a new object for player summary data###
  filter(toi > 2) %>%
  group_by(name) %>% ###group by goalie name###
  summarize(adjgsaa60 = mean(adjgsaa60), ###sum up all the variables###
            lgsaa = sum(lgsaa),
            mgsaa = sum(mgsaa),
            hgsaa = sum(hgsaa),
            toi = sum(toi)) %>%
  arrange(desc(adjgsaa60))

write.csv(df_playoff_games, "goalie.data.playoffs.csv") ###export to csv. take out the preceding #

ggplot(player_summary, aes(toi, adjgsaa60, label = name, alpha = toi)) +
  geom_smooth() +
  geom_text() +
  guides(alpha = FALSE) +
  theme_bw()
ggsave("adjgsaa60 vs toi.png")

