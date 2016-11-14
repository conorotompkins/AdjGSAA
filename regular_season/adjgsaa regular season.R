###set your working directory. you can also do this using the GUI (Session -> Set Working Directory)
setwd("C:/Users/conor/githubfolder/AdjGSAA/regular_season")
###these are the packages I use regularly. you need to install them (install.packages("packagename")) before loading them into the session with library(packagename)
library(ggthemes)
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(tidyr)
library(forcats)


###ok let's load the data into R###
goalie_data_regular_season <- read.csv("goalie.data.regular.season.csv") ###this file has to be in the R directory you set previously###
goalie_data_regular_season$season <- as.character(goalie_data_regular_season$season) ###turn the season variable into a factor###
goalie_data_regular_season$Date <- ymd(goalie_data_regular_season$Date) ###turn the date of the game into a format R can work with###
goalie_data_regular_season$Team <- as.character(goalie_data_regular_season$Team) ###turn the Team variable into a character string so we can modify it##
goalie_data_regular_season$game_type <- "regular_season" # add a column declaring that the game was a regular season game

goalie_data_regular_season <- goalie_data_regular_season %>% ###modify some variables so they are easier to work with
  mutate(svH = Sv.H / 100, ###turn the sv% variables into decimals (99 -> .99)
         svM = Sv.M / 100,
         svL = Sv.L / 100,
         saH = G.M + S.H, ###goals + shots = saves###
         saM = G.M + S.M,
         saL = G.L + S.L, 
         Name = as.character(Name)) %>%
  rename(team = Team, 
         age = Age, 
         date = Date, 
         name = Name, 
         salary = Salary)

goalie_data_regular_season <- goalie_data_regular_season %>% ###select the variables we want and omit any rows with NA (blank) values###
  select(game_type, team, name, age, season, date, TOI, svH, svM, svL, saH, saM, saL, G.H, G.M, G.L)

###combine Atlanta/Winnipeg and Phoenix/Arizona Team names
goalie_data_regular_season$team[goalie_data_regular_season$team == "WPG"]<-"ATL-WPG"
goalie_data_regular_season$team[goalie_data_regular_season$team == "ATL"]<-"ATL-WPG"
goalie_data_regular_season$team[goalie_data_regular_season$team == "PHX"]<-"PHX-ARI"
goalie_data_regular_season$team[goalie_data_regular_season$team == "ARI"]<-"PHX-ARI"

###Creating Per-Season Averages###
season_averages <- goalie_data_regular_season %>% ###create a new object in R to hold the per-season per-goalie averages###
  select(name, TOI, season, svH, svM, svL, saH, saM, saL) %>% ###select the variables we want to work with###
  group_by(name, season) %>% ###group the rows by name and season###
  summarize(TOI = sum(TOI), ###sum the TOI for the new rows we created###
            svH = mean(svH, na.rm = TRUE), ###take the average###
            svM = mean(svM, na.rm = TRUE), ###etc###
            svL = mean(svL, na.rm = TRUE),
            saH = sum(saH),
            saM = sum(saM),
            saL = sum(saL))

###create the per-season averages###
season_averages <- season_averages %>%
  group_by(season) %>% ###group the rows by season###
  summarize(la.svH = mean(svH, na.rm = TRUE), ###take the means of the danger save percentages###
            la.svM = mean(svM, na.rm = TRUE), 
            la.svL = mean(svL, na.rm = TRUE))
write.csv(season_averages, "adjgsaa.season.averages.csv") #create a csv of the season averages object. take out the preceding "#"s if you want to run this line

###Create Regular Season Goalie Statistics###
regular_season <- goalie_data_regular_season %>% ###create a new object###
  select(game_type, team, name, season, date, TOI, svH, svM, svL, saH, saM, saL, G.H, G.M, G.L) ###select the variables we want###

goalie_join_regular_season <- inner_join(regular_season, season_averages, by = "season") ###join the individual goalie data with the season average data###

df_regular_season <- goalie_join_regular_season %>% ###create a new object###
  mutate(lgsaa = (saL *(1 - la.svL) - G.L), ###create new variables. LGSAA = (low danger shots against * (1 - league average low danger save %) - low danger goals against)###
         mgsaa = (saM *(1 - la.svM) - G.M),
         hgsaa = (saH *(1 - la.svH) - G.H),
         adjgsaa60 = ((lgsaa + mgsaa + hgsaa) / TOI) * 60, ###create adjGSAA60###
         adjgsaa = lgsaa + mgsaa + hgsaa) ###create adjGSAA###

df_regular_season <- df_regular_season %>%
  group_by(name) %>% ###group the rows by goalie name###
  arrange(date) %>% ###arrange the rows by date###
  mutate(game_number = dense_rank(date),
         season = as.character(season)) %>% ###create the "game number" variable"###
  ungroup()

player_summary <- df_regular_season %>% ###create a new object for player summary data###
  group_by(name) %>% ###group by goalie name###
  summarize(adjgsaa = sum(adjgsaa), ###sum up all the variables###
            lgsaa = sum(lgsaa),
            mgsaa = sum(mgsaa),
            hgsaa = sum(hgsaa),
            toi = sum(TOI)) %>%
  arrange(desc(adjgsaa))

#write.csv(df_regular_season, "goalie.data.regular.season.csv") ###export to csv. take out the preceding "#"s if you want to run this line###

#df_regular_season <- read_csv("goalie.data.regular.season.csv") %>%
  #mutate(season = as.character(season))

regular_season_cumsum <- df_regular_season %>% ###create a data frame holding cumulative sums of adjGSAA
  select(name, date, game_number, adjgsaa) %>%
  arrange(date) %>%
  group_by(name) %>%
  mutate(cum_adjgsaa = cumsum(adjgsaa)) %>%
  ungroup()

top_goalies <- player_summary %>%
  select(name, adjgsaa) %>%
  rename(sum_adjgsaa = adjgsaa)

regular_season_cumsum <- regular_season_cumsum %>%
  left_join(top_goalies) %>%
  arrange(desc(sum_adjgsaa)) %>%
  mutate(name = fct_reorder(name, -sum_adjgsaa), 
         max_date = max(date))
?geom_label
#write.csv(regular_season_cumsum, "regular.season.cumsum.csv") ###export to csv. take out the preceding "#"s if you want to run this line###

###Summary Regular Season Graphs###
ggplot(regular_season_cumsum, aes(game_number, cum_adjgsaa, alpha = sum_adjgsaa, group = name)) + ###plot the rs.cumsum data using Game.Number as the X axis, cum.adjgsaa as the Y Axis, and color by the Name###
  geom_hline(yintercept = 0) + ###add a horizontal line at 0 on the Y axis###
  geom_line() + ###plot using a line graph###
  scale_alpha(range = c(0, 1)) +
  guides(alpha = FALSE) + ###turn off the color legend###
  xlab("Game Number") + ###label the X axis###
  ylab("Cumulative AdjGSAA") + ###label the Y axis###
  theme_bw() ###use a black and white theme###
ggsave("cumulative gsaa.png", width = 10, height = 10)

ggplot(filter(player_summary, toi > 3000), aes(reorder(name, adjgsaa), adjgsaa)) + ###plot the player summary data filtering out goalies with less than 3000 TOI. Name as the X axis, adjgsaa as the Y Axis, sort by adjgsaa###
  geom_bar(stat = "identity", position = "dodge", width = .5) + ###plot using a bar chart###
  coord_flip() + ###flip the axes###
  xlab(NULL) + ###no X axis label###
  ylab("AdjGSAA") + ###name the Y axis###
  ggtitle("NHL Goalies with > 3000 TOI, 2005-2016") +
  theme_bw() ###use a black and white theme###
ggsave("player summary gsaa.png", width = 6, height = 12)
