###set your working directory. you can also do this using the GUI (Session -> Set Working Directory)
setwd("C:/Users/conor/Dropbox/R/goalie_analysis")
###these are the packages I use regularly. you need to install them (install.packages("packagename")) before loading them into the session with library(packagename)
library(ggthemes)
library(scales)
library(ggplot2)
library(ggmap)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(tidyr)
library(cowplot)
library(forcats)


###ok let's load the data into R###
big.goalie.data <- read.csv("big.goalie.data.csv") ###this file has to be in the R directory you set previously###
testdata <- read.csv("big.goalie.data.csv") ###create a backup just in case###
big.goalie.data$season <- as.character(big.goalie.data$season) ###turn the season variable into a factor###
big.goalie.data$Date <- ymd(big.goalie.data$Date) ###turn the date of the game into a format R can work with###
big.goalie.data$Team <- as.character(big.goalie.data$Team) ###turn the Team variable into a character string so we can modify it##
big.goalie.data$game_type <- "regular_season"

big.goalie.data <- big.goalie.data %>% ###modify some variables so they are easier to work with
  mutate(svH = Sv.H / 100, ###turn the sv% variables into decimals (99 -> .99)
         svM = Sv.M / 100,
         svL = Sv.L / 100,
         saH = G.M + S.H, ###goals + shots = saves###
         saM = G.M + S.M,
         saL = G.L + S.L, 
         Name = as.character(Name))

big.goalie.data <- big.goalie.data %>% ###select the variables we want and omit any rows with NA (blank) values###
  select(game_type, Team, Name, Age, season, Date, TOI, svH, svM, svL, saH, saM, saL, G.H, G.M, G.L)

###combine Atlanta/Winnipeg and Phoenix/Arizona Team names
big.goalie.data$Team[big.goalie.data$Team == "WPG"]<-"ATL-WPG"
big.goalie.data$Team[big.goalie.data$Team == "ATL"]<-"ATL-WPG"
big.goalie.data$Team[big.goalie.data$Team == "PHX"]<-"PHX-ARI"
big.goalie.data$Team[big.goalie.data$Team == "ARI"]<-"PHX-ARI"

###Creating Per-Season Averages###
#RM_NA
season.averages <- big.goalie.data %>% ###create a new object in R to hold the per-season per-goalie averages###
  select(Name, TOI, season, svH, svM, svL, saH, saM, saL) %>% ###select the variables we want to work with###
  group_by(Name, season) %>% ###group the rows by name and season###
  summarize(TOI = sum(TOI), ###sum the TOI for the new rows we created###
            svH = mean(svH, na.rm = TRUE), ###take the average###
            svM = mean(svM, na.rm = TRUE), ###etc###
            svL = mean(svL, na.rm = TRUE),
            saH = sum(saH),
            saM = sum(saM),
            saL = sum(saL))

###create the per-season averages###
season.averages <- season.averages %>%
  group_by(season) %>% ###group the rows by season###
  summarize(la.svH = mean(svH, na.rm = TRUE), ###take the means of the danger save percentages###
            la.svM = mean(svM, na.rm = TRUE), 
            la.svL = mean(svL, na.rm = TRUE))
write.csv(season.averages, "mercad.season.averages.csv") ###create a csv of the season averages object. take out the preceding "#"s if you want to run this line###

###Create Regular Season Goalie Statistics###
regular.season <- big.goalie.data %>% ###create a new object###
  select(game_type, Team, Name, season, Date, TOI, svH, svM, svL, saH, saM, saL, G.H, G.M, G.L) ###select the variables we want###

goalie.join.regular.season <- inner_join(regular.season, season.averages, by = "season") ###join the individual goalie data with the season average data###

df.regular.season <- goalie.join.regular.season %>% ###create a new object###
  mutate(lgsaa = (saL *(1 - la.svL) - G.L), ###create new variables. LGSAA = (low danger shots against * (1 - league average low danger save %) - low danger goals against)###
         mgsaa = (saM *(1 - la.svM) - G.M),
         hgsaa = (saH *(1 - la.svH) - G.H),
         adjgsaa60 = ((lgsaa + mgsaa + hgsaa) / TOI) * 60, ###create adjGSAA60###
         adjgsaa = lgsaa + mgsaa + hgsaa) ###create adjGSAA###

df.regular.season <- df.regular.season %>%
  group_by(Name) %>% ###group the rows by goalie name###
  arrange(Date) %>% ###arrange the rows by date###
  mutate(Game.Number = dense_rank(Date),
         season = as.character(season)) %>% ###create the "game number" variable"###
  ungroup()

player.summary <- df.regular.season %>% ###create a new object for player summary data###
  group_by(Name) %>% ###group by goalie name###
  summarize(adjgsaa = sum(adjgsaa), ###sum up all the variables###
            lgsaa = sum(lgsaa),
            mgsaa = sum(mgsaa),
            hgsaa = sum(hgsaa),
            toi = sum(TOI)) %>%
  arrange(desc(adjgsaa))

write.csv(df.regular.season, "regular.season.mercad.csv") ###export to csv. take out the preceding "#"s if you want to run this line###

df.regular.season <- read_csv("regular.season.mercad.csv") %>%
  mutate(season = as.character(season))

rs.cumsum <- df.regular.season %>% ###create a data frame holding cumulative sums of adjGSAA
  select(Name, Date, Game.Number, adjgsaa) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(cum.adjgsaa = cumsum(adjgsaa)) %>%
  ungroup()

top_goalies <- player.summary %>%
  select(Name, adjgsaa) %>%
  rename(sum_adjgsaa = adjgsaa)

rs.cumsum <- rs.cumsum %>%
  left_join(top_goalies) %>%
  arrange(desc(sum_adjgsaa)) %>%
  mutate(Name = fct_reorder(Name, -sum_adjgsaa))

write.csv(rs.cumsum, "rs.cumsum.csv")

###Regular Season Graphs###
ggplot(rs.cumsum, aes(Game.Number, cum.adjgsaa, color = Name)) + ###plot the rs.cumsum data using Game.Number as the X axis, cum.adjgsaa as the Y Axis, and color by the Name###
  geom_hline(yintercept = 0) + ###add a horizontal line at 0 on the Y axis###
  geom_line() + ###plot using a line graph###
  guides(color = FALSE) + ###turn off the color legend###
  xlab("Game Number") + ###label the X axis###
  ylab("Cumulative AdjGSAA") + ###label the Y axis###
  theme_bw() ###use a black and white theme###
ggsave("cumulative gsaa.png", width = 10, height = 10)

ggplot(filter(player.summary, toi > 3000), aes(reorder(Name, adjgsaa), adjgsaa)) + ###plot the player summary data filtering out goalies with less than 3000 TOI. Name as the X axis, adjgsaa as the Y Axis, sort by adjgsaa###
  geom_bar(stat = "identity", position = "dodge", width = .5) + ###plot using a bar chart###
  coord_flip() + ###flip the axes###
  xlab(NULL) + ###no X axis label###
  ylab("AdjGSAA") + ###name the Y axis###
  ggtitle("NHL Goalies with > 3000 TOI, 2005-2016") +
  theme_bw() ###use a black and white theme###
ggsave("player summary gsaa.png", width = 6, height = 12)

ggplot(filter(df.regular.season, Name == "Marc-Andre.Fleury"), aes(Game.Number, adjgsaa60)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  labs(y = "Adjusted Goals Saved Above Average Per 60", title = "Marc-Andre Fleury") +
  theme_bw()
ggsave("maf mercad.png")

ggplot(df.regular.season, aes(TOI, adjgsaa60)) +
  geom_point(alpha = I(.3)) +
  theme_bw()
