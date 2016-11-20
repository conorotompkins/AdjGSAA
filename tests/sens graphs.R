library(readr)
library(dplyr)
library(RCurl)
library(ggplot2)
df <- read_csv("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/combined/adjgsaa.combined.csv") %>%
  mutate(season = as.character(season))
unique(df$season)
sens <- df %>%
  select(team, season, name, adjgsaa60, adjgsaa) %>%
  filter(team == "OTT") %>%
  group_by(name) %>%
  summarize(adjgsaa60 = mean(adjgsaa60),
            adjgsaa = sum(adjgsaa), 
            count = n()) %>%
  filter(count > 20) %>%
  arrange(desc(adjgsaa)) %>%
  mutate(name = as.factor(name))

ggplot(data = sens, aes(name, adjgsaa, alpha = count)) + 
  geom_bar(stat = "identity", color = "grey") +
  coord_flip() +
  theme_bw()

goalies <- c("Craig.Anderson", "Andrew.Hammond")
ggplot(filter(df, name == goalies), aes(game_number, adjgsaa60)) +
  geom_smooth() +
  facet_wrap(~name)