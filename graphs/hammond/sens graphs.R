library(readr)
library(dplyr)
library(RCurl)
library(ggplot2)
df <- read_csv("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/combined/adjgsaa.combined.csv") %>%
  mutate(season = as.character(season))

sens <- df %>%
  select(team, season, name, adjgsaa60, adjgsaa, toi) %>%
  filter((team == "OTT" & season %in% c("20142015", "20152016")) | name == "Mike.Condon") %>%
  filter(toi > 2) %>%
  group_by(name) %>%
  summarize(adjgsaa60 = mean(adjgsaa60),
            adjgsaa = sum(adjgsaa), 
            games_played = n()) %>%
  filter(games_played > 20) %>%
  arrange(desc(adjgsaa)) %>%
  mutate(name = as.factor(name))

ggplot(data = sens, aes(reorder(name, adjgsaa60), adjgsaa60, alpha = games_played)) + 
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", color = "grey55") +
  coord_flip() +
  labs(y = "Average Adj. GSAA per 60", x = NULL, title = "Ottawa Senators 2014-15 & 2015-16", subtitle = "(and Mike Condon)", caption = "@Null_HHockey") +
  scale_alpha_continuous(range = c(0, 1)) +
  guides(alpha = guide_legend(title = "Games Played")) +
  theme_nhh() +
  theme(panel.grid.major = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5))

?guides
ggplot(data = sens, aes(reorder(name, adjgsaa), adjgsaa, alpha = games_played)) + 
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", color = "grey55") +
  coord_flip() +
  labs(y = "Sum of Adj. GSAA", x = NULL, title = "Ottawa Senators 2014-15 & 2015-16", subtitle = "(and Mike Condon)", caption = "@Null_HHockey") +
  scale_alpha_continuous(range = c(0, 1)) +
  guides(alpha = guide_legend(title = "Games Played")) +
  theme_nhh() +
  theme(panel.grid.major = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5))
?subtitle
goalies <- c("Craig.Anderson", "Andrew.Hammond")
ggplot(filter(df, name == goalies), aes(game_number, adjgsaa60)) +
  geom_smooth() +
  facet_wrap(~name)