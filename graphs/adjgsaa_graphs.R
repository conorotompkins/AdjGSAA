setwd("C:/Users/conor/githubfolder/AdjGSAA/graphs")

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
library(RCurl)

source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")
theme_set(theme_nhh())

df_combined <- read.csv(text=getURL("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/combined/adjgsaa.combined.csv")) %>%
  mutate(season = as.character(season), 
         team = as.character(team), 
         name = as.character(name), 
         date = ymd(date)) %>%
  filter(toi > 2)

goalies <- c("Henrik.Lundqvist", "Steve.Mason")

ggplot(filter(df_combined, name %in% goalies), aes(game_number, adjgsaa60, color = name, fill = name)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 1, alpha = I(.8)) +
  geom_smooth(span = .3, se = T, size = 2, alpha = I(.5), show.legend=FALSE) +
  facet_wrap(~name, ncol = 1) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(y = "Adjusted Goals Saved Above Average Per 60", x = "Game Number", title = "2005-2016 5v5") +
  theme_bw()
ggsave("adjgaa60 goalie comparison.png")

combined_cumsum <- df_combined %>% ###create a data frame holding cumulative sums of adjGSAA
  select(name, date, game_number, adjgsaa) %>%
  group_by(name) %>%
  arrange(date) %>%
  mutate(cum_adjgsaa = cumsum(adjgsaa)) %>%
  ungroup()

player_summary <- df_combined %>% ###create a new object for player summary data###
  group_by(name) %>% ###group by goalie name###
  summarize(adjgsaa = sum(adjgsaa), ###sum up all the variables###
            lgsaa = sum(lgsaa),
            mgsaa = sum(mgsaa),
            hgsaa = sum(hgsaa),
            toi = sum(toi)) %>%
  arrange(desc(adjgsaa))

top_goalies <- player_summary %>%
  select(name, adjgsaa) %>%
  rename(sum_adjgsaa = adjgsaa)

combined_cumsum <- combined_cumsum %>%
  left_join(top_goalies) %>%
  arrange(desc(sum_adjgsaa)) %>%
  mutate(name = fct_reorder(name, -sum_adjgsaa), 
         max_date = max(date))


df2 <- combined_cumsum %>%
  filter(name %in% goalies)

df_filter <- player_summary %>%
  filter(toi > 3000) %>%
  select(name)

ggplot(filter(combined_cumsum, name %in% df_filter$name), aes(game_number, cum_adjgsaa, color = name)) +
  geom_hline(yintercept = 0) +
  geom_smooth(span = .3, se =  F) +
  labs(y = "Adj. Goals Saved Above Average", title = "Cumulative Adj. GSAA, career TOI > 3000") +
  guides(color = FALSE) +
  theme_bw()


season_sums <- df_combined %>%
  group_by(name, season) %>%
  select(name, season, adjgsaa60, toi) %>%
  summarize(sd_adjgsaa60 = sd(adjgsaa60, na.rm = TRUE),
            toi = sum(toi),
            adjgsaa60 =  mean(adjgsaa60, na.rm = TRUE)) %>%
  filter(toi > 500)

cor1 <- cor(season_sums$adjgsaa60, season_sums$sd_adjgsaa60) ^2
cor1 <- round(cor1, digits = 2)


ggplot(season_sums, aes(sd_adjgsaa60, adjgsaa60, label = name, alpha = toi)) +
  geom_hline(yintercept = 0) +
  geom_text() +
  geom_smooth(show.legend=FALSE) +
  labs(x = "Consistency (high to low)", y  = "Mean Adjusted GSAA per 60", title = paste("Goalie Quality versus Consistency, R^2 = ", cor1)) +
  theme_bw()
ggsave("Quality vs. Consistency.png")

ggplot(player.summary, aes(toi, adjgsaa, label = Name)) +
  geom_smooth() +
  geom_text() +
  theme_nhh()
ggsave("AdjGSAA vs TOI.png")

#player season summary bar chart
df_combined %>%
  select(season, name, adjgsaa60, toi) %>%
  mutate(key = paste0(name, season)) %>%
  group_by(key) %>%
  summarize(adjgsaa60 = mean(adjgsaa60),
            toi = sum(toi)) %>%
  filter(toi > 3000) %>%
  ggplot(aes(reorder(key, adjgsaa60), adjgsaa60)) +
    geom_col() +
    coord_flip()

ggsave("test plot.png")
?as.factor
?geom_bar
