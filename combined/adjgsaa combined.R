setwd("C:/Users/conor/githubfolder/AdjGSAA/combined")

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

df_regular_season <- read_csv("goalie.data.regular.season.csv") %>%
  mutate(season = as.character(season))

df_playoff_games <- read_csv("goalie.data.playoffs.csv") %>%
  mutate(season = as.character(season))

df_combined <- rbind(df_regular_season, df_playoff_games)

df_combined <- df_combined %>%
  group_by(name) %>% ###group the rows by goalie name###
  arrange(date) %>% ###arrange the rows by date###
  mutate(game_number = dense_rank(date),
         season = as.character(season)) %>% ###create the "game number" variable"###
  ungroup()
write.csv(df_combined, "adjgsaa.combined.csv")




