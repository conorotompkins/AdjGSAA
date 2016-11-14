setwd("C:/Users/conor/Dropbox/R/goalie_analysis")

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

df.regular.season <- read_csv("regular.season.mercad.csv") %>%
  mutate(season = as.character(season))

df.playoff.games <- read_csv("playoff.games.mercad.csv") %>%
  mutate(season = as.character(season))

mercad_combined <- rbind(df.regular.season, df.playoff.games)

mercad_combined <- mercad_combined %>%
  group_by(Name) %>% ###group the rows by goalie name###
  arrange(Date) %>% ###arrange the rows by date###
  mutate(Game.Number = dense_rank(Date),
         season = as.character(season)) %>% ###create the "game number" variable"###
  ungroup()
write.csv(mercad_combined, "adjgsaa.combined.csv")




