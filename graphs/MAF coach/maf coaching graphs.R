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
library(viridis)

source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")

maf <- read.csv(text=getURL("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/combined/adjgsaa.combined.csv")) %>%
  filter(name == "Marc-Andre.Fleury", toi > 2) %>%
  select(game_type, name, season, date, game_number, adjgsaa60) %>%
  group_by(name, season) %>%
  mutate(season_game_number = dense_rank(date)) %>%
  ungroup() %>%
  mutate(season = as.character(season), 
         name = as.character(name), 
         date = ymd(date),
         coach = ifelse(date < "2013-06-23", "Giles Meloche", "Mike Bales"))

ggplot(maf, aes(game_number, adjgsaa60)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 530) +
  geom_point(alpha = I(.5)) +
  geom_smooth(size = 2, span = .3) +
  geom_text(aes(585, -15, label="Coaching change"), size = 8) + 
  guides(color = guide_legend(title = "Goalie Coach"), fill = FALSE) +
  labs(y = "Adjusted GSAA per 60", x = "Game Number", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  theme_nhh()

ggplot(maf, aes(season_game_number, adjgsaa60)) +
  geom_hline(yintercept = 0) +
  #geom_vline(xintercept = 530) +
  geom_point(aes(color = coach, fill = coach), size = 1) +
  geom_smooth(aes(color = coach, fill = coach), size = 2, span = .3, show.legend = FALSE) +
  #geom_text(aes(585, -15, label="Coaching change", color = NULL), size = 8) + 
  #coord_cartesian(ylim = c(-20, 20)) +
  facet_wrap(~season, ncol = 1) +
  guides(color = guide_legend(title = "Goalie Coach"), fill = FALSE) +
  labs(y = "Adjusted GSAA per 60", x = "Game Number", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  theme_bw()


  

maf_summary <- maf %>%
  group_by(coach) %>%
  summarize(adjgsaa60_mean = mean(adjgsaa60, na.rm = TRUE),
            adjgsaa60_sd = sd(adjgsaa60, na.rm = TRUE)) %>%
  gather(measure, metric, -coach)

ggplot(filter(maf_summary, measure == "adjgsaa60_mean"), aes(measure, metric, fill = coach)) +
  geom_bar(stat = "identity", position = "dodge", color ="grey") +
  coord_cartesian(ylim = c(0, .5)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(y = "Mean AdjGSAA per 60", x = "Goalie Coach", title = "Marc-Andre Fleury Coach Analysis") +
  guides(fill = guide_legend(title = "Goalie Coach"), color = FALSE) +
  theme_nhh()

ggplot(filter(maf_summary, measure == "adjgsaa60_sd"), aes(measure, metric, fill = coach)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Standard Deviation of Adjusted GSAA per 60", x = "Goalie Coach", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  guides(fill = guide_legend(title = "Goalie Coach"), color = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_nhh()

#standard deviation line chart
maf %>%
  select(coach, season, adjgsaa60) %>%
  group_by(season, coach) %>%
  summarize(stddev = sd(adjgsaa60)) %>%
  ggplot(., aes(season, stddev, fill = coach, group = 1)) +
  geom_line() +
  ylim(c(0, 5))

ggplot(maf, aes(adjgsaa60, fill = coach)) +  
  geom_histogram(binwidth = .5, alpha = I(.8)) +
  geom_vline(xintercept = 0, size = 1) +
  facet_wrap(~coach, ncol = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  guides(fill = guide_legend(title = "Goalie Coach"), color = FALSE) +
  labs(x = "Adjusted GSAA per 60", y = "Count of games", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  theme_nhh()