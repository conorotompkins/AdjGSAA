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

maf <- df_combined <- read.csv(text=getURL("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/combined/adjgsaa.combined.csv")) %>%
  filter(name == "Marc-Andre.Fleury", toi > 2) %>%
  select(game_type, name, season, date, game_number, adjgsaa60) %>%
  mutate(coach = ifelse(date < "2013-06-23", "Giles Meloche", "Mike Bales"), 
         season = as.character(season), 
         team = as.character(team), 
         name = as.character(name), 
         date = ymd(date))

ggplot(maf, aes(game_number, adjgsaa60)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 530) +
  geom_point(alpha = I(.5)) +
  geom_smooth(size = 2, span = .3) +
  geom_text(aes(585, -15, label="Coaching change"), size = 8) + 
  guides(color = guide_legend(title = "Goalie Coach"), fill = FALSE) +
  labs(y = "Adjusted GSAA per 60", x = "Game Number", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  theme_nhh()

ggplot(maf, aes(Game.Number, adjgsaa60, color = coach, fill = coach)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 530) +
  geom_point(size = 2, alpha = I(.2)) +
  geom_smooth(size = 2, span = .3) +
  geom_text(aes(585, -15, label="Coaching change", color = NULL), size = 8) + 
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  guides(fill = guide_legend(title = "Goalie Coach"), color = FALSE) +
  labs(y = "Adjusted GSAA per 60", x = "Game Number", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  theme_nhh()

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