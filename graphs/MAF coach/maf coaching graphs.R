setwd("C:/Users/conor/githubfolder/AdjGSAA/graphs/MAF coach")

library(tidyverse)
library(viridis)

source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")
theme_set(theme_nhh())

subtitle <- "Games with >2 TOI"
caption <- "@NHH_Hockey"

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
  geom_text(aes(600, -15, label="Coaching change"), size = 6) + 
  guides(color = guide_legend(title = "Goalie Coach"), fill = FALSE) +
  labs(y = "Adjusted GSAA per 60", x = "Game Number", title = "Marc-Andre Fleury Goalie Coach Comparison", caption = caption) +
  theme(plot.caption = element_text(size=12, hjust=1))
ggsave("MAF career line chart.png", width = 12, height = 6)

ggplot(maf, aes(game_number, adjgsaa60, color = coach, fill = coach)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 530) +
  geom_point(alpha = I(.5)) +
  geom_smooth(size = 2, span = .3) +
  geom_text(aes(650, -15, label="Coaching change"), size = 6, color = "black") + 
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  guides(color = guide_legend(title = "Goalie Coach"), fill = FALSE) +
  labs(y = "Adjusted GSAA per 60", x = "Game Number", title = "Marc-Andre Fleury Goalie Coach Comparison", caption = caption) +
  theme(plot.caption = element_text(size=12, hjust=1))
ggsave("MAF career line chart with coaches.png", width = 12, height = 6)


maf_coach_summary <- maf %>%
  group_by(coach) %>%
  summarize(adjgsaa60_mean = mean(adjgsaa60, na.rm = TRUE),
            adjgsaa60_sd = sd(adjgsaa60, na.rm = TRUE)) 

diff <- .2033 - .0704
(diff * 3000) / 60
diff * 100

ggplot(maf_coach_summary, aes(coach, adjgsaa60_mean, fill = coach)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = "Mean AdjGSAA per 60", x = "Goalie Coach", title = "Marc-Andre Fleury Coach Analysis", caption = caption) +
  guides(fill = FALSE) +
  theme(plot.caption = element_text(size=12, hjust=1))
ggsave("coach gssaa60 mean bar chart.png", width = 12, height = 12)

ggplot(maf_coach_summary, aes(coach, adjgsaa60_sd, fill = coach)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(y = "Standard Deviation of Adjusted GSAA per 60", x = "Goalie Coach", title = "Marc-Andre Fleury Goalie Coach Comparison", caption = caption) +
  guides(fill = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  theme(plot.caption = element_text(size=12, hjust=1))
ggsave("coach gssaa60 stddev bar chart.png", width = 12, height = 12)

#standard deviation line chart
maf %>%
  select(coach, season, adjgsaa60) %>%
  group_by(season, coach) %>%
  summarize(stddev = sd(adjgsaa60)) %>%
  ggplot(., aes(season, stddev, color = coach, group = 1)) +
  geom_line(size = 2) +
  ylim(c(0, 5)) +
  labs(x = "Season", y = "Standard Deviation of Adjusted GSAA per 60", title = "Marc-Andre Fleury Goalie Coach Comparison", caption = caption) +
  scale_color_viridis(discrete = TRUE) +
  guides(color = guide_legend(title = "Goalie Coach")) +
  theme(plot.caption = element_text(size=12, hjust=1),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 10))
ggsave("coach gssaa60 stddev line chart.png", width = 10, height = 6)

ggplot(maf, aes(adjgsaa60, fill = coach)) +  
  geom_histogram(binwidth = .5, alpha = I(.8)) +
  geom_vline(xintercept = 0, size = 1) +
  facet_wrap(~coach, ncol = 1) +
  coord_cartesian(ylim = c(0, 70)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Adjusted GSAA per 60", y = "Count of games", title = "Marc-Andre Fleury Goalie Coach Comparison", caption = caption) +
  guides(fill = FALSE) +
  theme(plot.caption = element_text(size=12, hjust=1),
        plot.subtitle = element_text(size = 12))
ggsave("MAF coaches histogram.png", width = 12, height = 12)


df_combined <- read_csv("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/combined/adjgsaa.combined.csv")

df_combined %>%
  select(season, name, adjgsaa60, toi) %>%
  filter(toi > 2) %>%
  mutate(key = paste0(name, season)) %>%
  group_by(key) %>%
  summarize(adjgsaa60 = mean(adjgsaa60),
            toi = sum(toi)) %>%
  filter(toi > 3000) %>%
  ggplot(aes(reorder(key, adjgsaa60), adjgsaa60)) +
  geom_col(width = 1, alpha = I(.5), fill = "black") +
  geom_hline(yintercept = .07, color = "purple", size = 1.5) +
  geom_hline(yintercept = .2, color = "yellow", size = 1.5) +
  coord_flip() +
  labs(title = "Goalie seasons > 3000 TOI",
       y = "Adj. GSAA Per 60",
       x = "Goalies",
       caption = "@Null_HHockey") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 1),
        panel.background = element_rect(fill = "grey70"))
ggsave("MAF gsaa difference.png", width = 12, height = 12)

test <- df_combined %>%
  select(season, name, adjgsaa60, toi) %>%
  filter(toi > 2) %>%
  mutate(key = paste0(name, season)) %>%
  group_by(key) %>%
  summarize(adjgsaa60 = mean(adjgsaa60),
            toi = sum(toi)) %>%
  filter(toi > 3000)


df_combined %>%
  select(season, name, adjgsaa60, toi) %>%
  filter(toi > 2) %>%
  mutate(key = paste0(name, season)) %>%
  group_by(key) %>%
  summarize(adjgsaa60_stddev = sd(adjgsaa60),
            toi = sum(toi)) %>%
  filter(toi > 3000) %>%
  ggplot(aes(reorder(key, adjgsaa60_stddev), adjgsaa60_stddev)) +
  geom_col(width = 1, alpha = I(.5), fill = "black") +
  geom_hline(yintercept = 2.4, color = "purple", size = 1.5) +
  geom_hline(yintercept = 1.9, color = "yellow", size = 1.5) +
  coord_flip() +
  labs(title = "Goalie seasons > 3000 TOI",
       y = "Standard Deviations of Adj. GSAA Per 60",
       x = "Goalies",
       caption = "@Null_HHockey") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 1),
        panel.background = element_rect(fill = "grey70"))
ggsave("MAF gsaa stddev.png", width = 12, height = 12)


?viridis
viridis()
viridis_pal()
