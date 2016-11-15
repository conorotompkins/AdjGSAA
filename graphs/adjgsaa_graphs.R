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

test <- df_combined %>%
  unique(season)

abc <- df_combined %>%
  unique(season)

maf <- df_combined %>%
  filter(Name == "Marc-Andre.Fleury",
         TOI > 2) %>%
  select(Name, season, Date, Game.Number, adjgsaa60) %>%
  mutate(coach = ifelse(Date < "2013-04-28", "Giles Meloche", "Mike Bales"))

ggplot(maf, aes(Game.Number, adjgsaa60, color = coach, fill = coach)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = I(.5)) +
  geom_smooth(size = 2, span = .3) +
  #coord_cartesian(ylim = c(-40, 40)) +
  guides(color = guide_legend(title = "Goalie Coach"), fill = FALSE) +
  labs(y = "Adjusted GSAA per 60", x = "Game Number", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  theme_bw()
ggsave("fleury coach line graph.png")

?geom_smooth
?sd

maf_gp <- maf %>%
  select(coach, Date) %>%
  group_by(coach) %>%
  count()

maf_summary <- maf %>%
  group_by(coach) %>%
  summarize(adjgsaa60_mean = mean(adjgsaa60, na.rm = TRUE),
            adjgsaa60_sd = sd(adjgsaa60, na.rm = TRUE)) %>%
  gather(measure, metric, -coach)

ggplot(filter(maf_summary, measure == "adjgsaa60_mean"), aes(measure, metric, fill = coach)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_cartesian(ylim = c(0, .5)) +
  labs(y = "Mean AdjGSAA per 60", x = "Goalie Coach", title = "Marc-Andre Fleury Coach Analysis") +
  guides(fill = guide_legend(title = "Goalie Coach"), color = FALSE) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("maf coach mean.png")

ggplot(filter(maf_summary, measure == "adjgsaa60_sd"), aes(measure, metric, fill = coach)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Standard Deviation of Adjusted GSAA per 60", x = "Goalie Coach", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  guides(fill = guide_legend(title = "Goalie Coach"), color = FALSE) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("maf coach sd.png")

ggplot(maf, aes(adjgsaa60, fill = coach, color = coach)) +
  geom_density(alpha = I(.7)) +
  geom_vline(xintercept = 0) +
  labs(x = "Adjusted GSAA Per 60", y = "Density of observations", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  guides(fill = guide_legend(title = "Goalie Coach"), color = FALSE) +
  theme_bw()
ggsave("maf density plot.png")

ggplot(maf, aes(adjgsaa60, fill = coach)) +  
  geom_vline(xintercept = 0) +
  geom_histogram(binwidth = .5) +
  geom_vline(xintercept = 0) +
  facet_wrap(~coach) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  guides(fill = guide_legend(title = "Goalie Coach"), color = FALSE) +
  labs(x = "Adjusted GSAA per 60", y = "Count of games", title = "Marc-Andre Fleury Goalie Coach Comparison") +
  theme_bw()
ggsave("maf histogram.png")

goalie <- "Marc-Andre.Fleury"
ggplot(filter(df_combined, Name == goalie), aes(adjgsaa60, fill = season)) +
  geom_density(alpha = I(.7)) +
  geom_vline(xintercept = 0) +
  scale_y_continuous(label = percent) +
  scale_x_continuous(limits = c(-40, 8)) +
  labs(x = "Adjusted GSAA Per 60", y = "Density of observations", title = goalie) +
  guides(fill = guide_legend(title = "Season"), color = FALSE) +
  theme_bw()
