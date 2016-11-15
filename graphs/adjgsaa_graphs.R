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
library(viridis)

df.regular.season <- read_csv("regular.season.mercad.csv") %>%
  mutate(season = as.character(season)) %>%
  filter(TOI > 2)

goalies <- c("Henrik.Lundqvist", "Steve.Mason")

ggplot(filter(df.regular.season, Name %in% goalies), aes(Game.Number, adjgsaa60, color = Name, fill = Name)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 1, alpha = I(.8)) +
  geom_smooth(span = .3, se = T, size = 2, alpha = I(.5), show.legend=FALSE) +
  facet_wrap(~Name, ncol = 1) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(y = "Adjusted Goals Saved Above Average Per 60", x = "Game Number", title = "2005-2016 5v5") +
  theme_nhh()
  
  theme_bw() +
  theme(strip.text.x = element_text(size = 20),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"))
ggsave("adjgaa60 test.png")
?scale_fill_viridis
?geom_smooth


df1 <- df.regular.season %>%
  filter(Name %in% goalies) %>%
  select(Name, Game.Number, adjgsaa60)

ggplot(df1, aes(Game.Number, adjgsaa60, color = Name, fill = Name)) +
  geom_hline(yintercept = 0) +
  geom_smooth(span = .3) +
  labs(y = "Adjusted Goals Saved Above Average Per 60", title = NULL) + 
  theme_nhh()

?theme_bw()



rs.cumsum <- read_csv("rs.cumsum.csv") %>%
  mutate(season = as.character(season))

df2 <- rs.cumsum %>%
  filter(Name %in% goalies)

df_filter <- player.summary %>%
  filter(toi > 3000) %>%
  select(Name)

ggplot(filter(rs.cumsum, Name %in% df_filter$Name), aes(Game.Number, cum.adjgsaa, color = Name)) +
  geom_hline(yintercept = 0) +
  geom_smooth(span = .3, se =  F) +
  labs(y = "Adjusted Goals Saved Above Average", title = "Marc-Andre Fleury") +
  theme_nhh()


season_sums <- df.regular.season %>%
  group_by(Name, season) %>%
  select(Name, season, adjgsaa60, TOI) %>%
  summarize(sd_adjgsaa60 = sd(adjgsaa60, na.rm = TRUE),
            TOI = sum(TOI),
            adjgsaa60 =  mean(adjgsaa60, na.rm = TRUE)) %>%
  filter(TOI > 500)

cor1 <- cor(season_sums$adjgsaa60, season_sums$sd_adjgsaa60) ^2
cor1 <- round(cor1, digits = 2)


ggplot(season_sums, aes(sd_adjgsaa60, adjgsaa60, label = Name, alpha = TOI)) +
  geom_hline(yintercept = 0) +
  geom_text() +
  geom_smooth(show.legend=FALSE) +
  labs(x = "Consistency (high to low)", y  = "Mean Adjusted GSAA per 60", title = "Goalie Quality versus Consistency") +
  annotate("text", x = 5, y = 1.5, label = paste0("R^2 = ", cor1)) +
  theme(axis.line = axis_custom()) +
  theme_nhh()
ggsave("Quality vs. Consistency.png")

ggplot(player.summary, aes(toi, adjgsaa, label = Name)) +
  geom_smooth() +
  geom_text() +
  theme_nhh()
ggsave("AdjGSAA vs TOI.png")

test <- df.regular.season %>%
  unique(season)

abc <- df.regular.season %>%
  unique(season)

maf <- df.regular.season %>%
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
ggplot(filter(df.regular.season, Name == goalie), aes(adjgsaa60, fill = season)) +
  geom_density(alpha = I(.7)) +
  geom_vline(xintercept = 0) +
  scale_y_continuous(label = percent) +
  scale_x_continuous(limits = c(-40, 8)) +
  labs(x = "Adjusted GSAA Per 60", y = "Density of observations", title = goalie) +
  guides(fill = guide_legend(title = "Season"), color = FALSE) +
  theme_bw()
