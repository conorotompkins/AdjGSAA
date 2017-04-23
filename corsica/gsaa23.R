library(tidyverse)

setwd("~/github folder/AdjGSAA/corsica")

df <- read_csv("1617 5v5 individual game goalie data.csv")

colnames(df) <- tolower(colnames(df))

season <- df %>% 
  filter(toi > 20) %>% 
  mutate(season = "20162017") %>% 
  group_by(season) %>% 
  summarize(toi = mean(toi),
            sa = mean(sa),
            ga = mean(ga),
            xga = mean(xga)) %>% 
  mutate(avg_diff = ga - xga) %>% 
  select(season, avg_diff)

df_xgsaa <- df %>% 
  mutate(season = "20162017") %>% 
  left_join(season) %>% 
  filter(sa >= 1, toi >= 5) %>% 
  mutate(diff = ga - xga,
         diffaa = diff - avg_diff,
         gsaa23 = (diffaa / sa) * 23) %>% 
  select(season, player, date, toi, sa, ga, xga, diff, diffaa, gsaa23) %>% 
  group_by(player) %>% 
  summarize(toi = sum(toi),
            gsaa23_sum = sum(gsaa23),
            gsaa23_avg = round(mean(gsaa23), digits = 2),
            gsaa23_sd = round(sd(gsaa23), digits = 2)) %>% 
  filter(toi >= 1000)

ggplot(df_xgsaa, aes(gsaa23_avg, toi, label = player)) +
  geom_label() +
  scale_x_reverse() +
  theme_bw()

ggplot(df_xgsaa, aes(gsaa23_sum, toi, label = player)) +
  geom_label() +
  scale_x_reverse() +
  theme_bw()

ggplot(df_xgsaa, aes(gsaa23_avg, gsaa23_sd, label = player)) +
  geom_label() +
  scale_x_reverse() +
  theme_bw()

predictions <- df %>% 
  mutate(season = "20162017") %>% 
  left_join(season) %>% 
  filter(sa >= 1, toi >= 5) %>% 
  mutate(diff = ga - xga,
         diffaa = diff - avg_diff,
         gsaa23 = (diffaa / sa) * 23) %>% 
  select(season, player, date, toi, sa, ga, xga, diff, diffaa, gsaa23) %>% 
  mutate(prev_gsaa23 = lag(gsaa23)) %>% 
  na.omit()

ggplot(predictions, aes(gsaa23, prev_gsaa23)) +
  geom_point(alpha = .1)
  

https://danieljhocking.wordpress.com/2014/12/03/lags-and-moving-means-in-dplyr/