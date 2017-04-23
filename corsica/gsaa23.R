library(tidyverse)

setwd("~/github folder/AdjGSAA/corsica")

theme_set(theme_bw())
df <- read_csv("2007-08 to 2016-17 big goalie data.csv")

colnames(df) <- tolower(colnames(df))

#create season colums
#create date range
seasons <- data.frame(season = c("2007-2008",
                                 "2008-2009"),
                      beginning = as.POSIXct(c("2007-10-01",
                                               "2008-10-01"), "%y-%m-%d"),
                      end = as.POSIXct(c("2008-05-01", "2009-05-01"), "%y-%m-%d"))

season20072008 <- data.frame(season = c("2007-2008"),
                 date = seq.POSIXt(as.POSIXct("2007-10-01"), as.POSIXct("2008-05-01"), by="day"))
warnings()

first_date <- as.POSIXct(min(my_data_df$trans_date), "%m/%d/%y")
last_date <- as.POSIXct(max(my_data_df$trans_date), "%m/%d/%y")

ts <- seq.POSIXt(first_date, last_date, by="day")
ts <- ymd_hms(ts)
ts <- ymd(substr(ts, 1, 10))

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
  group_by(player) %>% 
  arrange(date) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  arrange(player, date) %>% 
  mutate(season = "20162017") %>% 
  left_join(season) %>% 
  filter(sa >= 1, toi >= 5) %>% 
  mutate(diff = ga - xga,
         diffaa = diff - avg_diff,
         gsaa23 = (diffaa / sa) * 23) %>% 
  select(season, player, date, game_number, toi, sa, ga, xga, diff, diffaa, gsaa23) %>% 
  mutate(gsaa23_prev_25 = rollapply(data = gsaa23, 
                                    width = 25, 
                                    FUN = mean, 
                                    align = "right", 
                                    fill = NA, 
                                    na.rm = T),
         gsaa23_next_25 = rollapply(data = gsaa23, 
                                    width = 25, 
                                    FUN = mean, 
                                    align = "left", 
                                    fill = NA, 
                                    na.rm = T)) %>% 
  na.omit()

lundqvist <- predictions %>% 
  filter(player == "HENRIK.LUNDQVIST") %>% 
  select(player, game_number, gsaa23, gsaa23_prev_25, gsaa23_next_25) %>% 
  gather(metric, measure, -c(player, game_number))


ggplot(lundqvist, aes(x = game_number, color = metric)) +
  geom_smooth(aes(y = measure))

ggplot(predictions, aes(gsaa23_next_25, gsaa23_prev_25)) +
  geom_point(alpha = .1) +
  geom_smooth()
  
cor(predictions$gsaa23_prev_25, predictions$gsaa23_next_25)
#https://danieljhocking.wordpress.com/2014/12/03/lags-and-moving-means-in-dplyr/
library(zoo)  
?rollmean

df_xgsaa <- df_xgsaa %>% 
  group_by(player, season) %>% 
  arrange(date) %>% 
  mutate(
