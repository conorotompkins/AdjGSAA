library(tidyverse)
library(lubridate)
library(zoo)
setwd("~/github folder/AdjGSAA/corsica")

#https://danieljhocking.wordpress.com/2014/12/03/lags-and-moving-means-in-dplyr/

theme_set(theme_bw())
df_goalie <- read_csv("2007-08 to 2016-17 big goalie data.csv")

colnames(df_goalie) <- tolower(colnames(df_goalie))

season20072008 <- data.frame(season = c("2007-2008"),
                             date = seq.POSIXt(as.POSIXct("2007-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2008-07-01", "%y-%m-%d"), by="day"))
season20082009 <- data.frame(season = c("2008-2009"),
                             date = seq.POSIXt(as.POSIXct("2008-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2009-07-01", "%y-%m-%d"), by="day"))
season20092010 <- data.frame(season = c("2009-2010"),
                             date = seq.POSIXt(as.POSIXct("2009-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2010-07-01", "%y-%m-%d"), by="day"))
season20102011 <- data.frame(season = c("2010-2011"),
                             date = seq.POSIXt(as.POSIXct("2010-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2011-07-01", "%y-%m-%d"), by="day"))
season20112012 <- data.frame(season = c("2011-2012"),
                             date = seq.POSIXt(as.POSIXct("2011-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2012-07-01", "%y-%m-%d"), by="day"))
season20122013 <- data.frame(season = c("2012-2013"),
                             date = seq.POSIXt(as.POSIXct("2012-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2013-07-01", "%y-%m-%d"), by="day"))
season20132014 <- data.frame(season = c("2013-2014"),
                             date = seq.POSIXt(as.POSIXct("2013-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2014-07-01", "%y-%m-%d"), by="day"))
season20142015 <- data.frame(season = c("2014-2015"),
                             date = seq.POSIXt(as.POSIXct("2014-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2015-07-01", "%y-%m-%d"), by="day"))
season20152016 <- data.frame(season = c("2015-2016"),
                              date = seq.POSIXt(as.POSIXct("2015-10-01", "%y-%m-%d"), 
                                                as.POSIXct("2016-07-01", "%y-%m-%d"), by="day"))
season20162017 <- data.frame(season = c("2016-2017"),
                             date = seq.POSIXt(as.POSIXct("2016-10-01", "%y-%m-%d"), 
                                               as.POSIXct("2017-07-01", "%y-%m-%d"), by="day"))

season_list <- list(season20072008, 
                                season20082009,
                                season20092010,
                                season20102011,
                                season20112012,
                                season20122013,
                                season20132014,
                                season20142015,
                                season20152016, 
                                season20162017)

df_season <- bind_rows(season_list) %>% 
  mutate(date = ymd(date),
         season = factor(season))
str(df_season$season)

df_goalie <- df_goalie %>% 
  left_join(df_season)

df_goalie <- df_goalie %>% 
  rename(sv_percent = `sv%`)

bad <- df_goalie[is.na(df_goalie$season), ] %>% 
  select(player, date, season)




season <- df_goalie %>% 
  filter(toi > 20) %>% 
  group_by(season) %>% 
  summarize(toi = mean(toi),
            sa = mean(sa),
            ga = mean(ga),
            xga = mean(xga)) %>% 
  mutate(avg_diff = ga - xga) %>% 
  select(season, avg_diff)

predictions <- df_goalie %>%
  group_by(player, season) %>% 
  arrange(date) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  arrange(player, date) %>% 
  left_join(season) %>% 
  filter(sa >= 1, toi >= 5) %>% 
  mutate(diff = ga - xga,
         diffaa = diff - avg_diff,
         gsaa23 = (diffaa / sa) * 23) %>% 
  select(season, player, date, game_number, toi, sa, ga, xga, diff, diffaa, gsaa23, sv_percent)

predictions <- predictions %>%
  group_by(player, season) %>% 
  mutate(gp = sum(n())) %>% 
  ungroup() %>% 
  filter(gp >= 50)

window_size <- 30
predictions <-  predictions %>% 
  select(season, player, date, game_number, gsaa23, sv_percent) %>% 
  group_by(player, season) %>% 
  arrange(player, desc(season), date) %>% 
  mutate(gsaa23_prev_mean = rollapply(data = gsaa23, 
                                    width = list(-1:-window_size), 
                                    FUN = mean, 
                                    align = "right", 
                                    fill = NA, 
                                    na.rm = T),
         gsaa23_next_mean = rollapply(data = gsaa23, 
                                      width = list(1:window_size), 
                                      FUN = mean, 
                                      align = "right", 
                                      fill = NA, 
                                      na.rm = T),
         gsaa23_prev_sd = rollapply(data = gsaa23, 
                                    width = list(1:-window_size), 
                                    FUN = sd, 
                                    align = "right", 
                                    fill = NA, 
                                    na.rm = T),
         sv_percent_prev = rollapply(data = sv_percent, 
                                        width = list(-1:-window_size), 
                                        FUN = mean, 
                                        align = "right", 
                                        fill = NA, 
                                        na.rm = T),
         sv_percent_next = rollapply(data = sv_percent, 
                                    width = list(1:window_size), 
                                    FUN = mean, 
                                    align = "right", 
                                    fill = NA, 
                                    na.rm = T))
           
           
lundqvist <- predictions %>% 
  filter(player == "HENRIK.LUNDQVIST") %>% 
  select(player, season, game_number, gsaa23, gsaa23_prev_25, gsaa23_next_25)

lundqvist <- lundqvist %>% 
  gather(metric, measure, -c(player, season, game_number))


lundqvist %>% 
  filter(season %in% c("2008-2009", "2009-2010")) %>% 
  ggplot(aes(x = game_number, color = metric, fill = metric)) +
  geom_hline(yintercept = 0) +
  geom_smooth(aes(y = measure)) +
  facet_wrap(~season, ncol = 1) +
  labs(title = "Henrik Lundqvist") +
  scale_y_reverse()


ggplot(predictions, aes(sv_percent_prev_25, sv_percent_next_25)) +
  geom_point(alpha = .1) +
  geom_smooth()

ggplot(predictions, aes(gsaa23_prev_25, gsaa23_next_25)) +
  geom_point(alpha = .15) +
  geom_smooth()

model_gsaa23 <- lm(gsaa23_next_mean ~ gsaa23_prev_mean + gsaa23_prev_sd, data = predictions)  
summary(model_gsaa23)
plot.lm(model_gsaa23)
?plot.lm
model_sv_percent <- lm(sv_percent_next_25 ~ sv_percent_prev_25, data = predictions)  
summary(model_sv_percent)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_sv_percent)

predictions$fit<- predict(model_gsaa23)

predictions_clean <- predictions %>% 
  na.omit()
cor(predictions_clean$gsaa23_prev_25, predictions_clean$gsaa23_next_25)
cor(predictions_clean$sv_percent_prev_25, predictions_clean$sv_percent_next_25)


