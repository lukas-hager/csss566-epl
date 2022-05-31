# This code has the objective of replicating the analysis made by 
# Krumer and Lechner to the EPL in order to study the causal effect 
# of playing on midweek 


###### ------------ 01. LIBRARIES AND DATA ----------------------------

# The data preparation has already been done

rm(list = ls())
{
  library(tidyverse)
  library(lubridate)
}


# We read the data 

data_midweek <- read_csv("data_midweek_final.csv")

#

###### ------------ 02. DESCRIPTIVE STATISTICS ----------------------------

# We start by thinking that the midweek assignment has some causal effect 
# on game outcomes

data_midweek %>% 
  group_by(midweek) %>% 
  tally()

data_midweek %>% 
  group_by(midweek, wday(date)) %>% 
  tally()


desc_stats <- data_midweek %>% 
  group_by(midweek) %>% 
  summarize(
    points_home_team = mean(points_home),
    points_away_team = mean(points_away),
    goals_home_team = mean(fthg),
    goals_away_team = mean(ftag),
    shots_home_team = mean(hs),
    shots_away_team = mean(as),
    shots_ot_home_team = mean(hst),
    shots_ot_away_team = mean(ast),
    fouls_home_team = mean(hf),
    fouls_away_team = mean(af),
    yellow_cards_home_team = mean(hy),
    yellow_cards_away_team = mean(ay),
    red_cards_home_team = mean(hr),
    red_cards_away_team = mean(ar)
  )


# Maybe by short week we can find some difference

data_midweek %>% 
  group_by(ht_short_week) %>% 
  summarize(points_home_team = mean(points_home),
            points_away_team = mean(points_away))


data_midweek %>% 
  group_by(at_short_week) %>% 
  summarize(points_home_team = mean(points_home),
            points_away_team = mean(points_away))

# Maybe if we look at rest *differential* as well, we can get some difference

rest_days <- data_midweek %>% 
  dplyr::select(date, season, home_team, away_team) %>% 
  pivot_longer(home_team:away_team) %>% 
  dplyr::select(-name) %>% 
  arrange(value, date) %>% 
  group_by(season) %>% 
  mutate(rest_days = as.numeric(date - lag(date)))

data_midweek %>% 
  left_join(rest_days %>% 
              rename(home_rest_days = rest_days),
            by = c('date', 'season', 'home_team' = 'value')) %>% 
  left_join(rest_days %>% 
              rename(away_rest_days = rest_days),
            by = c('date', 'season', 'away_team' = 'value')) %>% 
  mutate(home_rest_diff = home_rest_days - away_rest_days,
         away_rest_diff = away_rest_days - home_rest_days) %>% 
  group_by(home_rest_diff) %>% 
  summarise(points_home_team = mean(points_home),
            points_away_team = mean(points_away),
            n = n()) %>% 
  ungroup() %>% 
  filter(!is.na(home_rest_diff) & between(home_rest_diff,-21,21), n>=10) %>% 
  ggplot(data = .) + 
  geom_point(aes(x = home_rest_diff, y = points_home_team, size = n))

# What is the distribution of 

data_midweek %>% 
  dplyr::select(date, season, home_team, away_team, midweek) %>% 
  pivot_longer(home_team:away_team) %>% 
  group_by(value, season) %>% 
  summarise(total_midweek = sum(midweek)) %>% 
  ungroup() %>% 
  pull(total_midweek) %>% 
  summary()
