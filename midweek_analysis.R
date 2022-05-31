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




# There seems to be a difference in the % of matches each team 
# played midweek as host
data_midweek %>% 
  group_by(home_team, midweek) %>% 
  tally() %>%
  ungroup() %>% spread(key = midweek, value = n) %>% 
  mutate(percentage_mw = `1`/(`1` + `0`)*100) %>% 
  arrange(desc(percentage_mw)) %>% View()

data_midweek %>% 
  group_by(home_team, midweek, season) %>% 
  tally() %>%
  ungroup() %>% spread(key = midweek, value = n) %>% 
  mutate(percentage_mw = `1`/(`1` + `0`)*100) %>% 
  arrange(desc(percentage_mw)) %>% View()


# Midweek assignation does not seem to be randomly assigned after all 


# We are going to assume conditional independence between X (midweek games)
# and the outcome Y (points)

# Now, we do not now exactly how the assignment is made, only that it takes into account:
# - International matches
# - Christmas day (Boxing Day matches and New Year)

data_midweek <- data_midweek %>% 
  mutate(Holidays = case_when(
    (month(date) == 12 & day(date)>=25) | (month(date) == 1 & day(date) <= 2) ~ 1,
    TRUE ~0
  ))

# We are going to define the set Z as acn, before_national_comps, after_national_comps, 
# ln_capacity, distance and Holidays in order to estimate P(X|Z) and then apply IPW


XZmodel <- glm(as.factor(midweek) ~ as.factor(acn) + as.factor(before_national_comps) + 
                 as.factor(after_national_comps) + 
                 ln_capacity + distance + as.factor(Holidays) , 
               family = "binomial",
               data = data_midweek)

summary(XZmodel)


# What to do with all the non-significant variables?
# Should we do variable selection first?
# When should we consider second order terms and interactions?


pX_Z <- ifelse(data_midweek$midweek == 0, 1 - predict(XZmodel, type = "response"),
               predict(XZmodel, type = "response"))


data_midweek$w <- 1/pX_Z


# We now try to estimate the ACE using these weights 


