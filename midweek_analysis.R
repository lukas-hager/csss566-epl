# This code has the objective of replicating the analysis made by 
# Krumer and Lechner to the EPL in order to study the causal effect 
# of playing on midweek 


###### ------------ 01. LIBRARIES AND DATA ----------------------------

# The data preparation has already been done

rm(list = ls())
{
  library(lubridate)
  library(tidyverse)
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


# We want to look at the point differential by home vs away

data_midweek %>%
  select(date, season, points_home, points_away) %>% 
  pivot_longer(points_home:points_away) %>% 
  mutate(team = str_extract(name, '(home)|(away)')) %>% 
  group_by(team, season) %>% 
  summarise(mean_points = mean(value)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = season,
              names_from = team, 
              values_from = mean_points) %>% 
  arrange(season) %>% 
  mutate(home_team_adv = home - away)

# Point differential midweek by season

data_midweek %>%
  select(date, season, midweek, points_home, points_away) %>% 
  pivot_longer(points_home:points_away) %>% 
  mutate(team = str_extract(name, '(home)|(away)')) %>% 
  group_by(team, season, midweek) %>% 
  summarise(mean_points = mean(value)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(season,midweek),
              names_from = team, 
              values_from = mean_points) %>% 
  arrange(season) %>% 
  mutate(home_team_adv = home - away,
         midweek = ifelse(midweek == 1, 'is_midweek', 'is_not_midweek')) %>% 
  select(-home, -away) %>% 
  pivot_wider(id_cols = season,
              names_from = midweek,
              values_from = home_team_adv) %>% 
  mutate(midweek_change = is_midweek - is_not_midweek)

# Point differential by team by midweek

points_by_midweek <- data_midweek %>%
  mutate(id_val = row_number()) %>% 
  select(id_val, home_team, away_team) %>% 
  pivot_longer(home_team:away_team) %>% 
  mutate(name = str_extract(name, '(home)|(away)')) %>% 
  rename(team = value) %>% 
  left_join(data_midweek %>% 
              mutate(id_val = row_number()) %>% 
              select(id_val, midweek, points_home, points_away) %>% 
              pivot_longer(points_home:points_away) %>% 
              mutate(name = str_extract(name, '(home)|(away)')) %>% 
              rename(points = value),
            by = c('id_val', 'name')) %>% 
  group_by(name, team, midweek) %>% 
  summarise(mean_points = mean(points)) %>% 
  ungroup() %>% 
  mutate(midweek = ifelse(midweek == 1, 'is_midweek', 'is_not_midweek')) %>% 
  pivot_wider(id_cols = c(team,name), names_from = midweek, values_from = mean_points) %>% 
  filter(name == 'home') %>% 
  mutate(diff = is_not_midweek - is_midweek) %>% 
  arrange(desc(diff)) %>% 
  left_join(data_midweek %>% 
              group_by(home_team) %>% 
              summarise(n_midweek = sum(midweek)) %>% 
              ungroup() %>% 
              rename(team = home_team),
            by = 'team') 

points_by_midweek <- points_by_midweek %>% 
  mutate(team = factor(team, levels = points_by_midweek$team))

ggplot(data = points_by_midweek) + 
  geom_col(aes(x = team, y = diff, fill =n_midweek)) + 
  theme(axis.text.x = element_text(angle = 90))
  

pivot_longer(home_team:points_away) %>% 
  mutate(team = str_extract(name, '(home)|(away)')) %>% 
  group_by(team, season, midweek) %>% 
  summarise(mean_points = mean(value)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(season,midweek),
              names_from = team, 
              values_from = mean_points) %>% 
  arrange(season) %>% 
  mutate(home_team_adv = home - away,
         midweek = ifelse(midweek == 1, 'is_midweek', 'is_not_midweek')) %>% 
  select(-home, -away) %>% 
  pivot_wider(id_cols = season,
              names_from = midweek,
              values_from = home_team_adv) %>% 
  mutate(midweek_change = is_midweek - is_not_midweek)

# Percentage of matches played midweek by team

data_midweek %>% 
  select(season, date,home_team, away_team, midweek) %>% 
  pivot_longer(home_team:away_team) %>% 
  mutate(name = str_extract(name, '(home)|(away)')) %>% 
  group_by(name, value) %>% 
  summarise(n_midweek = sum(midweek),
            n_weekend = sum(1-midweek)) %>% 
  ungroup() %>% 
  group_by(value) %>% 
  mutate(midweek_perc = n_midweek / (sum(n_midweek) + sum(n_weekend))) %>% 
  select(-n_midweek, -n_weekend) %>% 
  pivot_wider(id_cols = value, names_from = name, values_from = midweek_perc) %>% 
  rename(away_midweek_match_perc = away,
         home_midweek_match_perc = home) %>% 
  mutate(total_midweek_match_perc = home_midweek_match_perc + away_midweek_match_perc) %>% 
  arrange(desc(total_midweek_match_perc))
