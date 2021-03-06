# This code has the objective of replicating the analysis made by 
# Krumer and Lechner to the EPL in order to study the causal effect 
# of playing on midweek 


###### ------------ 01. LIBRARIES AND DATA ----------------------------

# The data preparation has already been done

rm(list = ls())
{
  if(!require("glmnet")){install.packages("glmnet")}
  if(!require("stargazer")){install.packages("stargazer")}
  if(!require("knitr")){install.packages("knitr")}
  if(!require("CausalGAM")){install.packages("CausalGAM")}
  
  library(lubridate)
  library(glmnet)
  library(stargazer)
  library(knitr)
  library(CausalGAM)
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
  arrange(desc(percentage_mw)) %>% 
  mutate(bin = c(rep(1, 5), ))


# Midweek assignation does not seem to be randomly assigned after all 



###### ------------ 03. REGRESSION MODELS ----------------------------
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


# Check from all the midweek teams, how many are home and how many away
# Objective: are they comparable?

# We are going to define the set Z as acn, before_national_comps, after_national_comps, 
# ln_capacity, distance and Holidays in order to estimate P(X|Z) and then apply IPW


# Number of games during acn and intl tournaments!!!
XZmodel <- glm(as.factor(midweek) ~ as.factor(acn) + as.factor(before_national_comps) + 
                 as.factor(after_national_comps) + 
                 ln_capacity + distance + as.factor(Holidays) + as.factor(season) , 
               family = "binomial",
               data = data_midweek)

summary(XZmodel)


# What to do with all the non-significant variables?
# Should we do variable selection first?
# When should we consider second order terms and interactions?


pX_Z <- ifelse(data_midweek$midweek == 0, 1 - predict(XZmodel, type = "response"),
               predict(XZmodel, type = "response"))


data_midweek$w <- 1/pX_Z

n_distinct(as.factor(data_midweek$w))

# We now try to estimate the ACE using IPW where 
## Z = covariates used in the previous model 
## X = 1 if the team played during midweek 
## Yobs = 1 if the home team won 


x <- data_midweek$midweek  
yobs <- case_when(
  data_midweek$fthg > data_midweek$ftag ~  3, 
  data_midweek$fthg < data_midweek$ftag ~ 0,
  TRUE ~ 1)

# Regular ACE_IPW
mean(x*yobs/pX_Z  - (1-x)*yobs/(1-pX_Z))

# Modified IPW estimator

sum(x*yobs/pX_Z)/sum(x/pX_Z) - sum((1-x)*yobs/(1-pX_Z))/sum((1-x)/(1-pX_Z))


reg <- glm(yobs ~ x, weights = 1/pX_Z)
summary(reg)

# Let's take into account the strength of the teams

data_og <- bind_rows(
  lapply(list.files('Datasets',
                    full.names = TRUE,
                    pattern = '\\d{4}\\-\\d{2}\\.csv'),
         read_csv,
         col_types = cols())
) %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date(date, '%d/%m/%y'))


win_probabilities <- data_og %>% 
  select(div:bsa) %>% 
  pivot_longer(b365h:bsa) %>% 
  mutate(pred_result = str_sub(name, -1),
         bookmaker = str_sub(name, 1, -2),
         value = 1/value) %>% 
  select(-name) %>% 
  pivot_wider(names_from = pred_result,
              values_from = value) %>% 
  filter(!is.na(h)) %>% 
  mutate(sum_probs = h + d + a,
         h = h/sum_probs,
         d = d/sum_probs, 
         a = a/sum_probs) %>% 
  group_by(date, home_team, away_team) %>% 
  summarize(h = mean(h),
            a = mean(a),
            d = mean(d)) %>% 
  ungroup() %>% 
  mutate(expected_points_ht = 3*h + d, 
         expected_points_at = 3*a + d)


data_midweek <- data_midweek %>% 
  left_join(
    win_probabilities, 
    by = c("date", "home_team", "away_team")
  )

# We now do the rolling means of the expected points for all teams for their last 4 previous matches

dm2 <- data_midweek %>% 
  filter(!is.na(expected_points_ht)) %>% 
  pivot_longer(home_team:away_team) %>% 
  rename(team = value) %>% 
  mutate(exp_points = if_else(name == "away_team", expected_points_at, 
                              expected_points_ht)) %>% 
  group_by(season, team) %>% 
  arrange(date) %>% 
  mutate(cumulative_exp_points = cumsum(exp_points), 
         rm_exp_points = (cumulative_exp_points - lag(cumulative_exp_points, 4))/4) %>% 
  ungroup() %>% 
  pivot_wider(names_from = name, 
              values_from = team) %>% 
  select(date, season, home_team, away_team, rm_exp_points)


data_midweek2 <- data_midweek %>% 
  left_join(dm2 %>% select(-away_team) %>% 
              filter(!is.na(home_team)), 
            by = c("date", "season", "home_team")) %>% 
  rename(rm_exp_points_ht = rm_exp_points) %>% 
  left_join(dm2 %>% select(-home_team) %>% 
              filter(!is.na(away_team)), 
            by = c("date", "season", "away_team")) %>% 
  rename(rm_exp_points_at = rm_exp_points) 


data_midweek2 <- data_midweek2 %>% filter(!is.na(rm_exp_points_at), !is.na(rm_exp_points_ht)) %>% 
  mutate(
    diff_rm_exp_points = rm_exp_points_ht - rm_exp_points_at # This is our proxy for team strength 
  )

# Let's see if the number of matches make sense

data_midweek2 %>% 
  group_by(season) %>% 
  tally() 

# We make a regression adjusting for confounding 
### Consider X = treatment (midweek games),
###          Y = game won by home team 
###          p(Z) = propensity score
###          S = team strength

# If we Assume E[Y|X, p(Z), S] = beta0 + beta1x + beta2p(Z) + beta3S
# then beta1 is our ACE
data_midweek2 <- data_midweek2 %>% 
  mutate(y = if_else(fthg > ftag, 1, 0),
         y2 = case_when(
           fthg > ftag ~ 3, 
           fthg == ftag ~ 1,
           TRUE ~ 0
         ),
         pz = 1/w)

reg_adjusted <- lm(y ~ midweek + pz + diff_rm_exp_points, 
                   data = data_midweek2)
summary(reg_adjusted)


# So, apparently, playing midweek reduces the average number of games 
# you win by 0.03

# on points: 

reg_adjusted2 <- lm(y2 ~ midweek + pz + diff_rm_exp_points, 
                   data = data_midweek2)
summary(reg_adjusted2)

reg_adjusted2 <- lm(y2 ~ midweek + diff_rm_exp_points, 
                    data = data_midweek2, weights = w)
summary(reg_adjusted2)



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
  group_by(name, midweek) %>% 
  summarise(mean_points = mean(points)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = midweek, values_from = mean_points) %>% 
  rename('not_midweek' = `0`, 'midweek' = `1`) %>% 
  kable(format='latex')
 

%>% 
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

midweek_frequency <- data_midweek %>% 
  select(season, date,home_team, away_team, midweek) %>% 
  pivot_longer(home_team:away_team) %>% 
  group_by(value) %>% 
  summarise(n_midweek = sum(midweek),
            n = n()) %>% 
  ungroup() %>% 
  mutate(midweek_perc = n_midweek / n) %>% 
  arrange(desc(midweek_perc))

midweek_frequency <- midweek_frequency %>% 
  mutate(value = factor(value, levels = midweek_frequency$value))

ggplot(midweek_frequency) + 
  geom_col(aes(x = value, y = midweek_perc, fill = n)) + 
  geom_hline(yintercept = sum(midweek_frequency$n_midweek) / sum(midweek_frequency$n), 
             linetype = 'dashed', color = 'red') + 
  scale_y_continuous(labels = scales::percent) + 
  theme_bw() + 
  labs(y = 'Midweek Matches\nPercentage',
       fill = 'Total Matches\nPlayed',
       title = 'Assignment of Midweek Matches Varies by Club') + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank())

ggsave('/Users/hlukas/Google Drive/Grad School/2021-2022/Spring/CSSS 566/Project/Graphs/midweek_match_percentage.png',
       width = 6,
       height = 4)

# ---------------------------- MODEL PROPOSAL ------------------------------------

proportions_month <- data_midweek %>% 
  filter(! season %in% c("2007-08", "2008-09")) %>% 
  group_by(month(date, label = T), season) %>% 
  summarize(n = n(), 
            mw = sum(midweek),
            p = round(mw*100/n, 2)) %>% 
  select(-n, -mw) %>% 
  spread(key = `month(date, label = T)`, value = p)

names(proportions_month)[1] <- "Season"

kable(proportions_month, "latex", 
      caption = "Proportion of EPL games that take place during midweek per month and season",
      position = "H",
      digits = 4,
      booktabs = T)


data_midweek %>% 
  filter(! season %in% c("2007-08", "2008-09"),
         midweek == 1) %>% 
  group_by(month(date, label = T), season) %>% 
  summarize(n = n()) %>% 
  spread(key = `month(date, label = T)`, value = n)


# Outcomes of home teams as 


### Idea : 
# 1st of all, Bundesliga teams play fewer matches (34) than the english squads (38)
# We adjusted for international tournaments, but most of them happen during the summer, 
# so they affect the beginning of the season at most. We could consider the African Nations
# Cup can affect, but  we would see its effect only every 4 years during January.
# We have already seen that most of the midweek games take place during December - Febdruary in 
# EVERY season. Why can this be? Well, the 1st division english teams have to play de EPL and two cups:
# the FA cup and the EPL (Carabo) cup. The first one of these has most of its game (for 1st division teams)
# from January to February (https://en.wikipedia.org/wiki/FA_Cup#Qualification_for_subsequent_competitions) and 
# the second one has its last stages during December - January (https://en.wikipedia.org/wiki/2021%E2%80%9322_EFL_Cup#Quarter-finals)
# Moreover, the EPL plays matches every year on Boxing Day (December 26th) and New Year's
# This explains that there is a saturated calendar for EPL teams from December - February, so the league 
# has less available weekends to accommodate games (either because teams are playing other competitions or because
# they just played a match), an hence it makes use of midweeks to plan games.

# In that sense, let's make a regression for propensity score only on december - feb and on distance between cities
# (if I have to travel long distances, I would expect the EPL to not make me play midweek)

data_og <- bind_rows(
  lapply(list.files('Datasets',
                    full.names = TRUE,
                    pattern = '\\d{4}\\-\\d{2}\\.csv'),
         read_csv,
         col_types = cols())
) %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date(date, '%d/%m/%y'))


win_probabilities <- data_og %>% 
  select(div:bsa) %>% 
  pivot_longer(b365h:bsa) %>% 
  mutate(pred_result = str_sub(name, -1),
         bookmaker = str_sub(name, 1, -2),
         value = 1/value) %>% 
  select(-name) %>% 
  pivot_wider(names_from = pred_result,
              values_from = value) %>% 
  filter(!is.na(h)) %>% 
  mutate(sum_probs = h + d + a,
         h = h/sum_probs,
         d = d/sum_probs, 
         a = a/sum_probs) %>% 
  group_by(date, home_team, away_team) %>% 
  summarize(h = mean(h),
            a = mean(a),
            d = mean(d)) %>% 
  ungroup() %>% 
  mutate(expected_points_ht = 3*h+d, 
         expected_points_at = 3*a+d)


data_midweek <- data_midweek %>% 
  left_join(
    win_probabilities, 
    by = c("date", "home_team", "away_team")
  )

# We now do the rolling means of the expected points for all teams for their last 4 previous matches

dm2 <- data_midweek %>% 
  filter(!is.na(expected_points_ht)) %>% 
  pivot_longer(home_team:away_team) %>% 
  rename(team = value) %>% 
  mutate(exp_points = if_else(name == "away_team", expected_points_at, 
                              expected_points_ht)) %>% 
  group_by(season, team) %>% 
  arrange(date) %>% 
  mutate(cumulative_exp_points = cumsum(exp_points), 
         rm_exp_points = (cumulative_exp_points - lag(cumulative_exp_points, 4))/4) %>% 
  ungroup() %>% 
  pivot_wider(names_from = name, 
              values_from = team) %>% 
  select(date, season, home_team, away_team, rm_exp_points)


data_midweek <- data_midweek %>% 
  left_join(dm2 %>% select(-away_team) %>% 
              filter(!is.na(home_team)), 
            by = c("date", "season", "home_team")) %>% 
  rename(rm_exp_points_ht = rm_exp_points) %>% 
  left_join(dm2 %>% select(-home_team) %>% 
              filter(!is.na(away_team)), 
            by = c("date", "season", "away_team")) %>% 
  rename(rm_exp_points_at = rm_exp_points) %>% 
  mutate(dec_mar = if_else(month(date) %in% c(1, 2,3,  12), 1, 0))

rm(data_og)

data_midweek <- data_midweek %>% 
  filter(!is.na(rm_exp_points_at), !is.na(rm_exp_points_ht)) %>% 
  mutate(
    diff_rm_exp_points = rm_exp_points_ht - rm_exp_points_at # This is our proxy for team strength 
  )

rm(dm2)

mean(data_midweek$dec_mar)

modZ <- glm(midweek ~ as.factor(dec_mar) + distance, 
          family = "binomial",
          data = data_midweek)
summary(modZ)

# These results make sense: having a game between december and february increases the odds
# of playing a midweek game in 
exp(coef(modZ)[2])
# While increasing 1 km of distance between cities decreases the odds of playing a midweek game 
#in
exp(coef(modZ)[1])

pZ <- ifelse(data_midweek$midweek == 0, 1 - predict(modZ, type = "response"),
               predict(modZ, type = "response"))

# Are these probabilities well calibrated?

stadiums <- read_csv("PL - STADIUMS.csv") %>% 
  clean_names()

values <- read_csv("PL - VALUE.csv") %>% 
  clean_names() %>% 
  left_join(stadiums %>% 
              select(team, alt_name),
            by = c("club" = "alt_name"))

val_data <- data_og %>% 
  mutate(dec_mar = if_else(month(date) %in% c(1, 2,3,  12), 1, 0),
         wday = wday(date),
         midweek = if_else(!wday %in% c(1, 2, 7), 1, 0)) %>% 
  filter(year(date)<=2008) %>% 
  left_join(stadiums %>% 
              select(team, capacity, coords),
            by = c("home_team" = "team")) %>% 
  separate(coords, into = c("ht_lat", "ht_long"), sep = ",") %>% 
  left_join(stadiums %>% 
              select(team,coords),
            by = c("away_team" = "team")) %>% 
  separate(coords, into = c("at_lat", "at_long"), sep = ",") %>% 
  mutate(ht_lat = as.numeric(ht_lat),
         ht_long = as.numeric(ht_long),
         at_lat = as.numeric(at_lat),
         at_long = as.numeric(at_long))

distance <- numeric(nrow(val_data))
for(i in 1:nrow(val_data)){
  distance[i] <- distm (c(val_data[i, "ht_long"][[1]], val_data[i, "ht_lat"][[1]]),
                        c(val_data[i, "at_long"][[1]], val_data[i, "at_lat"][[1]]), 
                        fun = distHaversine)
}


cbind(val_data, distance) %>% 
  select(midweek, dec_mar, distance) %>% 
  na.omit() %>% 
  mutate(pred = predict(modZ, newdata = ., type = "response"),
         pred_bin = round(pred * 10) / 10) %>% 
  select(pred_bin,pred, midweek) %>% 
  group_by(pred_bin) %>% 
  summarise(true_outcome = mean(midweek),
            n = n()) %>% 
  ungroup() %>% 
  ggplot(.) + 
    geom_point(aes(x=pred_bin, y = true_outcome, size = n), color = 'dodgerblue3') +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'red') + 
    scale_x_continuous(expand = c(0,0), limits = c(0,.4)) + 
    scale_y_continuous(expand = c(0,0), limits = c(0,.4)) + 
    labs(x = 'Predicted Midweek Assignment Probability (Binned)',
         y = 'Actual Midweek Probability',
         title = 'Logit Regression is Somewhat Well-Calibrated',
         size = 'N') +
    theme_bw()

ggsave('/Users/hlukas/Google Drive/Grad School/2021-2022/Spring/CSSS 566/Project/Graphs/mw_prob_logit_cal.png',
       width = 6,
       height = 4)


pZ_cal <- case_when(
  pZ <= 0.05 ~ 0.05,
  pZ <= 0.10 ~ 0.1,
  pZ <= 0.15 ~ 0.15,
  pZ <= 0.20 ~ 0.20,
  pZ <= 0.25 ~ 0.25,
  pZ <= 0.30 ~ 0.30,
  pZ <= 0.35 ~ 0.35,
  pZ <= 0.40 ~ 0.40,
  pZ <= 0.45 ~ 0.45,
  pZ <= 0.50 ~ 0.50,
  pZ <= 0.55 ~ 0.55,
  pZ <= 0.60 ~ 0.60,
  pZ <= 0.65 ~ 0.65,
  pZ <= 0.70 ~ 0.70,
  pZ <= 0.75 ~ 0.75,
  pZ <= 0.80 ~ 0.80,
  pZ <= 0.85 ~ 0.85,
  pZ <= 0.90 ~ 0.90,
  pZ <= 0.95 ~ 0.95,
  TRUE ~ 1
)

  
# Let's compute the basic ATE using IPW as well as the stable IPW and a weighted regression
# adjusting for team strength (diff_rm_exp_points)

data_midweek <- data_midweek %>% 
  mutate(ht_outcome = if_else(fthg > ftag, 1, 0),
         ht_points = case_when(
           fthg > ftag ~ 3, 
           fthg < ftag ~ - 3, # We want point differential (as in Lopez' code)
           TRUE ~ 0
         ))

pX1 <- mean(data_midweek$midweek) 
pX0 <- 1 - pX1

x <- data_midweek$midweek

stable.weights <- x*pX1/pZ + (1-x)*pX0/(1-pZ)
mean(stable.weights) # This might be a problem

# Outcome: home team wins 
y_winner <- data_midweek$ht_outcome
mean(x*y_winner/pZ - (1-x)*y_winner/(1-pZ)) # IPW estimator
sum(x*y_winner/pZ)/sum(x/pZ) - sum((1-x)*y_winner/(1-pZ))/sum((1-x)/(1-pZ)) # modified IPW estimator
reg.y_winner <- glm(as.factor(ht_outcome) ~ midweek + diff_rm_exp_points, # Regression estimator
                    data = data_midweek, 
                    family = "binomial",
                    weights = 1/pZ)
summary(reg.y_winner) ## ! negative coefficient on midweek but nos significative
m <- coef(reg.y_winner)[2]
exp(m)


#Outcome: points
y_points <- data_midweek$ht_points
mean(y_points[x == 1]) - mean(y_points[x == 0]) # Naive estimator
mean(x*y_points/pZ - (1-x)*y_points/(1-pZ)) # IPW estimator
sum(x*y_points/pZ)/sum(x/pZ) - sum((1-x)*y_points/(1-pZ))/sum((1-x)/(1-pZ)) # modified IPW estimator
reg.y_points <- glm(ht_points ~ midweek + diff_rm_exp_points, # Regression estimator
                    data = data_midweek, 
                    weights = 1/pZ)
summary(reg.y_points) ## ! negative coefficient on midweek but nos significative
mcoef <- coef(reg.y_points)[2]
exp(mcoef)

### Using double robust estimators

mod_cond_exp <- lm(ht_points ~ midweek + dec_mar  + diff_rm_exp_points, 
                   data = data_midweek, 
                   weights = 1/pZ)
summary(mod_cond_exp)
coefs <- coef(mod_cond_exp)

dm0 <- data.frame(
  int = 1,
  x = 0,
  data_midweek$dec_mar, 
  data_midweek$diff_rm_exp_points
)

dm1 <- data.frame(
  int = 1,
  x = 1,
  data_midweek$dec_mar, 
  data_midweek$diff_rm_exp_points
)

m0 <- t(coefs) %*% t(as.matrix(dm0))
m1 <- t(coefs) %*% t(as.matrix(dm1))

E_Y1 <- mean(((x*y_points) - (x - pZ)*m0)/pZ)
E_Y0 <- mean((((1-x)*y_points) - (x - pZ)*m1)/(1-pZ))

E_Y1 - E_Y0

### bootstrapping 

nsim <-1e4

data_midweek$w = 1/pZ

naive_est <- numeric(nsim)
IPW_est <- numeric(nsim)
reg_est <- numeric(nsim)
dr_est <- numeric(nsim)

for(i in 1:nsim){
  
  # We resample the data frame
  
  indeces <- sample(1:nrow(data_midweek), size = nrow(data_midweek), 
                    replace = T)
  
  dat <- data_midweek[indeces, ]
  
  x <- dat$midweek
  y <- dat$ht_points
  w <- dat$w
  
  # Naive estimator
  naive_est[i] <- mean(y[x == 1]) - mean(y[x == 0]) 
  # IPW estimator
  IPW_est[i] <- mean(x*y*w - (1-x)*y*(1-w)) 
  
  mod <- lm(ht_points ~ midweek + dec_mar  + diff_rm_exp_points, 
            data = dat, 
            weights = w)
  # regression estimator
  reg_est[i] <- coef(mod)[[2]]
  
  # double robust est
  
  coefs <- coef(mod)
  
  dm0 <- data.frame(
    int = 1,
    x = 0,
    dat$dec_mar, 
    dat$diff_rm_exp_points
  )
  
  dm1 <- data.frame(
    int = 1,
    x = 1,
    dat$dec_mar, 
    dat$diff_rm_exp_points
  )
  
  m0 <- t(coefs) %*% t(as.matrix(dm0))
  m1 <- t(coefs) %*% t(as.matrix(dm1))
  
  E_Y1 <- mean(((x*y) - (x - 1/w)*m0)*w)
  E_Y0 <- mean((((1-x)*y) - (x - 1/w)*m1)/(1-1/w))
  
  dr_est[i] <- E_Y1 - E_Y0
  
  }

summary(naive_est) # no effect
summary(IPW_est) #small effect
summary(reg_est) # no effect
summary(dr_est) # large negative effect

#Outcome: goals
y_goals <- data_midweek$fthg
mean(x*y_goals/pZ - (1-x)*y_goals/(1-pZ)) # IPW estimator
sum(x*y_goals/pZ)/sum(x/pZ) - sum((1-x)*y_goals/(1-pZ))/sum((1-x)/(1-pZ)) # modified IPW estimator
reg.y_goals <- glm(fthg ~ midweek + diff_rm_exp_points, # Regression estimator
                    data = data_midweek, 
                    weights = 1/pZ)
summary(reg.y_goals) ## !! Mildly significative for goals
m <- coef(reg.y_goals)[2]
exp(m)


# Outcome: yellow cards
y_yc <- data_midweek$hy
mean(x*y_yc/pZ - (1-x)*y_yc/(1-pZ)) # IPW estimator
sum(x*y_yc/pZ)/sum(x/pZ) - sum((1-x)*y_yc/(1-pZ))/sum((1-x)/(1-pZ)) # modified IPW estimator
reg.y_yc <- glm(hy ~ midweek + diff_rm_exp_points, # Regression estimator
                   data = data_midweek, 
                   weights = 1/pZ)
summary(reg.y_yc) ## !! Mildly significative for yc
m <- coef(reg.y_yc)[2]
exp(m)