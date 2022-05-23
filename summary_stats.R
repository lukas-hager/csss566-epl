

rm(list = ls())
if(!require("geosphere")){install.packages("geosphere")}
library(data.table)
library(rmarkdown)
library(janitor)
library(tidyverse)
library(lubridate)
library(geosphere)

final_dataset <- read_csv("Datasets/final_dataset.csv")%>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date(date, '%d/%m/%y'))


data_og <- bind_rows(
  lapply(list.files('Datasets',
                    full.names = TRUE,
                    pattern = '\\d{4}\\-\\d{2}\\.csv'),
         read_csv,
         col_types = cols())
) %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date(date, '%d/%m/%y'))


stadiums <- read_csv("PL - STADIUMS.csv") %>% 
  clean_names()

values <- read_csv("PL - VALUE.csv") %>% 
  clean_names() %>% 
  left_join(stadiums %>% 
              select(team, alt_name),
            by = c("club" = "alt_name"))


# Per year

final_dataset <- final_dataset %>% 
  mutate(
    season = case_when(
    date <= dmy(19052001) ~ "2000-01",
    date <= dmy(11052002) ~ "2001-02",
    date <= dmy(11052003) ~ "2002-03",
    date <= dmy(15052004) ~ "2003-04",
    date <= dmy(15052005) ~ "2004-05",
    date <= dmy(07052006) ~ "2005-06",
    date <= dmy(13052007) ~ "2006-07",
    date <= dmy(11052008) ~ "2007-08",
    date <= dmy(24052009) ~ "2008-09",
    date <= dmy(09052010) ~ "2009-10",
    date <= dmy(22052011) ~ "2010-11",
    date <= dmy(13052012) ~ "2011-12",
    date <= dmy(19052013) ~ "2012-13",
    date <= dmy(11052014) ~ "2013-14",
    date <= dmy(24052015) ~ "2014-15",
    date <= dmy(17052016) ~ "2015-16",
    date <= dmy(21052017) ~ "2016-17",
    date <= dmy(13052018) ~ "2017-18",
    date <= dmy(12052019) ~ "2018-19",
    TRUE ~ "2019-20"
  ))


final_dataset %>% 
  group_by(season) %>% 
  tally() %>% 
  arrange(season)


data_og <- data_og %>% 
  mutate(
    season = case_when(
      date <= dmy(19052001) ~ "2000-01",
      date <= dmy(11052002) ~ "2001-02",
      date <= dmy(11052003) ~ "2002-03",
      date <= dmy(15052004) ~ "2003-04",
      date <= dmy(15052005) ~ "2004-05",
      date <= dmy(07052006) ~ "2005-06",
      date <= dmy(13052007) ~ "2006-07",
      date <= dmy(11052008) ~ "2007-08",
      date <= dmy(24052009) ~ "2008-09",
      date <= dmy(09052010) ~ "2009-10",
      date <= dmy(22052011) ~ "2010-11",
      date <= dmy(13052012) ~ "2011-12",
      date <= dmy(19052013) ~ "2012-13",
      date <= dmy(11052014) ~ "2013-14",
      date <= dmy(24052015) ~ "2014-15",
      date <= dmy(17052016) ~ "2015-16",
      date <= dmy(21052017) ~ "2016-17",
      date <= dmy(13052018) ~ "2017-18",
      date <= dmy(12052019) ~ "2018-19",
      TRUE ~ "2019-20"
    ))

data_og %>% 
  group_by(season) %>% 
  tally() %>% 
  arrange(season)

# we should have 7220 games
19*380

# We have this number of extra rows
nrow(data_og) - nrow(final_dataset)

# there is something weird in the data from the 
# two last seasons (2018-19 and 2019 - 20)


### ------     Question 01: halftime advantage ----------


# We compute thee Percentage of Games Won by the Home Team by 
# the Score Difference at halftime 

data_og %>% 
  filter(date <= dmy(13052018)) %>% 
  mutate(ht_diff = hthg - htag) %>% 
  pull(ht_diff) %>% 
  summary()

data_og %>% 
  filter(date <= dmy(13052018)) %>% 
  mutate(ht_diff = hthg - htag) %>% 
  ggplot(aes(x = ht_diff)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs("Home team score differential at halftime")


data_og %>% 
  filter(date <= dmy(13052018)) %>% 
  mutate(ht_diff = hthg - htag) %>% 
  ggplot(aes(x = ht_diff)) + 
  geom_histogram() + 
  theme_bw() + 
  labs("Home team score differential at halftime")


data_og %>% 
  filter(date <= dmy(13052018)) %>% 
  mutate(ht_diff = hthg - htag,
         result = case_when(
           fthg > ftag ~ "H",
           fthg < ftag ~ "A",
           TRUE ~ "D"
         )) %>% 
  filter(ftr != result) %>% 
  nrow()



data_og %>% 
  filter(date <= dmy(13052018)) %>% 
  mutate(ht_diff = hthg - htag) %>% 
  group_by(ht_diff, ftr) %>% tally() %>% 
  ungroup() %>% 
  spread(key = ftr, value = n, fill = 0) %>% 
  mutate(total = A + D + H,
         per_won = round(H*100/total, 6)) %>% 
  ggplot(aes(x = ht_diff, y = per_won)) + 
  geom_point() + 
  theme_bw() + 
  labs(x = "Home team score differential at halftime", 
       y = "% of games won")



### ------     Question 02: midweek games effect ----------

acn_2008 <- interval(dmy(01012008), dmy(29022008))
acn_2010 <- interval(dmy(01012010), dmy(31012010))
acn_2012 <- interval(dmy(01012012), dmy(28022012))
acn_2013 <- interval(dmy(01012013), dmy(28022013))
acn_2015 <- interval(dmy(01012015), dmy(28022015))
acn_2017 <- interval(dmy(01012017), dmy(28022017))

# We will prepare the data for this question
seasons <- c("2007-08", "2008-09", "2009-10", "2010-11", "2011-12", 
             "2012-13" ,"2013-14", "2014-15", "2015-16", "2016-17")

data_midweek <- data_og %>% 
  filter(season %in% seasons) %>% 
  mutate(wday = wday(date),
         midweek = if_else(!wday %in% c(1, 2, 7), 1, 0),
         points_home = case_when(
           ftr == "H" ~ 3,
           ftr == "D" ~ 1, 
           TRUE ~ 0
         ),
         points_away = case_when(
           ftr == "H" ~ 0,
           ftr == "D" ~ 1, 
           TRUE ~ 3
         ),
         # Africa Cup of Nations
         acn = case_when(
           date %within% acn_2008 ~ 1,
           date %within% acn_2010 ~ 1,
           date %within% acn_2012 ~ 1,
           date %within% acn_2013 ~ 1,
           date %within% acn_2015 ~ 1,
           date %within% acn_2017 ~ 1,
           TRUE ~ 0
         ),
         # Two months before WC, European Championship or Copa America
         before_national_comps = case_when(
           # World cups
           date %within% interval(dmy(11042010), dmy(11062010)) ~ 1,
           date %within% interval(dmy(12042014), dmy(12062014)) ~ 1,
           # European Championship
           date %within% interval(dmy(07042008), dmy(07062008)) ~ 1,
           date %within% interval(dmy(08042012), dmy(08062012)) ~ 1,
           date %within% interval(dmy(10042016), dmy(10062016)) ~ 1,
           # Copa America
           date %within% interval(dmy(26042007), dmy(26062007)) ~ 1,
           date %within% interval(dmy(01052011), dmy(01072011)) ~ 1,
           date %within% interval(dmy(11042015), dmy(11062015)) ~ 1,
           date %within% interval(dmy(03042016), dmy(03062016)) ~ 1,
           TRUE ~ 0
         ),
         after_national_comps = case_when(
           # World cups
           date %within% interval(dmy(11072010), dmy(11092010)) ~ 1,
           date %within% interval(dmy(13072014), dmy(13092014)) ~ 1,
           # European Championship
           date %within% interval(dmy(29062008), dmy(29082008)) ~ 1,
           date %within% interval(dmy(01072012), dmy(01092012)) ~ 1,
           date %within% interval(dmy(10072016), dmy(10092016)) ~ 1,
           # Copa America
           date %within% interval(dmy(15072007), dmy(15092007)) ~ 1,
           date %within% interval(dmy(24072011), dmy(24092011)) ~ 1,
           date %within% interval(dmy(04072015), dmy(04092015)) ~ 1,
           date %within% interval(dmy(26062016), dmy(26082016)) ~ 1,
           TRUE ~ 0
         )) %>% 
    select(date, season, home_team, away_team, wday, midweek, 
         # Game outcomes
         points_home, points_away, fthg, ftag, hs, as, hst, ast, 
         hf, af, hy, ay, hr, ar, hc, ac,
         # Game characteristics
         acn, before_national_comps, after_national_comps
         ) 

rm(list = ls()[str_detect(ls(), "acn")])


# Now we are going to add the Game characteristic variables

data_midweek <- data_midweek %>% 
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

distance <- numeric(nrow(data_midweek))
for(i in 1:nrow(data_midweek)){
  distance[i] <- distm (c(data_midweek[i, "ht_long"][[1]], data_midweek[i, "ht_lat"][[1]]),
                        c(data_midweek[i, "at_long"][[1]], data_midweek[i, "at_lat"][[1]]), 
                        fun = distHaversine)
}


data_midweek <- cbind(data_midweek, distance) %>% 
  mutate(ln_capacity = log(capacity),
         season_part = case_when( # Is the game played at the beginning, middle or end of the season?
           month(date) %in% c(8, 9, 10) ~ "beginning",
           month(date) %in% c(11, 12, 1, 2) ~ "middle",
           TRUE ~ "end"
         ))


apply(data_midweek, 2, function(x)sum(is.na(x)))
# We now add the team characteristics variables

# First we are going to look for teams that were just recently promoted

seasons_df <- data.frame(
  season = seasons, 
  previous_season = c(NA, seasons[1:(length(seasons) - 1)])
)

promotions <- data.frame()

for(i in 1:length(seasons)){
  season_ <- seasons[i]
  if(season_ == seasons_df$season[1]){
    

  promotions <- rbind(promotions, 
                      cbind(rep(season_, 3), c("Sunderland",  "Birmingham", "Derby")))
    
  } else {
    
    prev_season <- seasons_df %>% filter(season == season_) %>% 
      pull(previous_season)
    
    team_season <- data_midweek %>% filter(season == season_) %>% 
      pull(home_team) %>% unique()
    
    team_prev <- data_midweek %>% filter(season == prev_season) %>% 
      pull(home_team) %>% unique()
    
    promotions <- rbind(promotions, 
                        cbind(rep(season_, 3), team_season[!team_season %in% team_prev]))
  }
}

names(promotions) <- c("season", "team")
promotions <- promotions %>% 
  mutate(promoted = 1)

data_midweek <- data_midweek %>% 
  left_join(promotions, 
            by = c("home_team" = "team",
                   "season" = "season")) %>% 
  rename(ht_promoted = promoted) %>% 
  left_join(promotions, 
            by = c("away_team" = "team",
                   "season" = "season")) %>% 
  rename(at_promoted = promoted)

data_midweek <- data_midweek %>% 
  replace_na(list("at_promoted" = 0, 
                  "ht_promoted" = 0))



# Then we obtain the value of the teams 

data_midweek <- data_midweek %>% 
  left_join(values %>% 
              select(team, season, mv), 
            by = c("home_team" = "team",
                   "season" = "season")) %>% 
  rename(ht_mv = mv) %>% 
  left_join(values %>% 
              select(team, season, mv), 
            by = c("away_team" = "team",
                   "season" = "season")) %>% 
  rename(at_mv = mv)

apply(data_midweek, 2, function(x)sum(is.na(x)))


# Finally, we create an indicator variable for short weeks (2 or more matches in the same week)

data_midweek <- data_midweek %>% 
  mutate(week = str_c(week(date), "-", year(date))) %>% 
  group_by(home_team, season, week) %>% 
  mutate(ht_games_week = n()) %>% 
  ungroup() %>% 
  group_by(away_team, season, week) %>% 
  mutate(at_games_week = n()) %>% 
  ungroup() %>% 
  mutate(ht_short_week = if_else(ht_games_week > 1, 1, 0),
         at_short_week = if_else(at_games_week > 1, 1, 0))


write_csv(data_midweek, "data_midweek.csv")


# Weekends: Saturdays (7), Sundays (1) and Mondays (2)
data_midweek %>% 
  group_by(midweek) %>% tally()

data_midweek %>% 
  group_by(midweek) %>% 
  summarize(APH = mean(points_home),
            APA = mean(points_away))

# We will use the transfermarkt data to complete the analysis
