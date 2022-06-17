
library(glmnet)
library(MASS)
library(caret)
library(ROCR)
library(tidyverse)

dat <- read_csv("dw_fit.csv") %>% 
  dplyr::select(-w, # weights
         -h, -a, -d) %>%  #estimated probabilities (from odds)
  mutate(
    ht_mv = as.numeric(str_sub(ht_mv, 2, -2)),
    at_mv = as.numeric(str_sub(at_mv, 2, -2))
  ) %>% 
  na.omit() #should we do this or simply remove the rolling mean variables (except
            # for the ones about the odds)?

dat %>% 
  group_by(season) %>% 
  tally()


#### --------------1. Fitting option A) -----------------

# We train with the data from seasons 2009-10 to 2012-13 (4 seasons)
# We test/estimate with the data from seasons 2014-15 to 2016-17 (3 seasons)
# Reason for doing this: not trying to make predictions using future data

# For the propensity score, it makes sense to only use variables that are known before 
# the season (so we cannont use variables as goals, odds, etc.)
train.opA <- dat %>% 
  filter(!season %in% c("2014-15", "2015-16", "2016-17")) %>% 
  dplyr::select(midweek, acn, before_national_comps, 
                after_national_comps, capacity, distance, dec_mar) 
test.opA <- dat %>% 
  filter(season %in% c("2014-15", "2015-16", "2016-17")) %>% 
  dplyr::select(midweek, acn, before_national_comps, 
                after_national_comps, capacity, distance, dec_mar) 

nrow(dat) - nrow(train.opA) - nrow(test.opA) #should be zero

# For the propensity score we will use a gradient boosting algorithm 


# We perform a 10-fold CV with the selected parameters to tune the 
# gradient boosting model 

train.gbm <- train.opA %>% 
  dplyr::select(before_national_comps, distance, dec_mar,
                midweek)

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

model.gbm <- train(as.factor(midweek) ~ ., data = train.opA, 
                   method = "gbm", 
                   trControl = fitControl,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE)
summary(model.gbm)
# Our most important variables are if the game takes place during december and march
# and the distance between cities

# The parameters that will be used are: d=1, trees = 50, lambda = 0.1
print(model.gbm)


# Let's do the predictions now 
pZ <- predict(model.gbm, newdata = test.opA,
                             type = "prob")[,2]

pX1 <- mean(test.opA$midweek)
pX0 <- 1 - pX1

stable.weights <- test.opA$midweek*pX1/pZ + (1-test.opA$midweek)*pX0/(1-pZ)
mean(stable.weights) # Leeeets goooooo!!



# Are the probabilities well calibrated?

calibration.gbm <- data.frame(
  mw = test.opA$midweek, 
 pZ,
  bin=cut(pZ,
          c(seq(0, 1, by = 0.05)),
          include.lowest=TRUE)) %>% 
  group_by(bin) %>% 
  summarize(
    n = n(), 
    mean_observed = mean(mw)
  ) %>% 
  ungroup() %>% 
  mutate(
    upper_bound = seq(0.1, 0.35, by = 0.05)
  )



calibration.gbm %>% 
  ggplot(aes(x = mean_observed, y = bin)) + 
  geom_point() + 
  geom_line(data = calibration.gbm, 
            aes(x = upper_bound, y = bin, group = 1), color = "red") + 
  theme_bw() + 
  labs(main = "Calibration scatterplot for the boosting xG model",
       y = "Prediction bin",
       x = "Observed relative frecuencies of midweek games")
# Not that bad 

test.opA %>% 
  mutate(pred = predict(model.gbm, newdata = test.opA, type = "prob")[,2],
         pred_bin = round(pred * 10) / 10) %>% 
  dplyr::select(pred_bin,pred, midweek) %>% 
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
       title = 'Boosted Linear Regression is Well-Calibrated',
       size = 'N') +
  theme_bw()

ggsave('/Users/hlukas/Google Drive/Grad School/2021-2022/Spring/CSSS 566/Project/Graphs/mw_prob_boost_cal.png',
       width = 6,
       height = 4)

# We now train the model for the points differential (multinomial regression)
train.opA <- dat %>% 
  filter(!season %in% c("2014-15", "2015-16", "2016-17")) %>% 
  mutate(ht_points = as.factor(ht_points)) %>% 
  dplyr::select(-season, -wday, -home_team, -away_team)

test.opA <- dat %>% 
  filter(season %in% c("2014-15", "2015-16", "2016-17")) %>% 
  mutate(ht_points = as.factor(ht_points)) %>% 
  dplyr::select(-season, -wday, -home_team, -away_team)

x_opA <- model.matrix(ht_points~., data = train.opA)
y_opA <- train.opA$ht_points

fit <- cv.glmnet(x_opA, y_opA, family = "multinomial")
plot(fit)

# We make the predictions 
x_opA.test <- model.matrix(ht_points~., data = test.opA)
m <- predict(fit, newx = x_opA.test, 
             s = "lambda.min", type = "class")
m <- as.numeric(m)

test.opA$m <- m


# We now perform boostrap estimation on the test sample

nsim <-1e5

test.opA$pZ = pZ
test.opA$w = 1/pZ

naive_est <- numeric(nsim)
IPW_est <- numeric(nsim)
reg_est <- numeric(nsim)
dr_est <- numeric(nsim)

for(i in 1:nsim){
  
  # We resample the data frame
  
  indeces <- sample(1:nrow(test.opA), size = nrow(test.opA), 
                    replace = T)
  
  dat <- test.opA[indeces, ]
  
  x <- dat$midweek
  y <- dat$ht_points %>% as.character() %>% as.numeric()
  w <- dat$w
  m <- dat$m
  
  # Naive estimator
  naive_est[i] <- mean(y[x == 1]) - mean(y[x == 0]) 
  # IPW estimator
  IPW_est[i] <- mean(x*y*w - (1-x)*y*w/(w-1)) #1-p = (w-1)/w
  
  #mod <- glm(ht_points ~ midweek + dec_mar  + diff_rm_exp_points, 
  #           data = dat, 
  #           weights = w)
  # # regression estimator
  # reg_est[i] <- coef(mod)[[2]]
  
  # double robust est
  
  
  E_Y1 <- mean(((x*y) - (x - 1/w)*m)*w)
  E_Y0 <- mean((((1-x)*y) - (x - 1/w)*m)/(1-1/w))
  
  dr_est[i] <- E_Y1 - E_Y0
  
}

summary(naive_est) # no effect
summary(IPW_est) #no effect
summary(dr_est) # no effect
