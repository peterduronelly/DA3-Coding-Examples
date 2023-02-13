# CHAPTER 18
# version x 2023-02-05
# based on Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# and on modifications by Agoston Reguly (github.com/regulyagoston)
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################

# CLEAR MEMORY & LIBRARIES
rm(list=ls())

# Import libraries ---------------------------------------------------
library(tidyverse)
library(stargazer)
library(Hmisc)
library(timeDate)
library(lubridate)
library(caret)
library(prophet)
library(modelsummary)
library(scales)
library(viridis)

#########################################################################################

# DATA IMPORT, EDA & FEATURES


daily_agg<-read_csv('https://osf.io/jcxmk/download') %>% 
  mutate(date = as.Date(date))

glimpse(daily_agg)

# create time variables
#   - year, quarter, month & day
#   - weekdays using the lubridate library
# an intro to the lubridate package: https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html

daily_agg <- daily_agg %>%
  mutate(
    year = year(date),
    quarter = quarter(date),
    month = factor(month(date)),
    day = day(date)) %>%
  mutate(
    dow = factor(
      lubridate::wday(date, week_start = getOption("lubridate.week.start", 1)))) %>%
  mutate(
    weekend = factor(as.integer(dow %in% c(6,7))))

# add US school holidays

daily_agg <- daily_agg %>% 
  mutate(
    school_off = ((
      day>15 & month==5 & day <=30) | 
        (month==6 |  month==7) |
        (day<15 & month==8) | 
        (day>20 & month==12) ))

# add trend variable

daily_agg <- daily_agg %>% 
  mutate(trend = c(1:nrow(daily_agg)))

# add other US holidays as a dummy variable
# note: the timeDate library handles and manages date and time objects

holidays <-  as.Date(holidayNYSE(2010:2017))

daily_agg <- daily_agg %>% 
  mutate(isHoliday = ifelse(date %in% holidays,1,0))

daily_agg %>%
  summarise(
    frequency=n(),
      min = min(QUANTITY),
      P1 = quantile(QUANTITY, 0.01), 
      D1 = quantile(QUANTITY, 0.1), 
      Q1 = quantile(QUANTITY, 0.25), 
      Me = quantile(QUANTITY, 0.5), 
      Q3 = quantile(QUANTITY, 0.75), 
      D9 = quantile(QUANTITY, 0.9), 
      P99 = quantile(QUANTITY, 0.99),
      max = max(QUANTITY)) 


# define variables for EDA

# monthly averages

daily_agg <- 
  daily_agg %>% 
  group_by(month) %>% 
  mutate(q_month = mean(QUANTITY)) %>% 
  ungroup()

# log quantity (watch for 0 values)

daily_agg <- daily_agg %>% 
  mutate(QUANTITY2 = ifelse(QUANTITY<1, 1, QUANTITY)) %>% 
  mutate(q_ln = log(QUANTITY2))

# tickets sold for each day of week for each month of the year ('seasonal' averages)

daily_agg <- 
  daily_agg %>% 
  group_by(month, dow) %>% 
  mutate(tickets = mean(QUANTITY),
         tickets_ln = mean(q_ln)) %>% 
  ungroup()

# named date vars for graphs

mydays <- c("Mon","Tue","Wed",
            "Thu","Fri","Sat",
            "Sun")
daily_agg$dow_abb   <-factor(mydays[daily_agg$dow],  levels=mydays)
daily_agg$month_abb <-factor(month.abb[daily_agg$month],levels=month.abb)


# PLOTS

# sales, daily, 2015

ggplot(data=daily_agg[daily_agg$year==2015,], 
       aes(x=date, y=QUANTITY)) +
  geom_line(size=0.4, color='black') +
  scale_x_date(
    breaks = as.Date(c("2015-01-01","2015-04-01","2015-07-01","2015-10-01","2016-01-01")),
    labels = date_format("%m-%d-%Y"),
    date_minor_breaks = "1 month" ) +
  labs( x = "date (day)", y="daily ticket sales" , title = 'Tickets sold daily - 2015' ) +
  scale_color_discrete(name = "") + 
  theme_bw()

# sales, daily, 2010- 2014

ggplot(data=daily_agg[(daily_agg$year>=2010) & (daily_agg$year<=2014),], 
       aes(x=date, y=QUANTITY)) +
  geom_line(size=0.2, color='black') +
  scale_x_date(breaks = as.Date(
    c("2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01")),
    labels = date_format("%Y"),
    minor_breaks = "3 months") +
  labs( x = "date (day)", y="daily ticket sales" , title = 'Tickets sold daily - 2010-2014') +
  scale_color_discrete(name = "") + 
  theme_bw()

# monthly box plot

ggplot(
  data=daily_agg, 
    aes(x=month_abb, y=QUANTITY)) +
  labs( x = "date (month)", y="daily ticket sales", title = 'Monthly sales distribution' ) +
  geom_boxplot(color='black',outlier.color = 'darkgrey', outlier.alpha = 0.9, outlier.size = 1) + 
  theme_bw()

ggplot(
  data=daily_agg, 
  aes(x=dow_abb, y=QUANTITY)) +
  labs( x = "day of week", y="daily ticket sales", title = 'Day-of-week sales distribution' ) +
  geom_boxplot(color='black',outlier.color = 'darkgrey', outlier.alpha = 0.9, outlier.size = 1) + 
  theme_bw()

# heatmap for possible interactions

ggplot(
  daily_agg, 
    aes(x = dow_abb, y = month_abb, fill = tickets)) +
  geom_tile(colour = "white") +
  labs(x = 'day of the week', y = 'month', title = 'Interaction heatmap') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "D") +
  theme_bw() +
  theme(legend.position = "right",
        legend.text = element_text(size=6),
        legend.title =element_text(size=6))

ggplot(daily_agg, aes(x = dow_abb, y = month_abb, fill = tickets_ln)) +
  geom_tile(colour = "white") +
  labs(x = 'day of week', y = 'month', title = 'Interaction heatmap - log sales') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "D") +
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size=6),
        legend.title =element_text(size=6))



###############################################################################################
#
# MODELLING PREP

# note: cross-validation for time series is different from that for cross-sesction data


# last year of data as holdout

data_holdout<- daily_agg %>%
  filter(year==2016)

# rest is for training
data_train <- daily_agg %>%
  filter(year<2016)

# extra column for  cross-validation

data_train <- data_train %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.integer(rowname))

# a list telling which row belongs to which year

test_index_list <- data_train %>% 
  split(f = factor(data_train$year)) %>% 
  lapply(FUN = function(x){x$rowname})
  
train_index_list <- test_index_list %>% 
  lapply(FUN = function(x){setdiff(data_train$rowname, x)})

# train_control parameter of the train() function requires an other approach for 
# train-test split to cross-validation
  
train_control <- trainControl(
  method = "cv",
  index = train_index_list, #index of train data for each fold
  savePredictions = TRUE
)


###############################################################################################
#
# MODELLING

# fitting models: OLS

# model 1 linear trend + monthly seasonality

model1 <- as.formula(QUANTITY ~ 1 + trend + month)

reg1 <- train(
  model1,
  method = "lm",
  data = data_train,
  trControl = train_control
)

reg1
summary(reg1$finalModel)

ggplot(
  data = data_train, 
    aes(x = date)) + 
  geom_line(aes(y = QUANTITY), color = 'grey') + 
  geom_line(aes(y = predict(reg1, data_train)), color = 'black', size = 0.75) +
  labs(title = 'Simple linear model with monthly dummies') + 
  theme_bw()

ggplot(
  data = data_train[data_train$year == 2010,], 
  aes(x = date)) + 
  geom_line(aes(y = QUANTITY), color = 'grey') + 
  geom_line(aes(y = predict(reg1, data_train[data_train$year == 2010,])), color = 'black', size = 0.75) +
  labs(title = 'Simple linear model with monthly dummies - 2010 only') + 
  theme_bw()


# model 2 linear trend + monthly seasonality + days of week seasonality 

model2 <- as.formula(QUANTITY ~ 1 + trend + month + dow)

reg2 <- train(
  model2,
  method = "lm",
  data = data_train,
  trControl = train_control
)

ggplot(
  data = data_train, 
  aes(x = date)) + 
  geom_line(aes(y = QUANTITY), color = 'grey') + 
  geom_line(aes(y = predict(reg2, data_train)), color = 'black') +
  labs(title = 'Linear model with monthly and day-of-week dummies') + 
  theme_bw()

ggplot(
  data = data_train[data_train$year == 2010,], 
  aes(x = date)) + 
  geom_line(aes(y = QUANTITY), color = 'gray') + 
  geom_line(
    aes(y = predict(reg2, data_train[data_train$year == 2010,])), 
    color = 'black', size = 1) +
  labs(title = 'Linear model with monthly and day-of-week dummies - 2010 only') + 
  theme_bw()


# model 3 linear trend + monthly seasonality + days of week  seasonality + holidays 

model3 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday)

reg3 <- train(
  model3,
  method = "lm",
  data = data_train,
  trControl = train_control
)

ggplot(
  data = data_train, 
  aes(x = date)) + 
  geom_line(aes(y = QUANTITY), color = 'grey') + 
  geom_line(aes(y = predict(reg3, data_train)), color = 'black') +
  labs(title = 'Linear model with monthly + day-of-week + holiday dummies') + 
  theme_bw()

# model 4 linear trend + monthly seasonality + days of week  seasonality + holidays + sch*dow

model4 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday + school_off*dow)

reg4 <- train(
  model4,
  method = "lm",
  data = data_train,
  trControl = train_control
)

# model 5 linear trend + monthly seasonality + days of week  seasonality + holidays + interactions

model5 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday + school_off*dow + weekend*month)

reg5 <- train(
  model5,
  method = "lm",
  data = data_train,
  trControl = train_control
)

ggplot(
  data = data_train, 
  aes(x = date)) + 
  geom_line(aes(y = QUANTITY), color = 'grey') + 
  geom_line(aes(y = predict(reg5, data_train)), color = 'black') +
  labs(title = 'Linear model with monthly + day-of-week + holiday + school off + interaction') + 
  theme_bw()

# model 6 =  multiplicative trend and seasonality 
# note: in case of a multiplicative trend we are taking logs, predict these log values 
# and transform them back with correction term - see chapter 13 for details
# see how to get the correction term below

model6 <- as.formula(q_ln ~ 1 + trend + month + dow + isHoliday + school_off*dow)

reg6 <- train(
  model6,
  method = "lm",
  data = data_train,
  trControl = train_control
)

stargazer(reg2$finalModel, reg3$finalModel, reg4$finalModel, reg5$finalModel, type = "text", digits=2)


# Cross-validated RMSE 

model_names <- c("reg1","reg2","reg3","reg4","reg5")
rmse_CV <- c()

for (i in model_names) {
  rmse_CV[i]  <- get(i)$results$RMSE
}
rmse_CV

# we are cheating here by computing RMSE on the full train dataset 
# because could not obtain CV fold train errors

correction_term <- mean(reg6$finalModel$residuals^2)

rmse_CV["reg6"] <- reg6$pred %>% 
  mutate(pred = exp(pred  + correction_term/2)) %>% 
  group_by(Resample) %>% 
  summarise(rmse = RMSE(pred, exp(obs))) %>% 
  as.data.frame() %>% 
  summarise(mean(rmse)) %>% 
  as.numeric()

rmse_CV

# predicting with fbProphet
# tutorial here: https://facebook.github.io/prophet/docs/quick_start.html#r-api
# a quick intro to Fourier Transformation is available here: https://www.youtube.com/watch?v=spUNpyF58BY

# prophet -  multiplicative option -- tried but produced much worse results (~34. RMSE)

model_prophet <- prophet(fit=F, 
                         seasonality.mode = "additive", 
                         yearly.seasonality = "auto",
                         weekly.seasonality = "auto",
                         growth = "linear",
                         daily.seasonality=TRUE)

model_prophet <-  add_country_holidays(model_prophet, "US")

model_prophet <- fit.prophet(model_prophet, 
                             df= data.frame(
                               ds = data_train$date,
                               y = data_train$QUANTITY )
                             )

cv_pred <- cross_validation(model_prophet, initial = 365, period = 365, horizon = 365, units = 'days')

rmse_prophet_cv <- performance_metrics(cv_pred, rolling_window = 1)$rmse

rmse_prophet_cv

###########################x
# Evaluate best model on holdout set --------------------------------------------
###########################x

data_holdout <- data_holdout %>% 
  mutate(y_hat_5 = predict(reg5, newdata = .))

rmse_holdout_best <- RMSE(data_holdout$QUANTITY, data_holdout$y_hat_5)
rmse_holdout_best

###########################x
# Plot best predictions --------------------------------------------
###########################x

#graph relative RMSE (on holdout) per month 
rmse_monthly <- data_holdout %>% 
  mutate(month = factor(format(date,"%b"), 
                        levels= unique(format(sort(.$date),"%b")), 
                        ordered=TRUE)) %>% 
  group_by(month) %>% 
  summarise(
    RMSE = RMSE(QUANTITY, y_hat_5),
    RMSE_norm= RMSE(QUANTITY, y_hat_5)/mean(QUANTITY)
            ) 

g_predictions_rmse<- ggplot(rmse_monthly, aes(x = month, y = RMSE_norm)) +
  geom_col(bg=color[1], color=color[1]) +
  labs( x = "Date (month)", y="RMSE (normalized by monthly sales)" ) +
    theme_bg() 
g_predictions_rmse
#save_fig("ch18_swim_predictions_rmse", output, "small")
save_fig("ch18-figure-7b-swim-predictions-rmse", output, "small", plot=g_predictions_rmse)

g_predictions<-
  ggplot(data=data_holdout, aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat_5, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(expand=c(0,0), breaks = as.Date(c("2016-01-01","2016-03-01","2016-05-01","2016-07-01","2016-09-01","2016-11-01", "2017-01-01")),
               labels = date_format("%d%b%Y"),
               date_minor_breaks = "1 month" )+
  scale_color_manual(values=color[1:2], name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_bg() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-07-15"), y = 50, label = "Predicted", color=color[2], size=3)+
  #annotate("text", x = as.Date("2016-09-01"), y = 125, label = "Actual", color=color[1], size=3)
  theme(legend.position=c(0.7,0.8),
      legend.direction = "horizontal",
      legend.text = element_text(size = 6),
      legend.key.width = unit(.8, "cm"),
      legend.key.height = unit(.3, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.8))
         )
g_predictions
#save_fig("ch18_swim_predictions", output, "large")
save_fig("ch18-figure-6-swim-predictions", output, "large", plot=g_predictions)


g_predictions_m <- ggplot(data=data_holdout %>% filter(month==8), aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat_5, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  geom_ribbon(aes(ymin=QUANTITY,ymax=y_hat_5), fill=color[4], alpha=0.3) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,150))+
  scale_x_date(expand=c(0.01,0.01), breaks = as.Date(c("2016-08-01","2016-08-08","2016-08-15","2016-08-22","2016-08-29")),
               limits = as.Date(c("2016-08-01","2016-08-31")),
               labels = date_format("%d%b")) +
  scale_color_manual(values=color[1:2], name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_bg() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-08-04"), y = 55, label = "Actual", color=color[2], size=2)+
  #annotate("text", x = as.Date("2016-08-17"), y = 115, label = "Predicted", color=color[1], size=2)
  theme(legend.position=c(0.7,0.8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6))
  )
g_predictions_m
#save_fig("ch18_swim_predictions_m", output, "small")
save_fig("ch18-figure-7a-swim-predictions-m", output, "small", plot=g_predictions_m)

#ch18-table-1-swim-rmse
#ch18-table-2-cs-models-rmse
#ch18-table-3-arima-folds