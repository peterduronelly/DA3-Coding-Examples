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

library(tidyverse)
library(fpp3)
library(cowplot)
library(scales)
library(gridExtra)
library(Rcpp)

#########################################################################################
#
# UDFS


get_RMSE_from_model <- function(m, resid_col_name = ".resid", groupby = c(".id", ".model")){
  m %>%
    residuals() %>%
    as_tibble() %>%
    group_by_at(groupby) %>%
    summarise(RMSE = mean(get(resid_col_name)**2, na.rm = TRUE)**(1/2))
}

get_MSE_from_forecast <- function(forecast, groupby = c(".id", ".model")){
  forecast %>%
    as_tibble() %>%
    group_by_at(groupby) %>%
    summarise(MSE = mean(e^2)) %>%
    ungroup()
}


#########################################################################################
# 
# DATA PREP

#load raw data

data <- read_csv('https://osf.io/n3jty/download')

# 18 years data
# 1 year holdout
# 4 years of test
# 13 years of train (rolling window)
# pick if seasonal or non seasonal version used, will be cut later
# here we pick pn, not seasonally adjusted

data <- data %>% mutate(date = yearmonth(date))

# pick nsa (non-seasonally adjusted) data as prices, unemployment rate, employment

data <- data %>%
  mutate(
    p=pn,
    u=us,
    emp=emps
  )


# logs, lags, differences, trend + monthly dummies

data <- data %>%
  mutate(
    dp   = difference(p, lag=1, order_by = date),
    p_lag = lag(p),
    lnp = log(p),
    dlnp   = difference(lnp, lag=1, order_by = date),
    lnp_lag = lag(lnp),
    dlnp_lag   = lag(dlnp),
    du     = difference(u, lag=1, order_by = date),
    lnemp = log(emp),
    dlnemp = difference(lnemp, lag=1, order_by = date)
  ) %>%
  mutate(
    trend = 1:nrow(data),
    month = as.factor(month(date))
  )

# save as time-series tibble

data <- data %>% as_tsibble(index=date)

# note:  Built on top of the tibble, a tsibble (or tbl_ts) is a data- and model-oriented object. 
#   The tsibble package provides a 'tbl_ts' class (the 'tsibble') for
#   temporal data in an data- and model-oriented format. The 'tsibble'
#   provides tools to easily manipulate and analyse temporal data, such as
#   filling in time gaps and aggregating over calendar periods.
#   https://cran.rstudio.com/web/packages/tsibble/vignettes/intro-tsibble.html

# duplicate for later validity check

data_all <- data

# workfile with data from 2000 through 2017

data <- data %>% filter(year <= 2017)



#########################################################################################
# 
# EDA

# levels

gl1 = ggplot(data = data, 
       aes(x = as.Date(date), y = p))+
  geom_line() +
  ylab('2000 = 100') +
  xlab("date") +
  labs(title = "Case-Shiller Home Price index - nsa values") + 
  scale_y_continuous(limits = c(50,300), breaks = seq(50,300,50)) +
  scale_x_date(expand = c(0.01, 0.01),   
               breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  
               labels = date_format("%Y")) +
  theme_bw()

gl2 = ggplot(data = data, 
             aes(x = as.Date(date), y = emp))+
  geom_line() +
  ylab('thousands') +
  xlab("date") +
  labs(title = "US employment") + 
  scale_x_date(expand = c(0.01, 0.01),   
               breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  
               labels = date_format("%Y")) +
  theme_bw()

# changes

gc1 = ggplot(
  data = data %>% filter(!is.na(dlnp)), 
    aes(x = as.Date(date), y = dlnp))+
  geom_line() +
  ylab("log first differences") +
  xlab("date") +
  geom_hline(yintercept = 0) + 
  labs(title = "Monthly change in the Case-Shiller Home Price index") + 
  scale_y_continuous(limits = c(-0.04,0.04), breaks = seq(-0.04,0.04,0.01)) +
  scale_x_date(date_breaks="2 years", labels = date_format("%Y")) +
  theme_bw()

gc2 = ggplot(data = data %>% filter(!is.na(dlnemp)), 
       aes(x = as.Date(date), y = dlnemp))+
  geom_line() +
  geom_hline(yintercept = 0) + 
  ylab('log first differences') +
  xlab("date") +
  labs(title = "Monthly change in US employment") + 
  scale_x_date(expand = c(0.01, 0.01),   
               breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  
               labels = date_format("%Y")) +
  theme_bw()


grid.arrange(gl1, gl2, ncol = 1, nrow = 2)

grid.arrange(gc1, gc2, ncol = 1, nrow = 2)



#########################################################################################
# 
# DATA PREP

# holdout: last year of data

data_holdout <- data %>%
  slice((n()-11):n())

# work 

data_work <- data %>%
  slice(1:(n()-12))

# prepare for cross-validation, define size of train
# here we are creating overlapping 13-year time periods for CV
#   2000-2012
#   2001-2013
#   2002-2014
#   2003-2015

train_length=13

data_tr <- data_work %>%
  slice(1:(n()-12)) %>% # last year of training data not used in any fold as training
  slide_tsibble(.size = train_length*12, .step = 12)

data_cv_test <- data_work %>%
  slice(train_length*12+1:n()) %>% 
  slide_tsibble(.size = 12, .step = 12) %>%
  select(trend, month)



#########################################################################################
# 
# MODELLING


# ARIMA

# with only tseries of price index

# note: to cross-validate auto.arima,
# step 1: run it and find ARIMA specification on the whole train data, p,q chosen by BIC
#   note, need to add PDQ(0,0,0) to models 
#         in order to shut down the fancy seasonality-fitting part of auto ARIMA
# step 2: use the selected model as a candidate


# m1: p ~ month + trend, without any ARIMA

m1_formula <- "p ~ month + trend"
m1 <- TSLM(as.formula(m1_formula))

# note: TSLM comes from the fable package (https://cran.r-project.org/web/packages/fable/fable.pdf)
#   which fits a linear model with time series components. The TSLM function returns 
#   a 'model specification'


# m2: p ~ auto ARIMA

m2_pre <- data_work %>%
  model(auto_arima = ARIMA(p ~  PDQ(0,0,0)))

# note: ARIMA() function comes from the fable package. The auto_arima parameter 
#   is for the automated discovery of the optimal order for an ARIMA model.
# 
# The model() function comes from the fabletools package and it trains specified 
#   model definition(s) to a datasetto each series within .data (as identified by the key structure). 
#   The result will be a mable (a model table), which neatly stores the estimated 
#   models in a tabular structure. Rows of the data identify different series within the data, 
#   and each model column contains all models from that model definition. 
#   Each cell in the mable identifies a single model.


m2_pre$auto_arima[[1]]$fit$model
m2_pre$auto_arima[[1]]$fit$par
m2_pre$auto_arima[[1]]$fit$est
m2_pre$auto_arima[[1]]$fit$spec

# extracting the optimal AR, I, and MA configs from the model's fit attribute
p2_auto <- m2_pre$auto_arima[[1]]$fit$spec$p # AR
d2_auto <- m2_pre$auto_arima[[1]]$fit$spec$d # I
q2_auto <- m2_pre$auto_arima[[1]]$fit$spec$q # MA

m2_formula <- paste0("p ~  pdq(",paste(p2_auto,d2_auto,q2_auto, sep=","),") + PDQ(0,0,0)")

m2 <-  ARIMA(as.formula(m2_formula))

typeof(m2)

# more on environments here: http://adv-r.had.co.nz/ (Advanced R by Hadley Wickham)


# m3: p ~ auto ARIMA + month

m3_pre <- data_work %>%
  model(auto_arima = ARIMA(p ~ month+  PDQ(0,0,0)))

p3_auto <- m3_pre$auto_arima[[1]]$fit$spec$p
d3_auto <- m3_pre$auto_arima[[1]]$fit$spec$d
q3_auto <- m3_pre$auto_arima[[1]]$fit$spec$q

m3_formula <- paste0("p ~  pdq(",paste(p3_auto,d3_auto,q3_auto, sep=","),") + PDQ(0,0,0) + month")

m3 <-  ARIMA(as.formula(m3_formula))


# m4: p ~ auto ARIMA + month + trend

m4_pre <- data_work %>%
  model(auto_arima = ARIMA(p ~ month + trend + PDQ(0,0,0)))
p4_auto <- m4_pre$auto_arima[[1]]$fit$spec$p
d4_auto <- m4_pre$auto_arima[[1]]$fit$spec$d
q4_auto <- m4_pre$auto_arima[[1]]$fit$spec$q

m4_formula <- paste0("p ~  pdq(",paste(p4_auto,d4_auto,q4_auto, sep=","),") + PDQ(0,0,0) + month + trend")

m4 <-  ARIMA(as.formula(m4_formula))


# m5: dp ~ month + trend, without any ARIMA

m5_formula <- "dp ~ month + trend"

m5 <- TSLM(as.formula(m5_formula))

# m6: lnp ~ auto ARIMA + month

m6_pre <- data_work %>%
  model(auto_arima = ARIMA(lnp ~  month + PDQ(0,0,0)))

p6_auto <- m6_pre$auto_arima[[1]]$fit$spec$p
d6_auto <- m6_pre$auto_arima[[1]]$fit$spec$d
q6_auto <- m6_pre$auto_arima[[1]]$fit$spec$q

m6_formula <- paste0("lnp ~ month + pdq(",paste(p6_auto,d6_auto,q6_auto, sep=","),") + PDQ(0,0,0)")

m6 <-  ARIMA(as.formula(m6_formula))


# cross-validate & forecast

# cross-validating m1-m4 with p on left-hand-side

models_1_4 <- data_tr %>%
  model(m1 = m1,
        m2 = m2,
        m3 = m3,
        m4 = m4
  )

rmse_train_1_4 <- models_1_4 %>%
  get_RMSE_from_model()

# forecasting and calculating MSE

forecast_1_4 <- models_1_4 %>%
  forecast(new_data = data_cv_test) %>%
  as_tsibble() %>%
  dplyr::rename(p_pred = .mean) %>%
  select(.id, .model, date, p_pred) %>%
  left_join(data[,c("date","p")]) %>%
  group_by(.id, .model) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

# The forecast function allows you to produce future predictions of a time series from fitted models.
#   If the response variable has been transformed in the model formula, the transformation will be
#   automatically back-transformed

# get MSE for folds

summary_1_4 <- forecast_1_4 %>%
  get_MSE_from_forecast()


# cross-validating m5 with dp on left-hand-side

model_5 <- data_tr %>%
  model(m5 = m5)

rmse_train_dp <- model_5 %>%
  get_RMSE_from_model()

# forecast

forecast_5 <- model_5 %>%
  forecast(new_data = data_cv_test) %>%
  as_tsibble() %>%
  dplyr::rename(dp_pred = .mean) %>%
  select(.id, .model, date, dp_pred) %>%
  left_join(data[,c("date","p","p_lag")]) %>%
  group_by(.id, .model) %>%
  mutate(p_pred = cumsum(dp_pred) + p_lag[1]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

# compute MSE for folds

summary_5 <- forecast_5 %>%
  get_MSE_from_forecast()

summary_5

# cross-validating & forecasting m6 with lnp on left-hand-side

model_6 <- data_tr %>%
  model(m6 = m6)

rmse_train_6 <- model_6 %>%
  get_RMSE_from_model()

forecast_6 <- model_6 %>%
  forecast(new_data = data_cv_test) %>%
  as_tsibble() %>%
  dplyr::rename(lnp_pred = .mean) %>%
  select(.id, .model, date, lnp_pred) %>%
  left_join(data[,c("date","p")]) %>%
  left_join(rmse_train_6) %>%
  group_by(.id, .model) %>%
  mutate(p_pred = exp(lnp_pred)*exp((RMSE**2)/2) ) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_6 <- forecast_6 %>%
  get_MSE_from_forecast()

summary_6


# collect CV RMSE for all models
# as in Table 18.2

summary_folds <- bind_rows(list(summary_1_4, summary_5, summary_6)) %>%
  spread(.id, MSE) %>%
  as.data.frame()

colnames(summary_folds) <- c("Model", paste0("Fold ", colnames(summary_folds)[-1]))

summary_final <- bind_rows(list(summary_1_4, summary_5, summary_6)) %>%
  group_by(.model) %>%
  dplyr::summarise(CV_RMSE = sum(MSE/4)**0.5) %>%
  as.data.frame()

model_formulas <- summary_final %>%
  dplyr::pull(.model) %>%
  paste0("_formula") %>%
  sapply(FUN=get)

colnames(summary_final) <- c("Model", "CV RMSE")

summary_table_18_2 <- summary_final %>%
  add_column("Model def" = model_formulas, .before = "CV RMSE")

summary_table_18_2



# VECTOR AUTOREGRESSION (VAR)

# Comment: In the textbook, Table 18.3 has VAR RMSE values for the model without seasonality. 
# It’s noted at \url{https://gabors-data-analysis.com/errata/#part-iii} 
# Without seasonality, we have: RMSE (average) =8.0. With seasonality, we have: RMSE (average) =4.5. 
# In R we could do not figure out how to add seasonality. Let us know if you solved it...    


var_formula <- "vars(dp, du, dlnemp) ~ AR(1) "

var <- VAR(as.formula(var_formula))


# cross-validate & forecast

var_data <- data_tr %>%
  filter(!is.na(dp)) %>% # need to exclude first row
  model(var = var)

rmse_train_var <- var_data %>%
  get_RMSE_from_model(resid_col_name = "dp")


aux <- var_data %>%
  forecast(h=12) %>% 
  as_tsibble()

aux$dp_pred <- aux$.mean_dp

# get errors by calculating price predictions using the predicted values of dp

forecast_var <- aux %>%
  select(.id, .model, date, dp_pred) %>%
  left_join(data[,c("date","p","p_lag")]) %>%
  group_by(.id, .model) %>%
  mutate(p_pred = cumsum(dp_pred) + p_lag[1]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

# compute MSE for folds

summary_var <- forecast_var %>%
  get_MSE_from_forecast()

summary_var


# rmse by folds + cv rmse, for all 7 models
# (Table 18.3 )

summary_folds <- bind_rows(list(summary_1_4, summary_5, summary_6, summary_var)) %>%
  spread(.id, MSE) %>%
  as.data.frame()

colnames(summary_folds) <- c("Model", paste0("Fold ", colnames(summary_folds)[-1]))

# Table 18.3 RMSE by folds

summary_rmse_folds <- summary_folds %>%
  mutate_at(vars(-Model), sqrt)

summary_rmse_folds

# Table 18.3 last column: cv average RMSE
# create average MSE across folds and take square root

summary_cvavg <- bind_rows(list(summary_1_4, summary_5, summary_6, summary_var)) %>%
  group_by(.model) %>%
  dplyr::summarise(CV_RMSE = sum(MSE/4)**0.5) %>%
  as.data.frame()

model_formulas <- summary_cvavg %>%
  dplyr::pull(.model) %>%
  paste0("_formula") %>%
  sapply(FUN=get)

colnames(summary_cvavg) <- c("Model", "CV RMSE")

summary_table_18_3_lastcol <- summary_cvavg %>%
  add_column("Model def" = model_formulas, .before = "CV RMSE")

summary_table_18_3_lastcol


#########################################################################################
# 
# PREDICTION

# best model is M4

bestm <- "m4"

conf_level <-  80
conf_level_chr <- paste0(as.character(conf_level),"%")

# re-estimate best model on full work set

model_best <- data_work %>%
  model(best = get(bestm))

rmse_train_best <- model_best %>%
  get_RMSE_from_model(groupby = c(".model"))

# forecast for holdout

forecast_holdout_best <- model_best %>%
  forecast(new_data = select(data_holdout, trend, month)) %>%
  hilo(level = c(conf_level)) %>%
  as_tsibble() %>%
  rename(p_pred = .mean)  %>%
  select(.model, date, p_pred, conf_level_chr) %>%
  unpack_hilo(conf_level_chr) %>%
  left_join(data_holdout[,c("date","p")]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_holdout_best <- forecast_holdout_best %>%
  get_MSE_from_forecast(groupby = c(".model"))

summary_holdout_best


#########################################################################################
# 
# GRAPHS

# 2015-18, actual vs prediction from best arima

data_plot <- data %>%
  left_join(forecast_holdout_best) %>%
  filter(year(date)>=2015)

conf_level_lower <- paste0(conf_level_chr, "_lower")
conf_level_upper <- paste0(conf_level_chr, "_upper")

  
ggplot(
  data = data_plot , 
    aes(x = as.Date(date), y = p))+
  geom_line(size = 0.8, aes(color = "actual")) +
  geom_line(aes(x = as.Date(date), y = p_pred, color = "prediction "),  size = 1) +
  geom_ribbon(aes(ymin =  get(conf_level_lower), ymax = get(conf_level_upper)), alpha=0.2,   bg='#990033') +
  ylab("2000 = 100") +
  xlab("date") +
  labs(title = 'Case-Shiller Home Price Index - actual vs prediction interval') + 
  scale_color_manual(name="",values=c('black', '#990033')) +
  scale_x_date(date_breaks="1 years", labels = date_format("%Y")) +
  theme_bw()+
  theme(legend.position=c(0.7,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
