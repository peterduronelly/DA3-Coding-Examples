# CHAPTER 14
# CH14 Airbnb
# version x 2023-01-07
# based on Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# and on modifications by Agoston Reguly (github.com/regulyagoston)
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x



# ------------------------------------------------------------------------------------------------------
#### SET UP
# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(scales)


################################################################################
# UDF
################################################################################

price_diff_by_variables2 <- function(df, factor_var, dummy_var, factor_lab, dummy_lab){
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe,
  # 2) the factor variable (like room_type)
  # 3)the dummy variable you are interested in (like TV)
  
  # Process your data frame and make a new dataframe which contains the stats
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)
  
  stats <- df %>%
    group_by(!!factor_var, !!dummy_var) %>%
    dplyr::summarize(
      Mean = mean(price, na.rm=TRUE),se = sd(price)/sqrt(n()))
  
  stats[,2] <- lapply(stats[,2], factor)
  
  ggplot(
    stats, 
    aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(
      stat='identity', 
      position = position_dodge(width=0.9), 
      alpha=0.8)+
    geom_errorbar(
      aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)),
      position=position_dodge(width = 0.9), 
      width = 0.25)+
    scale_color_manual(
      name=dummy_lab,
      values=c('#000000','#990033')) +
    scale_fill_manual(
      name=dummy_lab,
      values=c('#000000','#990033')) +
    ylab('Mean Price')+
    xlab(factor_lab) +
    theme_bw()+
    theme(
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border=element_blank(),
      axis.line=element_line(),
      legend.position = "top",
      legend.box = "vertical",
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 5, face = "bold"),
      legend.key.size = unit(x = 0.4, units = "cm")
    )
}



################################################################################
# PART I - EDA
################################################################################



# Load data


# Used area
data <-
  read_csv('https://raw.githubusercontent.com/peterduronelly/DA3-Coding-Examples/main/data/airbnb_hackney_workfile.csv') %>%
  mutate_if(is.character, factor)

################################################################################
# Quick look at data #
################################################################################
glimpse(data)
skim(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# what to do with missing values?


# 1. drop if no target (already did)
data <- data %>%
  drop_na(price)


# 2. imput when few, not that important
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds)
  )


# 3. drop columns when many missing not imortant
to_drop <- c("usd_cleaning_fee", "p_host_response_rate", "d_reviews_per_month")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_days_since = ifelse(is.na(n_days_since),1, 0),
    n_days_since    =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating = ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating    = ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month    = ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month       = ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
  )

table(data$flag_days_since)

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# 5. Add features: existing variables in different functional form
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )

data <- data %>%
  mutate(ln_price = log(price))


# Look at data
summary(data$price)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]



################################################################################
# Business logic: define our prediction problem
# Filter irrelevant observations
################################################################################

# Decision
# Size, we need a normal apartment, 1-7persons
data <- data %>%
  filter(n_accommodates < 8
         )

# that's gonna be our sample
skimr::skim(data)


################################################################################
# Descriptive statistics
################################################################################

#How is the average price changing in my district by `property_type`, `room_type` and the `bed_type`?
data %>%
  group_by(f_property_type, f_room_type) %>%
  summarise(
    frequency=n(),
    min = min(price),
    P1 = quantile(price, 0.01), 
    D1 = quantile(price, 0.1), 
    Q1 = quantile(price, 0.25), 
    Me = quantile(price, 0.5),
    mean = mean(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75), 
    D9 = quantile(price, 0.9), 
    P99 = quantile(price, 0.99),
    max = max(price))

data %>%
  group_by(f_bed_type) %>%
  summarise(
    frequency=n(),
    min = min(price),
    P1 = quantile(price, 0.01), 
    D1 = quantile(price, 0.1), 
    Q1 = quantile(price, 0.25), 
    Me = quantile(price, 0.5),
    mean = mean(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75), 
    D9 = quantile(price, 0.9), 
    P99 = quantile(price, 0.99),
    max = max(price))

Hmisc::describe(data$price)


# to avoid messing up graphs, we exclude extremely pricey observations 
# anything above 400 is considered to be extreme; this is the subjective decision of the modeler! 
datau <- subset(data, price<400)


# Distribution of price by type below 400

# Histograms
# price
ggplot(data = datau, aes(price)) + 
  geom_histogram(
    aes(y = (..count..)/sum(..count..)), 
    bins = 20, 
    fill = 'black', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(title = "Price distribution", x = "price in USD", y = "percent") +
  theme(plot.title = element_text(size = rel(1))) +
  scale_x_continuous(
    limits=c(0,400), breaks = seq(0,400, 50)) + 
  scale_y_continuous(
    expand = c(0.00,0.00), limits=c(0, 0.3), breaks = seq(0, 0.30, by = 0.02), 
    labels = scales::percent_format(1))

# lnprice: log values in x-axis
ggplot(data = datau, aes(x=ln_price)) + 
  geom_histogram(
    aes(y = (..count..)/sum(..count..)), 
    bins = 20, fill = 'black', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(title = "Log price distribution", x = "price in USD", y = "percent") +
  theme(plot.title = element_text(size = rel(1)))+ 
  scale_x_continuous(breaks = seq(2.4,6.6, 0.6)) + 
  scale_y_continuous(
    expand = c(0.00,0.00), 
    limits=c(0, 0.16), breaks = seq(0, 0.30, by = 0.02), 
    labels = scales::percent_format(1))

# lnprice: original values, log scale in x-axis
# require(scales)
ggplot(data = datau, aes(price)) + 
  geom_histogram(
    aes(y = (..count..)/sum(..count..)), 
    bins = 20, 
    fill = 'black', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(title = "Price distribution on log scale", x = "price in USD (log scale)", y = "percent") +
  theme(plot.title = element_text(size = rel(1))) + 
  scale_x_log10() + 
  annotation_logticks(sides = 'b') + 
  scale_y_continuous(
    expand = c(0.00,0.00), 
    limits=c(0, 0.16), 
    breaks = seq(0, 0.30, by = 0.02), labels = scales::percent_format(1))


## Boxplot: price by room type
ggplot(
  data = datau, 
  aes(x = f_room_type, y = price)) +
  stat_boxplot(
    aes(group = f_room_type), 
   geom = "errorbar", width = 0.3,
   color = c("#000000","#333333", "#666666"), size = 0.5, na.rm=T)+
  geom_boxplot(
    aes(group = f_room_type),
    color = c("#000000","#333333", "#666666"), 
    fill = c("#000000","#333333", "#666666"),
    size = 0.5, width = 0.6, alpha = 0.8, na.rm=T, 
    outlier.shape = NA) +
  scale_y_continuous(
    expand = c(0.01,0.01),
    limits = c(0,250), breaks = seq(0,250,50)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_bw()


# Boxplot: price by property type & number of accommodates
ggplot(
  data = datau, 
  aes(
    x = factor(n_accommodates), y = price,
    fill = factor(f_property_type), 
    color=factor(f_property_type))) +
  geom_boxplot(
    alpha=0.8, 
    na.rm=T, 
    outlier.shape = NA, 
    width = 0.8) +
  stat_boxplot(
    geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(
    name="", values=c("#000000","#990033")) +
  scale_fill_manual(
    name="", values=c("#000000","#990033")) +
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme_bw() +
  theme(legend.position = c(0.3,0.8))



################################################################################
# PART II - MODELLING
################################################################################


################################################################################
# Setting up models                 
################################################################################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since", "flag_days_since")

# Factorized variables
basic_add <- c("f_bathroom","f_cancellation_policy","f_bed_type")
reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)


################################################################################
# Interactions
################################################################################

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "f_room_type", "d_familykidfriendly", "Room type", "Family kid friendly")
p2 <- price_diff_by_variables2(data, "f_room_type", "f_property_type", "Room type", "Property type")
#Look up canelation policy
p3 <- price_diff_by_variables2(data, "f_cancellation_policy", "d_familykidfriendly", "Cancellation policy", "Family kid friendly")
p4 <- price_diff_by_variables2(data, "f_cancellation_policy", "d_tv", "Cancellation policy", "TV")
#Look up property type
p5 <- price_diff_by_variables2(data, "f_property_type", "d_cats", "Property type", "Cats")
p6 <- price_diff_by_variables2(data, "f_property_type", "d_dogs", "Property type", "Dogs")

g_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
g_interactions


# dummies suggested by graphs
X1  <- c("f_room_type*f_property_type",  "f_room_type*d_familykidfriendly")

# additional interactions of factors and dummies
X2  <- c("d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type")
X3  <- c(
  paste0(
    "(f_property_type + f_room_type + f_cancellation_policy + f_bed_type) * (",
    paste(amenities, collapse=" + "),")"))

# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))


################################################################################
# separate hold-out set 
################################################################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20230118)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)


################################################################################
# cross validation      
################################################################################

## K = 5
k_folds=5
# Create the folds
set.seed(20230118)

folds_i <- sample(rep(1:k_folds, length.out = nrow(data_work)))

# Create results
model_names <- c()
model_nvars <- c()
model_bic <- c()
model_r2 <- c()
model_rmse_train <- c()
model_rmse_test <- c()


for (i in (1:8)){
  print(paste0( "Estimating model: " ,i ))
  
  # get model
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")

  # specify formula
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))

  # initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  # estimate regression on the whole work data
  print('Here comes OLS')
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  print(paste0('BIC: ', BIC))
  nvars <- model_work_data$rank -1
  print(paste0('nvars: ', nvars))
  r2 <- summary(model_work_data)$r.squared
  print(paste0('r2: ', r2))
  rmse_train = sqrt(sum(model_work_data$residuals^2)/nrow(data_work))
  print(paste0('rmse train: ', rmse_train))
  
  # cross-validation
  set.seed(20230118)
  cv_i = train(
    formula, data_work, method = 'lm', 
    trControl = trainControl(method = 'cv', number = k_folds),
    na.action = "na.omit"
  )
  print(paste0('CV RMSE: ', cv_i$resample$RMSE))
  rmse_test = sqrt(sum(cv_i$resample$RMSE^2)/k_folds)
  print(paste0('rmse test: ', rmse_test))
  
  # gather key metrics
  model_names[i] <- model_pretty_name
  model_nvars[i] <- nvars
  model_bic[i] <- BIC
  model_r2[i] <- r2
  model_rmse_train[i] <- rmse_train
  model_rmse_test[i] <- rmse_test
}

# combine results
cv_result = data.frame(
  model_names, 
  model_nvars, 
  model_bic,
  model_r2, 
  model_rmse_train,
  model_rmse_test)
  
colnames(cv_result) <- c('model', 'coefficients', "BIC", "R2", "RMSE_train", "RMSE_test")

colors = c("Training RMSE"="#000000","Test RMSE" = "#990033")

ggplot( data = cv_result, aes( x = factor( coefficients ) , group = 1 ) )+
  geom_line(aes( y = RMSE_train, color = 'Training RMSE'), size = 1 ) +
  geom_point(aes( y = RMSE_train, color = 'Training RMSE'), size = 2 ) + 
  geom_line(aes( y = RMSE_test , color = 'Test RMSE') , size = 1 ) +
  geom_point(aes( y = RMSE_test , color = 'Test RMSE') , size = 2 ) +
  labs(y='RMSE',x='Number of coefficients',color = "", title = "RMSE: Training & Test")+
  scale_color_manual(values = colors) + 
  scale_y_continuous(
    expand = expansion(),  limits = c(30, 45), breaks = seq(30, 45, by = 1)) +
  theme_bw()+
  theme(legend.position=c(0.5, 0.8))



################################################################################
# LASSO
################################################################################

# take model 8 (and find observations where there is no missing data)may
vars_model_7 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities)
vars_model_8 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = k_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_8, "price"), collapse = " + ")))

set.seed(20230125)

lasso_model <- caret::train(
  formula,
  data = data_work,
  method = "glmnet",
  preProcess = c("center", "scale"),
  trControl = train_control,
  tuneGrid = tune_grid,
  na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)

lasso_cv_metrics <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE, Rsquared)

lasso_metrics <- tibble(
  model='LASSO',
  coefficients = nrow(lasso_coeffs_nz), 
  BIC = NA, 
  R2 =  lasso_cv_metrics[2][[1]], 
  RMSE_train = NA,
  RMSE_test =  lasso_cv_metrics[1][[1]] )

cv_result <- rbind(cv_result, lasso_metrics)


################################################################################
# PART III - DIAGNOSTICS
################################################################################

# 1. Performance on the holdout set
# model 3, model 7 & lasso

yvar <- "price"

# m3
# estimate regression on the whole work data
formula <- formula(paste0(yvar,modellev3))
m3 <- lm(formula, data = data_work)
# predict on the holdout set
m3_pred <- predict(m3, newdata = data_holdout)
m3_rmse = RMSE(m3_pred, data_holdout$price)

# m7
# estimate regression on the whole work data
formula <- formula(paste0(yvar,modellev7))
m7 = lm(formula, data = data_work)
# predict on the holdout set
m7_pred = predict(m7, newdata = data_holdout)
m7_rmse = RMSE(m7_pred, data_holdout$price)


# lasso
ml_pred <- predict(lasso_model, newdata = data_holdout)
ml_rmse <- RMSE(ml_pred, data_holdout$price)

results <- rbind(m3_rmse, m7_rmse, ml_rmse)
rownames(results) <- c('Model 3','Model 7','LASSO')
colnames(results) <- c('RMSE on hold-out sample')
results

# 2. Plot your results for lasso

data_holdout$ml_pred <- ml_pred

ggplot( 
  data_holdout , aes( y = price , x = ml_pred ) ) +
  geom_point( 
    size = 1 , 
    color = 'black' ) +
  geom_abline( 
    intercept = 0, 
    slope = 1, 
    size = 1, 
    color = 'black' , 
    linetype = 'dashed'
  ) +
  xlim(-1,max(data_holdout$price))+
  ylim(-1,max(data_holdout$price))+
  labs(
    x='Predicted price (US$)',
    y='Price (US$)', 
    title = "Predicted vs actual price")+
  theme_bw()

# 3. Predict and plot with model 7

m7_pPI <- predict( m7 , newdata = data_holdout , interval = 'predict' , level = 0.8 )
m7_pPI <- m7_pPI %>% as.matrix() %>% as.data.frame()

data_holdout$m7_pred <- m7_pPI$fit
data_holdout$m7_pred_pi80_l <- m7_pPI$lwr
data_holdout$m7_pred_pi80_h <- m7_pPI$upr

pred_by_accommodates <- data_holdout %>% 
  select( n_accommodates , m7_pred , m7_pred_pi80_l, m7_pred_pi80_h ) %>% 
  group_by( n_accommodates ) %>% 
  summarise(
    fit = mean(m7_pred, na.rm=TRUE), 
    pred_lwr = mean(m7_pred_pi80_l, na.rm=TRUE), 
    pred_upr = mean(m7_pred_pi80_h, na.rm=TRUE))

ggplot(
  pred_by_accommodates, aes(x=factor(n_accommodates))) +
  geom_point( aes(y = fit), 
    size = 10 , 
    color = 'black' ) +  
  geom_errorbar(
    aes(
      ymin=pred_lwr, 
      ymax=pred_upr  
      ),
    width=.3,size=1) +
  scale_y_continuous(
    name = "Predicted price (US dollars)"
    ) +
  scale_x_discrete(
    name = "Accomodates (Persons)"
    ) +
  labs(title = 'Predictions with 80% confidence intervals') +
  theme_bw() 

# 4. How do lasso coefficients change as lambda changes? 

plot(lasso_model$finalModel , xvar = "lambda", label = TRUE)


