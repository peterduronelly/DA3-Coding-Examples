# CHAPTER 14
# CH14 Airbnb
# version x 2023-01-07
# based on Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
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


########################################
# PART I - EDA
########################################

# !!! make sure you have run ch14_airbnb_prepare.R before


#############
# Load data #
#############

# Used area
area <- "hackney"
data <-
  read_csv('https://raw.githubusercontent.com/peterduronelly/DA3-Coding-Examples/main/data/airbnb_hackney_workfile.csv') %>%
  mutate_if(is.character, factor)


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

mse_lev <- function(pred, y) {
  # Mean Squared Error for log models
  (mean((pred - y)^2, na.rm=T))
}



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
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accommodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
#    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
    )


# 3. drop columns when many missing not imortant
to_drop <- c("usd_cleaning_fee", "p_host_response_rate")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    n_bathrooms = ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), 
    n_beds = ifelse(is.na(n_beds), median(n_beds, na.rm = T), n_beds), 
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
          )

table(data$flag_days_since)



# 5. Add features: existing variables in different functional form
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    n_accommodates2 = n_accommodates^2, 
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
  labs(title = "Price distribution", x = "price in USD", y = "percent") +
  theme(plot.title = element_text(size = rel(1)))+ 
  scale_x_continuous(breaks = seq(2.4,6.6, 0.6)) + 
  scale_y_continuous(
    expand = c(0.00,0.00), 
    limits=c(0, 0.16), breaks = seq(0, 0.30, by = 0.02), 
    labels = scales::percent_format(1))

# lnprice: original values, log scale in x-axis
require(scales)
ggplot(data = datau, aes(price)) + 
  geom_histogram(
    aes(y = (..count..)/sum(..count..)), 
    bins = 20, 
    fill = 'black', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(title = "Price distribution log price", x = "price in USD (log scale)", y = "percent") +
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
basic_add <- c("f_cancellation_policy","f_bed_type")
reviews <- c("n_review_scores_rating", "flag_review_scores_rating")
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
X3  <- c(paste0(
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
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  rmse_train = sqrt(sum(model_work_data$residuals^2)/nrow(data_work))
  
  # cross-validation
    cv_i = train(
    formula, data_work, method = 'lm', 
    trControl = trainControl(method = 'cv', number = k_folds)
  )
  
  rmse_test = sqrt(sum(cv_i$resample$RMSE^2)/k_folds)
  
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
    expand = expansion(),  limits = c(34, 44), breaks = seq(32, 44, by = 1)) +
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

set.seed(20230118)

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
  model = nrow(lasso_coeffs_nz), 
  BIC = NA, 
  R2 =  lasso_cv_metrics[2][[1]], 
  RMSE_train = NA,
  RMSE_test =  lasso_cv_metrics[1][[1]] )

cv_result <- rbind(cv_result, lasso_metrics)


########################################
# PART III - DIAGNOSTICS
########################################



###################################################
# Diagnsotics #
###################################################
model3_level <- model_results_cv[["modellev3"]][["model_work_data"]]
model7_level <- model_results_cv[["modellev7"]][["model_work_data"]]


# look at holdout RMSE
model7_level_work_rmse <- mse_lev(predict(model7_level, newdata = data_work), data_work[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse <- mse_lev(predict(model7_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])


# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bg() 
level_vs_pred
save_fig("ch14-figure-8a-level-vs-pred", output, "small")


# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(n_accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
            conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= paste0(output, "modellev7_holdout_summary.tex"))


F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(n_accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (US dollars)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
F14_CI_n_accomodate
save_fig("ch14-figure-8b-ci-n-accomodate", output, "small")



#NOT USED
# Density chart (not in book)
g3 <- ggplot(data = datau, aes(x=price)) +
  geom_density(aes(color=f_room_type, fill=f_room_type),  na.rm =TRUE, alpha= 0.3) +
  labs(x="Price (US dollars)", y="Density", color = "") +
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3]),
                     labels=c("Entire home/apt","Private room", "Shared room")) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1], color[3]),
                    labels=c("Entire home/apt","Private room", "Shared room")) +
  theme_bg() 
  theme(legend.position = c(0.7,0.7),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "white"))
g3


# Barchart  (not in book)
plot4 <- ggplot(data = datau, aes(x = factor(n_accommodates), color = f_room_type, fill = f_room_type)) +
  geom_bar(alpha=0.8, na.rm=T, width = 0.8) +
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1],  color[3])) +
  labs(x = "Accomodates (Persons)",y = "Frequency")+
  theme_bg() 
  theme(legend.position = "bottom")
plot4




#ch14-table-1-levlog-pred
#ch14-table-3-fit-level
