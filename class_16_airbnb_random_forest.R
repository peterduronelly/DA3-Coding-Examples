# CHAPTER 16
# version x 2023-01-29
# based on Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# and on modifications by Agoston Reguly (github.com/regulyagoston)
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################


# CLEAR MEMORY
rm(list=ls())


library(tidyverse)
library(caret)
# Random forest package
library(ranger)
library(modelsummary)
# install.packages("pdp")
library(pdp)
# Gradient boosting machine
library(gbm)
# Pretty plots
library(rattle)
library(tables)




#########################################################################################

# DATA IMPORT, EDA & FEATURES

# Used area
area <- "london"
url <- 'https://raw.githubusercontent.com/peterduronelly/DA3-Coding-Examples/main/data/airbnb_london_workfile_adj_book.csv'
data <- read_csv(url) %>%
  mutate_if(is.character, factor) %>%
  filter(!is.na(price))


# We focus on normal apartments, n<8
data <- data %>% filter(n_accommodates < 8)



# copy a variable - purpose later, see at variable importance
data <- data %>% mutate(n_accommodates_copy = n_accommodates)

# basic descr stat -------------------------------------------
skimr::skim(data)

data %>% 
  summarise(
    frequency=n(),
    min = min(price),
    P1 = quantile(price, 0.01), 
    D1 = quantile(price, 0.1), 
    Q1 = quantile(price, 0.25), 
    Me = quantile(price, 0.5), 
    Q3 = quantile(price, 0.75), 
    D9 = quantile(price, 0.9), 
    P99 = quantile(price, 0.99),
    max = max(price))  

Hmisc::describe(data$price)

datasummary( f_room_type * f_property_type  ~ Percent() , data = data )
datasummary( f_room_type + f_property_type  ~ Percent() , data = data )

# create train and holdout samples 
# train is where we do it all, incl CV

set.seed(20230201)

train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

# Check the number of observations
dim(data_train)
dim(data_holdout)


#########################################################################################

# DEFINE MODELS: FROM SIMPLE TO EXTENDED

# Basic Variables incl. neighborhood
basic_vars <- c(
  "n_accommodates", "n_beds", "n_days_since",
  "f_property_type","f_room_type", "n_bathrooms", "f_cancellation_policy", "f_bed_type",
  "f_neighbourhood_cleansed")

# reviews
reviews <- c("n_number_of_reviews", "flag_n_number_of_reviews" ,
             "n_review_scores_rating", "flag_review_scores_rating")

# dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)

# interactions for the LASSO
# as seen in Chapter 14 EDA
X1  <- c("n_accommodates*f_property_type",  "f_room_type*f_property_type",  "f_room_type*d_familykidfriendly",
         "d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type")
# with boroughs
X2  <- c("f_property_type*f_neighbourhood_cleansed", "f_room_type*f_neighbourhood_cleansed",
         "n_accommodates*f_neighbourhood_cleansed" )


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, amenities)
predictors_E <- c(basic_vars, reviews, amenities, X1,X2)


#########################################################################################

# RANDOM FORESTS 

# do 5-fold CV

train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# simpler model - random forest

# set tuning

tune_grid <- expand.grid(
  .mtry = c(8),
  .splitrule = "variance",
  .min.node.size = c(50)
)
# note
#   mtry: number of variables to possibly split at in each node
#   splitrule: the default splitting rule during random forests tree building consists
#           of selecting, out of all splits of the (randomly selected) candidate variables, 
#           the split that minimizes the Gini impurity (in the case of classification) 
#           and the SSE (in case of regression); 
#           other options for regressions: "extratrees", "maxstat" or "beta" with default "variance"
#   see more here: https://bradleyboehmke.github.io/HOML


# run model

set.seed(1234)
system.time({
rf_model_1 <- train(
  formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity"
)
})
rf_model_1

# more complicated model with the same tuning parameters

set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2


# evaluate results

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  )
)

# note: the 'resamples' function provides methods for collection, analyzing and 
#   visualizing a set of resampling results from a common data set

summary(results)

# model 2 with an expanded grid - will take forever to run
# tune_grid <- expand.grid(
#   .mtry = c(8, 10, 12),
#   .splitrule = "variance",
#   .min.node.size = c(5, 10, 15)
# )
# 
# set.seed(1234)
# system.time({
# rf_model_2 <- train(
#   formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
#   data = data_train,
#   method = "ranger",
#   trControl = train_control,
#   tuneGrid = tune_grid,
#   importance = "impurity"
# )
# })
# 
# rf_model_2

# auto tuning - takes even longer
# set.seed(1234)
# system.time({
#   rf_model_2auto <- train(
#     formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
#     data = data_train,
#     method = "ranger",
#     trControl = train_control,
#     importance = "impurity"
#   )
# })
# rf_model_2auto 



#########################################################################################
#
# DIAGNOSTICS 

# Variable Importance Plots 

# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


# variable importance plot
# 1) full varimp plot, full
# 2) varimp plot grouped
# 3) varimp plot , top 10
# 4) varimp plot  w copy, top 10

rf_model_2_var_imp <- ranger::importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_2_var_imp_df

# quick look

plot(varImp(rf_model_2))

# only above a cutoff

cutoff = 600

ggplot(
    rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
    aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='black', size=1.5) +
  geom_segment(
    aes(x=varname,xend=varname,y=0,yend=imp_percentage), 
    color='black', size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=8), axis.title.y = element_text(size=8))



# full varimp plot, top 10 only

ggplot(
    rf_model_2_var_imp_df[1:10,], 
    aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='black', size=3) +
  geom_segment(
    aes(x=varname,xend=varname,y=0,yend=imp_percentage), 
    color='black', size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  labs(title = 'Simple variable importance plots') + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


# grouped varimp plot:
#   keep binaries created off factors together

# UDF
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}

varnames <- rf_model_2$finalModel$xNames
f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_cancellation_policy_varnames <- grep("f_cancellation_policy",varnames, value = TRUE)
f_bed_type_varnames <- grep("f_bed_type",varnames, value = TRUE)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
               f_cancellation_policy = f_cancellation_policy_varnames,
               f_bed_type = f_bed_type_varnames,
               f_property_type = f_property_type_varnames,
               f_room_type = f_room_type_varnames,
               f_bathroom = "f_bathroom",
               n_days_since = "n_days_since",
               n_accommodates = "n_accommodates",
               n_beds = "n_beds")

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(
  varname = rownames(rf_model_2_var_imp_grouped),
  imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

ggplot(
    rf_model_2_var_imp_grouped_df, 
    aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='black', size=3) +
  geom_segment(
    aes(x=varname,xend=varname,y=0,yend=imp_percentage), 
    color='black', size=1) +
  ylab("Importance (Percent)") +   
  xlab("Variable Name") +
  labs(title = 'Grouped variable importance plots') + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


# PDP: partial dependence plots 

# 1) Number of accommodates
pdp_n_acc <- pdp::partial(rf_model_2, 
                          pred.var = "n_accommodates", 
                          pred.grid = distinct_(data_holdout, "n_accommodates"), 
                          train = data_train)

pdp_n_acc %>%
  autoplot( ) +
  geom_point(color='black', size=2) +
  geom_line(color='black', size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  labs(title = 'Partial dependence plot for # of accomodates') + 
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_bw()

# note: autoplot() is a generic function to visualize various data object. It tries to 
#   give better default graphics and customized choices for each data type, quick and 
#   convenient to explore your genomic data compare to low level ggplot method. It is much 
#   simpler and easy to produce fairly complicate graphics, though you may lose some 
#   flexibility for each layer.

# 2) room type
pdp_n_roomtype <- pdp::partial(rf_model_2, pred.var = "f_room_type", 
                               pred.grid = distinct_(data_holdout, "f_room_type"), 
                               train = data_train)

pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color='black', size=4) +
  ylab("Predicted price") +
  xlab("Room type") +
  labs(title = 'PDP for room type') + 
  scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
  theme_bw()


# Out-of-sample performance: RMSE # mean(price) on the holdout set

# cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))


# summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood_cleansed %in% c(
    "Westminster", "Camden", "Kensington and Chelsea", "Tower Hamlets", "Hackney", "Newham")) %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("Apartment", "House")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Borough", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

result_3

#########################################################################################
#
# COMPETING MODELS

# OLS with dummies for area
# using model B

set.seed(1234)
system.time({
ols_model <- train(
  formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
  data = data_train,
  method = "lm",
  trControl = train_control
)
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))

# LASSO
# using extended model w interactions

set.seed(1234)
system.time({
lasso_model <- train(
  formula(paste0("price ~", paste0(predictors_E, collapse = " + "))),
  data = data_train,
  method = "glmnet",
  preProcess = c("center", "scale"),
  tuneGrid =  expand.grid(
    "alpha" = 1, 
    "lambda" = seq(0.01, 0.25, by = 0.01)),
  trControl = train_control
)
})

# choose your version! 
lasso_coeffs <- coef(
    lasso_model$finalModel,
    lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`)  # watch out for your column name! 

# lasso_coeffs <- coef(
#   lasso_model$finalModel,
#   lasso_model$bestTune$lambda) %>%
#   as.matrix() %>%
#   as.data.frame() %>%
#   rownames_to_column(var = "variable") %>%
#   rename(lasso_coefficient = `s1`)  # watch out for your column name! 

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

regression_coeffs <- merge(ols_model_coeffs_df, lasso_coeffs_non_null, by = "variable", all=TRUE)

# CART with built-in pruning
set.seed(1234)
system.time({
cart_model <- train(
  formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
  data = data_train,
  method = "rpart",
  tuneLength = 10,
  trControl = train_control
)
})
# note: tuneLength: an integer denoting the amount of granularity in the tuning parameter grid.
#   By default, this argument is the number of levels for each tuning parameters
#   that should be generated by train.

cart_model

fancyRpartPlot(cart_model$finalModel, sub = "")
# note: a wrapper for plotting rpart trees using prp


# GBM: Gradient Boosting Machine

# a complicated tuneGrid with many options takes forever to run; you are, however,
# welcome to experiment with these options
# gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), 
#                          n.trees = (4:10)*50, 
#                          shrinkage = 0.1, 
#                          n.minobsinnode = 20 
# )

gbm_grid <-  expand.grid(interaction.depth = 5, 
                         n.trees = 250, 
                         shrinkage = 0.1, 
                         n.minobsinnode = 20 
)
# interaction.depth: Integer specifying the maximum depth of each tree (i.e., the highest level of
#   variable interactions allowed). A value of 1 implies an additive model, a value
#   of 2 implies a model with up to 2-way interactions, etc. Default is 1.
# n.trees: Integer specifying the total number of trees to fit. This is 
#   equivalent to the number of iterations and the number of basis functions in the additive expansion.
#   Default is 100.
# shrinkage: A shrinkage parameter applied to each tree in the expansion. Also known as
#   the learning rate or step-size reduction; 0.001 to 0.1 usually work, but a smaller
#   learning rate typically requires more trees. Default is 0.1.
# n.minobisnode: Integer specifying the minimum number of observations in the terminal nodes
#   of the trees. Note that this is the actual number of observations, not the total
#   weight

set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})

gbm_model
gbm_model$finalModel


# COMPARE RESULTS

final_models <-
  list("OLS" = ols_model,
       "LASSO (model w/ interactions)" = lasso_model,
       "CART" = cart_model,
       "Random forest 1: smaller model" = rf_model_1,
       "Random forest 2: extended model" = rf_model_2,
       "GBM"  = gbm_model)

results <- resamples(final_models) %>% summary()


# model evaluation based on  CV RMSE

result_4 <- imap(
  final_models, 
  ~{mean(results$values[[paste0(.y,"~RMSE")]])}
  ) %>% 
  unlist() %>% 
  as.data.frame() %>%
  rename("CV RMSE" = ".")
# note: imap(): apply a function to each element of a vector, and its index
# for instance: mean(results$values[["OLS~RMSE"]])

# model evaluation based on holdout set RMSE

result_5 <- map(
  final_models, 
  ~{RMSE(
      predict(.x, newdata = data_holdout), 
      data_holdout[["price"]]
      )}
  ) %>% 
  unlist() %>% 
  as.data.frame() %>%
  rename("Holdout RMSE" = ".")
# note: map(): apply a function to each element of a vector

result_5

