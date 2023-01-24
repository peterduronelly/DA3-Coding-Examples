# CHAPTER 15
# version x 2023-01-14
# based on Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# and on modifications by Agoston Reguly (github.com/regulyagoston)
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CLEAR MEMORY
rm(list=ls())

# Descriptive statistics and regressions
library(caret)
library(tidyverse)
library(skimr)
library(ggthemes)
library(gridExtra)
library(lattice)
library(glmnet)
library(rpart) # recursive partitioning for classification & regression
library(rattle)
library(rpart.plot)
library(xtable)
library(Hmisc)
library(modelsummary)
library(scales)






#######################################################################################

# DATA IMPORT
data <- read_csv( "https://osf.io/7gvz9/download" )
# convert to factor
data <- data %>% mutate_if(is.character, factor)

glimpse( data )

# SAMPLE DESIGN

# manage missing
data$fuel <- fct_explicit_na(data$fuel, na_level = "Missing")
data$drive <- fct_explicit_na(data$drive, na_level = "Missing")
data$cylinders <- fct_explicit_na(data$cylinders, na_level = "Missing")
data$transmission <- fct_explicit_na(data$transmission, na_level = "Missing")
data$type <- fct_explicit_na(data$type, na_level = "Missing")


# missing changed to good not missing
# data$condition <- fct_explicit_na(data$condition, na_level = "Missing")
table(data$condition)
data$condition[is.na(data$condition)] <- "good"
table(data$condition       )

# same steps as in ch13, see code in ch13 for details
data <- data %>% filter(Hybrid ==0) %>% dplyr::select(-Hybrid)
data <- data %>% filter(fuel=="gas")
data <- data %>% filter(!condition %in% c("new", "fair"))
data <- data %>% filter(price %in% c(500:25000), odometer <=100)
data <- data %>% filter(!(price < 1000 & (condition == "like new"|age < 8)))
data <- data %>% filter(!(transmission == "manual"))
data <- data %>% filter(!type %in% c("truck", "pickup"))
data <- data %>% dplyr::select(-pricestr)



# to be on the safe side
data <- data %>% drop_na(price)


################################################################################
# DATA GENERATION & DESCRIPTIVES
################################################################################
# price  age   odometer + condition cylinder dealer city LE

# condition
data <- data %>%
  mutate(cond_excellent = ifelse(condition == "excellent", 1,0),
         cond_good = ifelse(condition == "good", 1,0),
         cond_likenew = ifelse(condition == "like new", 1,0))

# cylinders
data <- data %>%
  mutate(cylind6 = ifelse(cylinders=="6 cylinders",1,0))


#chicago
data$chicago <- ifelse(data$area=="chicago",1,0)

# age: quadratic, cubic
data <- data %>%
  mutate(agesq = age^2,
         agecu = age^3)

# odometer: quadratic
data <- data %>%
  mutate(odometersq = odometer^2)


datasummary_skim(data, 'numeric')
datasummary_skim(data, 'categorical')


Hmisc::describe(data$price)



################################################################################
# Create test and train samples #
################################################################################
# now all stuff runs on training vs test (holdout), alternative: 4-fold CV

# create test and train samples (70% of observations in train sample)
smp_size <- floor(0.7 * nrow(data))
set.seed(20230125)

train_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$train <- 0
data$train[train_ids] <- 1

# Create train and test sample variables
data_train <- data %>% filter(train == 1)
data_test <- data %>% filter(train == 0)


################################################################################
# MODELLING
################################################################################


# Regression tree (rpart)


summary(data_train$price)
summary(data_test$price)
# testing whether the splits are similar (H0: they are similar)
# note: don't expect any surprises here
t.test(data_train$price, data_test$price)
var.test(data_train$price, data_test$price, alternative = "two.sided") 
  # default is two-sided 


summary(data_train$age)
summary(data_test$age)


# SINGLE PREDICTOR: AGE
model1 <- formula(price ~ age)

# single split
# (make sure it's a single split by setting "maxdepth" to 1)

cart1 <- train(
  model1, 
  data = data_train, 
  method = "rpart2",
  trControl = trainControl(method="none"), #method = 'none': no resampling is applied
  tuneGrid= data.frame(maxdepth=1))

# summary & tree graph
summary(cart1$finalModel)
rpart.plot(cart1$finalModel, tweak=1.2, digits=-1, extra=1)

# goodness-of-fit
pred_cart1_test <- predict(cart1, data_test)
rmse_cart1 <- sqrt(mean((pred_cart1_test - data_test$price)^2))

# visualize results
# scatterplot with step function
plot_helper <- seq(min(data_train$age), max(data_train$age))
plot_helper_df <-data.frame(age=plot_helper)
plot_helper_df$xend <- c(plot_helper+1)
plot_helper_df$yend <- predict(cart1, plot_helper_df)
pred_cart1_train <- predict(cart1, data_train)

ggplot(
  data = data_train, 
  aes(x = age, y=price)) +
  geom_point(colour = '#333333') +
  geom_segment(
    data = plot_helper_df,  
    aes(x = age, y=yend, xend=xend, yend=yend), 
    color='black', size=1, na.rm=TRUE) +
  scale_y_continuous(
    expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500), 
    labels = comma_format(big.mark = ",")) +
  scale_x_continuous(
    expand=c(0.01,0.01),limits=c(0, 25), breaks=seq(0, 25, by=5) ) +
  labs(x = "Age (years)", y = "Price (US dollars)") +
  theme_bw() 


# splits at two levels
# (make sure it stops by setting "maxdepth" to 2)

cart2 <- train(
  model1, 
  data = data_train, 
  method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=2))

# get the final result's summary
s2 <-summary(cart2)
s2$frame
s2$frame %>% filter(var == '<leaf>')


tab_cart2 <- data.frame(
  "Category" = c("Age 1-4", "Age 5-7","Age 8-12","Age 13 or more"),
  "Count" = c(summary(cart2)$frame$n[7], summary(cart2)$frame$n[6], summary(cart2)$frame$n[4], summary(cart2)$frame$n[3]),
  "Average_price" = c(summary(cart2)$frame$yval[7], summary(cart2)$frame$yval[6], summary(cart2)$frame$yval[4], summary(cart2)$frame$yval[3])
  )

# goodness-of-fit
pred_cart2_test <- predict(cart2, data_test)
rmse_cart2 <- sqrt(mean((pred_cart2_test - data_test$price)^2))

# visualize results
# tree graph
rpart.plot(cart2$finalModel, tweak=1.2, digits=-1, extra=1)

# scatterplot with step function
plot_helper_df$yend <- predict(cart2, plot_helper_df)
pred_cart2_train <- predict(cart2, data_train)

ggplot(
  data = data_train, 
  aes(x = age, y=price)) +
  geom_point(colour = '#333333') +
  geom_segment(
    data = plot_helper_df,  
    aes(x = age, y=yend, xend=xend, yend=yend), 
    color='black', size=1, na.rm=TRUE) +
  scale_y_continuous(
    expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500), 
    labels = comma_format(big.mark = ",")) +
  scale_x_continuous(
    expand=c(0.01,0.01),limits=c(0, 25), breaks=seq(0, 25, by=5) ) +
  labs(x = "Age (years)", y = "Price (US dollars)") +
  theme_bw() 

# splits with  rpart defaults
# NB: typo in book, CART is with cp=0.01 not cp=0.001
cart3 <- train(
  model1, 
  data = data_train, 
  method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.01))

summary(cart3)
pred_cart3_test <- predict(cart3, data_test)
rmse_cart3 <- sqrt(mean((pred_cart3_test - data_test$price)^2))

# visualize
# tree graph
rpart.plot(cart3$finalModel, tweak=1.2, digits=-1, extra=1)

# scatterplot with step function - train data
plot_helper_df$yend <- predict(cart3, plot_helper_df)
pred_cart3_train <- predict(cart3, data_train)


ggplot(
  data = data_train, 
  aes(x = age, y=price)) +
  geom_point(colour = '#333333') +
  geom_segment(
    data = plot_helper_df,  
    aes(x = age, y=yend, xend=xend, yend=yend), 
    color='black', size=1, na.rm=TRUE) +
  scale_y_continuous(
    expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500), 
    labels = comma_format(big.mark = ",")) +
  scale_x_continuous(
    expand=c(0.01,0.01),limits=c(0, 25), breaks=seq(0, 25, by=5) ) +
  labs(x = "Age (years)", y = "Price (US dollars)") +
  theme_bw() 


# Competing models


# OLS: age only (linear regression)

linreg1 <- lm(model1 , data=data_train)
summary(linreg1)
pred_linreg1_test <- predict(linreg1, data_test)
rmse_linreg1 <- sqrt(mean((pred_linreg1_test - data_test$price)^2))

# scatterplot with predicted values
pred_linreg1_train <- predict(linreg1, data_train)

ggplot(data = data_train) +
  geom_point(
    aes(x = age, y = price), 
    color = '#663300', shape = 16, show.legend=FALSE, na.rm = TRUE) +
  geom_line(aes(x=age,y=pred_linreg1_train), colour='black', size = 1.2) +
  scale_y_continuous(
    expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500),
    labels = comma_format(big.mark = ",")) +
  scale_x_continuous(
    expand=c(0.01,0.01), limits=c(0, 25), breaks=seq(0, 25, by=5)) +
  labs(x = "Age (years)", y = "Price (US dollars)") +
  theme_bw() 


# lowess, age only

lowess1 <- loess(model1, data=data_train)
# no prediction with loess on test
pred_lowess1_test <- predict(lowess1, data_test)
rmse_lowess1 <- sqrt(mean((pred_lowess1_test - data_test$price)^2))

# scatterplot with predicted values
pred_lowess1_train <- predict(lowess1, data_train)

ggplot(data = data_train, aes(x=age , y=price)) +
  geom_point(
    aes(x = age, y = price), 
    color = '#663300', shape = 16, show.legend=FALSE, na.rm = TRUE) +
  geom_smooth(method="loess", colour="black", se=F, size=1.2) +
  labs(x = "Age", y = "Price") +
  coord_cartesian(xlim=c(0, 25), ylim=c(0, 20000)) +
  scale_y_continuous(
    expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500),
    labels = comma_format(big.mark = ",")) +
  scale_x_continuous(
    expand=c(0.01,0.01), limits=c(0, 25), breaks=seq(0, 25, by=5)) +
  theme_bw()



# MULTIPLE PREDICTOR VARIABLES


# ols  with multiple variables
model2 <- formula(price ~ age + odometer + LE + XLE + SE + cond_excellent + cond_good + cylind6 + dealer+chicago)
linreg2 <- lm(model2 , data=data_train)
summary(linreg2) # check out the 'Chicago' variable; remember what we saw on the t.test for the Chicago vs the LA subset?

pred_linreg2_test <- predict(linreg2, data_test, na.action = na.pass)
rmse_linreg2 <- sqrt(mean((pred_linreg2_test - data_test$price)^2))
rmse_linreg2


# add squared for age, odometer
model3 <- formula(price ~ age + agesq+ odometer+odometersq +LE + XLE + SE + cond_excellent + cond_good + cylind6 + dealer+chicago)
linreg3 <- lm(model3 , data=data_train)
summary(linreg3)
pred_linreg3_test <- predict(linreg3, data_test, na.action = na.pass)
rmse_linreg3 <- sqrt(mean((pred_linreg3_test - data_test$price)^2))
rmse_linreg3


# trees

# splits at four levels, for illustrative purposes
# (make sure it stops by setting "maxdepth" to 4)
cart4 <- train(
  model2, 
  data=data_train, 
  method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=4),
  na.action = na.pass)

summary(cart4) # too complex to read! 

# we only predict for the test set to calculate RMSE
pred_cart4_test <- predict(cart4, data_test, na.action = na.pass)
rmse_cart4 <- sqrt(mean((pred_cart4_test - data_test$price)^2))

# visualize
# tree graph
rpart.plot(cart4$finalModel, tweak=1.2, digits=-1, extra=1)


# instead of 'maxdepth' we control 'cp'
# the difference between method = 'rpart' & method = 'rpart2'
# is rpart tuning parameter is 'cp', that is rpart2 is 'maxdepth'
cart5 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.002),
  control = rpart.control(minsplit = 20),
  na.action = na.pass)

print(cart5)

summary(cart5)
pred_cart5_test <- predict(cart5, data_test, na.action = na.pass)
rmse_cart5 <- sqrt(mean((pred_cart5_test - data_test$price)^2))

# visualize
# tree graph
rpart.plot(cart5$finalModel, tweak=1.2, digits=-1, extra=1)


# PRUNING TREES

# build A very large tree

cart6 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.0001), # low cp value implies many splits! 
  control = rpart.control(minsplit = 4),
  na.action = na.pass)

# visualize
# tree graph
rpart.plot(cart6$finalModel, tweak=1.2, digits=-1, extra=1) # we have built a monster!!!

pred_cart6_test <- predict(cart6, data_test, na.action = na.pass)
rmse_cart6 <- sqrt(mean((pred_cart6_test - data_test$price)^2))

# take the last model (large tree) and prunce (cut back)
pfit <-prune(cart6$finalModel, cp=0.005 )
# determines a nested sequence of subtrees of the supplied rpart object by recursively snipping off
# the least important splits, based on the complexity parameter (cp).

summary(pfit)

# getting rmse
pred_cart7_test <- predict(pfit, data_test, na.action = na.pass)
rmse_cart7 <- sqrt(mean((pred_cart7_test - data_test$price)^2))
rmse_cart7

summary(pfit)

# visualize
# tree graph
rpart.plot(pfit, digits=-1, extra=1, tweak=1)

# the print() method displays the improvement with different pruning parameters
# if we want to stop at 0.005 improvement, we need to cut back the tree until it has 
# no more than 9 split actions (10 end nodes are the result of 9 splits)
printcp(pfit)


# PERFORMANCE SUMMARY

df_rmse <- data.frame(
  "Model" = c("CART1", "CART2","CART3","CART4", "CART5","CART6","CART7", "OLS multivar", "OLS extended"),
  "Describe" = c("2 term. nodes", "4 term. nodes","5 term. nodes","cp = 0.01","cp = 0.002","cp = 0.0001","pruned", "multi-var", "w/ squared vars"),
  "RMSE" = c(rmse_cart1, rmse_cart2, rmse_cart3, rmse_cart4,rmse_cart5,rmse_cart6,rmse_cart7, rmse_linreg2, rmse_linreg3)
)

arrange( df_rmse , RMSE )


# VARIABLE IMPORTANCE

cart4_var_imp <- varImp(cart4)$importance
arrange(cart4_var_imp, desc(Overall))

cart4_var_imp_df <- data.frame(
  varname = rownames(cart4_var_imp),
  imp = cart4_var_imp$Overall) %>%
    mutate(varname = gsub("cond_", "Condition:", varname) ) %>%
    arrange(desc(imp)) %>%
    mutate(imp_percentage = imp/sum(imp)
           )

ggplot(cart4_var_imp_df, 
       aes(x=reorder(varname, imp), 
           y=imp_percentage)) +
  geom_point(color='#990033', size=2) +
  geom_segment(
    aes(x=varname,xend=varname,y=0,yend=imp_percentage), 
    color='#990033', size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


## a note for varimp 

# https://topepo.github.io/caret/variable-importance.html
# Recursive Partitioning: The reduction in the loss function (e.g. mean squared error) 
#   attributed to each variable at each split is tabulated and the sum is returned. 
#   Also, since there may be candidate variables that are important but are not used in a split,
#   the top competing variables are also tabulated at each split. 
#   This can be turned off using the maxcompete argument in rpart.control.
# To avoid this, we can rerun cart4 with a new control fn to ensure matching 

# the 'maxcompete' argument: the number of competitor splits retained in the output. It is useful to know not
# just which split was chosen, but which variable came in second, third, etc.
  
cart4 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.01),
  control = rpart.control(minsplit = 20, maxcompete = FALSE),
  na.action = na.pass)

# maxcompete: the number of competitor splits retained in the output. It is useful to know not
# just which split was chosen, but which variable came in second, third, etc

cart4_var_imp <- varImp(cart4)$importance
cart4_var_imp_df <-
  data.frame(varname = rownames(cart4_var_imp),imp = cart4_var_imp$Overall) %>%
  mutate(varname = gsub("cond_", "Condition:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))
  
# a very 'skinny' graph about which variables are important in this myopic model
ggplot(cart4_var_imp_df, 
       aes(x=reorder(varname, imp), 
           y=imp_percentage)) +
  geom_point(color='#990033', size=2) +
  geom_segment(
    aes(x=varname,xend=varname,y=0,yend=imp_percentage), 
    color='#990033', size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  theme_bw()
