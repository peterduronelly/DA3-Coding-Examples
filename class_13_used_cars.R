# CHAPTER 13
# CH11 Used cars
# version x 2023-01-05
# based on Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# and on modifications by Agoston Reguly (github.com/regulyagoston)
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x


#### SET UP
# CLEAR MEMORY
rm(list=ls())


# import libraries
library(tidyverse)
library(lmtest)
library(sandwich)
library(haven)
library(stargazer)
library(caret)
library(grid)
library(modelsummary)
library(scales)


################################################################################

# DATA IMPORT
data <-  read.csv( 'https://osf.io/7gvz9/download', stringsAsFactors = TRUE)
glimpse(data)

# SAMPLE DESIGN
# before massaging our data let's see what we have

unique(data$area)
unique(data$subarea)
unique(data$fuel)

fct_count(data$fuel)
fct_explicit_na(data$fuel) # red NA actually means a missing value

fct_count(data$condition)
fct_count(data$type)

# manage missing
data$fuel <- fct_explicit_na(data$fuel, na_level = "Missing")
fct_explicit_na(data$fuel) # watch how (Missing) became "Missing"

data$condition <- fct_explicit_na(data$condition, na_level = "Missing")
data$drive <- fct_explicit_na(data$drive, na_level = "Missing")
data$cylinders <- fct_explicit_na(data$cylinders, na_level = "Missing")
data$transmission <- fct_explicit_na(data$transmission, na_level = "Missing")
data$type <- fct_explicit_na(data$type, na_level = "Missing")


# drop hybrid models then drop column
data <- data %>% filter(Hybrid ==0) %>% dplyr::select(-Hybrid)

# check frequency by fuel type
data %>%
  group_by(fuel) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

  # Note: according to the docs dplyr has a summarise() method only. 'Summarize'
  # may work but sometimes throws an error. 

# keep gas-fuelled vehicles
data <- data %>% filter(fuel=="gas")


# one way to analyze the target variable per categorical variables is the 'datasummary' method 
# of the modelsummary package; it provides a neat output 

datasummary(price ~ condition * mean, data = data)

datasummary( condition * price ~ Mean + SD  + Median + P0 + P25 + P50 + P75 + P100,  data = data )

# while this is a fancy method it has its limitations in terms of what you get out of the data
# we are better of using basic dplyr methods to see what's inside our data
# mean and std are usual metrics to analyze our data but they are the main picture
# if you want to see the (conditional) distributions in numbers use quantiles

# question: do you think condition is missing at the low end of the quality spectrum? 
# are they missing to hide crappyness? 

data %>% 
  group_by(condition) %>% 
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

# can we see unrealistic prices?

# can we see unrealistic odometer values?


data %>% 
  group_by(condition) %>% 
  summarise(
    frequency=n(),
    min = min(odometer),
    P1 = quantile(odometer, 0.01), 
    D1 = quantile(odometer, 0.1), 
    Q1 = quantile(odometer, 0.25), 
    Me = quantile(odometer, 0.5), 
    Q3 = quantile(odometer, 0.75), 
    D9 = quantile(odometer, 0.9), 
    P99 = quantile(odometer, 0.99),
    max = max(odometer))  

# drop vehicles in fair and new condition, trucks
data <- data %>% filter(!condition %in% c("new", "fair"))

# drop unrealistic values for price and odometer reading
data <- data %>% filter(price %in% c(500:25000), odometer <=100)

# drop if price is smaller than 1000 and condition is like new or age is less than 8
data <- data %>% filter(!(price < 1000 & (condition == "like new"|age < 8)))

# check frequency by transmission
data %>%
  group_by(transmission) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# remove obs w manual transmission,
data <- data %>% filter(!(transmission == "manual"))

data %>%
  group_by(type) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# drop if truck
data <- data %>% filter(!(type == "truck"))
 
# drop pricestr
data <- data %>% dplyr::select(-pricestr)

################################################################################

# DATA GENERATION & DESCRIPTIVES

# condition
data <- data %>%
  mutate(cond_excellent = ifelse(condition == "excellent", 1,0),
         cond_good = ifelse(condition == "good", 1,0),
         cond_likenew = ifelse(condition == "like new", 1,0))

# cylinders
data <- data %>%
  mutate(cylind6 = ifelse(cylinders=="6 cylinders",1,0))

table(data$cylinders)
table(data$cylind6)


# age: quadratic, cubic
data <- data %>%
  mutate(agesq = age^2,
         agecu = age^3)

# odometer: quadratic
data <- data %>%
  mutate(odometersq = odometer^2)


# Frequency tables

# area
data %>%
  group_by(area) %>%
  dplyr::summarize(frequency=n(), mean=mean(price))

# are prices in Chicago really higher than in LA? 

t.test(data %>% 
         filter(area == "chicago") %>% 
         select(price), 
       data %>% 
         filter(area == "los angeles") %>% 
         select(price)
       )

# focus only on Chicago
data <- data %>%
  filter(area=="chicago")

# condition
data %>%
  group_by(condition) %>%
  dplyr::summarize(frequency=n(), mean=mean(price))

# drive
data %>%
  group_by(drive) %>%
  dplyr::summarize(frequency=n(), mean=mean(price))

  # what do we think about rwd cars?

# dealer
data %>%
  group_by(dealer) %>%
  dplyr::summarize(frequency=n(), mean=mean(price))

# a little digression into dealers' pricing behavior
data %>% 
  group_by(condition, dealer) %>% 
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
  # what do you think about cars sold by dealers with missing condition?

# data summary
data %>%
  dplyr::select(age, odometer, LE, XLE, SE, cond_likenew, cond_excellent, cond_good, cylind6) %>%
    summary()

Hmisc::describe(data$age)
  # "Gmd" (Gini’s mean difference) is the mean absolute difference between any two distinct elements of a vector
  # this is a measure of dispersion, the computation of which no measure of central tendency is necessary
  
  # "Info" is related to how continuous the variable is. The lowest information comes from a variable having only 
  # one unique values following by a highly skewed binary variable. Info is reported to two decimal places. 
  # 24 distinct values info = 0.997 => a really continuous variable


Hmisc::describe(data$cond_likenew)
  # two distinct values, 15 1s and the rest 0: info = 0.152
Hmisc::describe(data$cylind6)
  # two distinct values, 41 1s and the rest is 0: info = 0.37

## PLOTS

# price

ggplot(data = data, aes(price)) + 
  geom_histogram(bins = 20, fill = 'black', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(title = "Price distribution of used cars", x = "price in USD", y = "frequency") +
  theme(plot.title = element_text(size = rel(1))) + 
  scale_x_continuous(expand = c(0.01,0.01),breaks = seq(0,20000, 2500))

# ln price
ggplot(data = data, aes(x = lnprice)) + 
  geom_histogram(
    aes(y = (..count..)/sum(..count..)),
    bins = 20, fill = 'black', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(
    title = "Price distribution of used cars", 
    x = "ln price in USD", y = "percent of total"
    ) +
  scale_y_continuous(
    expand = c(0.01,0.01),
    labels = scales::percent_format(accuracy = 0.1)
    ) +
  theme(plot.title = element_text(size = rel(1))) + 
  scale_x_continuous(expand = c(0.01,0.01),breaks = seq(6,10, 1))

# another way to plot log price is to convert the x-axis scale to logs and format it accordingly


ggplot(data = data, aes(price)) + 
  geom_histogram(bins = 20, fill = 'black', col = 'white', size = 0.25, alpha = 0.8) + 
  theme_bw() + 
  labs(title = "Price distribution of used cars", x = "price in USD (log scale)", y = "frequency") +
  theme(plot.title = element_text(size = rel(1))) + 
  scale_x_log10() + 
  annotation_logticks(sides = 'b')
 

###############################################################################
# REGRESSION ANALYSIS


# lowess
ggplot(data = data, aes(x=age, y=price)) +
  geom_point( color = "#330099", size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="loess", se=F, colour="#000000", size=1, span=0.9) +
  labs(x = "Age (years)",y = "Price (US dollars)") +
  theme_bw() +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,20000), breaks = seq(0,20000, 5000)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0,30), breaks = seq(0,30, 5))


# color guide: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#hexadecimal-color-code-chart


###################################
# Linear regressions

# Model 1: Linear regression on age
model1 <- as.formula(price ~ age + agesq)
# this is a 'formula' object:
class(model1)
# Models 2-5: Multiple linear regressions
# note: condition - missing will be baseline for regs

model2 <- as.formula(price ~ age + agesq + odometer)
model3 <- as.formula(price ~ age + agesq + odometer + odometersq + LE + cond_excellent + cond_good + dealer)
model4 <- as.formula(price ~ age + agesq + odometer + odometersq + LE + XLE + SE + cond_likenew +
                       cond_excellent + cond_good + cylind6 + dealer)

model5 <- as.formula(price ~ age + agesq + agecu + odometer + odometersq + LE*age + XLE*age + SE*age +
                       cond_likenew*age + cond_excellent*age + cond_good*age + cylind6*age + odometer*age + dealer*age)


reg1 <- lm(model1, data=data)
reg2 <- lm(model2, data=data)
reg3 <- lm(model3, data=data)
reg4 <- lm(model4, data=data)
reg5 <- lm(model5, data=data)

# reg1 is an 'lm' object with some useful attributes we can extract

reg1$coefficients
summary(reg1, vcov = 'sandwich')

logLik(reg1)

# Find AIC and BIC from the data
# official: AIC = 2k - 2*(max value of the likelihood function)
# R: AIC = 2(k+1) - 2*(max value of the likelihood function)

2*(reg1$rank+1) - 2*logLik(reg1)
AIC(reg1)


# BIC = k*ln(n) - 2*2*(max value of the likelihood function)
# same correction is required with rank
(reg1$rank+1)*log(nrow(data)) - 2*logLik(reg1)
BIC(reg1)

# evaluation of the models

models <- c("reg1", "reg2","reg3", "reg4", "reg5")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()

for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <- RMSE(predict(get(models[i])), get(models[i])$model$price)
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}



############################################################
# Linear regression evaluation

# Lowess vs. quadratic (reg1) regression
ggplot(data = data, aes(x=age)) +
  geom_smooth(aes(y=price, colour="black"), method="loess", se=F, size=1) +
  geom_line(aes(y=predict(reg1), colour="red"), size=1,lty=2) + 
  labs(x = "Age (years)",y = "Price (US dollars)") + 
  theme_bw() +
  scale_color_manual(name = "", values=c("black","red"),labels=c("Lowess in age","Quadratic in age")) + 
  scale_x_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  scale_y_continuous(limits = c(0,20000), breaks = seq(0,20000, 5000)) + 
  theme(legend.position = c(0.7,0.7),
        legend.direction = "horizontal",
         legend.background = element_blank(),
        legend.box.background = element_rect(color = "white"),
        legend.text = element_text(size = 10), 
        axis.title = element_text(size = 10))


# All models
eval <- data.frame(models, k, RSquared, RMSE, BIC)

# gsub(pattern, replacement, x) 

eval <- eval %>%
  mutate(models = paste0("(",gsub("reg","",models),")")) %>%
  rename(Model = models, "R-squared" = RSquared, "Training RMSE" = RMSE, "N predictors" = k)


#################################################################
# Cross-validation

# set number of folds
k <- 4

set.seed(13505)
cv1 <- train(model1, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv2 <- train(model2, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv3 <- train(model3, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv4 <- train(model4, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv5 <- train(model5, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")

# calculate average rmse
cv <- c("cv1", "cv2", "cv3", "cv4", "cv5")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                       get(cv[i])$resample[[1]][2]^2 +
                       get(cv[i])$resample[[1]][3]^2 +
                       get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
           rbind(cv1$resample[1], rmse_cv[1]),
           rbind(cv2$resample[1], rmse_cv[2]),
           rbind(cv3$resample[1], rmse_cv[3]),
           rbind(cv4$resample[1], rmse_cv[4]),
           rbind(cv5$resample[1], rmse_cv[5])
           )

colnames(cv_mat)<-c("Resample","Model1", "Model2", "Model3", "Model4", "Model5")
cv_mat


###############################################################################
# Prediction

data <- data %>% dplyr::select(age, agesq, odometer, odometersq, SE, LE, XLE, cond_likenew,
                        cond_excellent, cond_good, dealer,price, cylind6)

# Add new observation
new <- list(age=10, agesq=10^2,odometer=12,odometersq=12^2,SE=0,XLE=0, LE=1, 
            cond_likenew=0,cond_excellent=1,cond_good=0, 
            dealer=0, cylind6=0, price=NA)


# Predict price with all predictors (Model1)
reg1 <- lm(model1, data=data)

# Standard errors of residuals
p1 <- predict(reg1, data)
resid_p1 <- p1-data$price
summary(resid_p1)

# predict value for newly added obs
pred1_new <- predict(reg1, newdata = new,se.fit = TRUE, interval = "prediction")
p1<- pred1_new$fit


# Predict price with all predictors (Model3)
reg3 <- lm(model3, data=data)

# Standard errors of residuals
p3 <- predict(reg3, data)
resid_p3 <- p3-data$price
summary(resid_p3)

# predict value for newly added obs
pred3_new <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction")
p3<- pred3_new$fit
pred3_new 

#get model rmse
data$p3a <- predict(reg3, data)
rmse3 <- RMSE(data$p3a,data$price)
rmse3

# Result summary
sum1 <- cbind(t(p1), t(p3))
colnames(sum1) <- c('Model1', 'Model3')
rownames(sum1) <- c('Predicted', 'PI_low (95%)', 'PI_high (95%)')

sum1

# prediction


# summary of predictions and PI 80% version
# predict value for newly added obs
pred1_new80 <- predict(reg1, newdata = new, se.fit=TRUE, interval = "prediction", leve=0.8)
p180<- pred1_new80$fit
pred3_new80 <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction", level=0.8)
p380<- pred3_new80$fit

# Result summary
sum2 <- cbind(t(p180), t(p380))
colnames(sum2) <- c('Model1', 'Model3')
rownames(sum2) <- c('Predicted', 'PI_low (80%)', 'PI_high (80%)')
sum2
