# Chapter 16
# A simplified demo of random forest and gradient boosting machines
# with grid search using a toy data set.
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################


rm(list=ls())

library(tidyverse)
library(caret)
library(ranger)
library(modelsummary)
library(pdp)
library(rattle)
library(tables)
library(gbm)
library(ISLR)



# IMPORT & PARTITION DATA 

data <- as.data.frame(Hitters)

data <- data %>% filter(!is.na(Salary))

set.seed(20230201)

train_indices <- as.integer(createDataPartition(data$Salary, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]


# RANDOM FOREST

tune_grid <- expand.grid(
    .mtry = c(3, 4, 5),
    .splitrule = "variance",
    .min.node.size = c(6, 8, 10)
  )

train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

set.seed(20230201)

# we are running an rf model on Salary with all other variables as independent
rf_model <- train(formula(Salary ~ .),
              data = data_train,
              method = "ranger",
              trControl = train_control,
              tuneGrid = tune_grid,
              importance = "impurity"
)

rf_model$results
rf_model$bestTune

# plot varimp
plot(varImp(rf_model))

# partial dependence: Salary vs CHits
pdp_rf_model <- pdp::partial(rf_model, 
                          pred.var = "CHits", 
                          pred.grid = distinct_(data_holdout, "CHits"), 
                          train = data_train)
pdp_rf_model %>%
  autoplot( ) +
  geom_point(color='black', size=2) +
  geom_line(color='black', size=1) +
  ylab("Predicted Salary") +
  xlab("CHits") +
  labs(title = 'Partial dependence plot for hits') + 
  scale_x_continuous(limit=c(0,2750), breaks=seq(0,2750,500))+
  theme_bw()


# GBM

gbm_grid <-  expand.grid(interaction.depth = c(1,3,5), 
                         n.trees = c(100, 300, 500), 
                         shrinkage = 0.1, 
                         n.minobsinnode = c(6, 8, 10))

set.seed(20230201)
system.time({
  gbm_model <- train(formula(Salary ~ .),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})

gbm_model$finalModel
gbm_model$bestTune


# COMPARE DIGNOSTICS

final_models <-
  list("Random forest" = rf_model,
       "GBM"  = gbm_model)

results <- resamples(final_models) %>% summary()

results

df_CV_RMSE <- imap(
  final_models, 
  ~{mean(results$values[[paste0(.y,"~RMSE")]])}
) %>% 
  unlist() %>% 
  as.data.frame() %>%
  rename("CV RMSE" = ".")

df_holdout_RMSE <- map(
  final_models, 
  ~{RMSE(
    predict(.x, newdata = data_holdout), 
    data_holdout[["Salary"]]
  )}
) %>% 
  unlist() %>% 
  as.data.frame() %>%
  rename("Holdout RMSE" = ".")


# COMPARE ACTUAL VS PREDICTED ON THE HOLDOUT DATA

data_holdout$gbm_salary <- predict(gbm_model, data_holdout)
data_holdout$rf_salary <- predict(rf_model, data_holdout)


colors <- c('GBM'='#990033', 'RF'= 'blue')

ggplot( 
  data_holdout, aes(x = Salary)   ) +
  geom_point( 
    aes(y = gbm_salary, color = 'GBM' ), 
    size = 2 ) +
  geom_point( 
    aes(y = rf_salary, color = 'RF' ), 
    size = 2 ) +
  geom_abline( 
    intercept = 0, 
    slope = 1, 
    size = 1, 
    linetype = 'dashed'
  ) +
  scale_color_manual(breaks = c('GBM', 'RF'), values = colors) + 
  theme(legend.text=element_text(size=14)) + 
  xlim(-1,max(data_holdout$Salary))+
  ylim(-1,max(data_holdout$Salary))+
  labs(
    x='Actual salary (US$)',
    y='Predicted salary (US$)', 
    color = 'models',
    title = "Predicted vs actual salary - GBM & RF")+
  theme_bw()
