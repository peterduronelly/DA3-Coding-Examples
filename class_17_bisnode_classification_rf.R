# CHAPTER 17
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


library(glmnet)
library(margins)
library(skimr)
library(cowplot)
library(gmodels) 
library(modelsummary)
library(tidyverse)
library(viridis)
library(rattle)
library(caret)
library(pROC)
# install.packages("ROCR")
library(ROCR)
library(ranger)
library(rpart)
library(rpart.plot)
library(scales)


# source helper functions
# note: getwd() returns the R project home directory which may be different from 
# your actual directory (you can work in a different directory from wher your 
# R project is located). to get the actual directory of your current file use: 
#   dirname(rstudioapi::getSourceEditorContext()$path)

source(paste(dirname(rstudioapi::getSourceEditorContext()$path), 'utils.R', sep = '/'))

# these helper functions define plotting function + summary function for logit

#########################################################################################

# DATA IMPORT, EDA & FEATURES

data <- read_csv('https://raw.githubusercontent.com/peterduronelly/DA3-Coding-Examples/main/data/bisnode_firms_clean.csv')

# summary
skimr::skim(data)

# visual representation of the default ratio

ggplot( 
  data = data , 
  aes( x = default ) ) +
  geom_histogram( 
    aes( y = ..count.. / sum( count ) ) , 
    size = 0.1 , fill = 'black',bins = 3)+
  labs(y='Probabilities',x='0: Exists, 1: Defaults')+
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1), 
    limits = c(0,1)
  ) + 
  theme_bw()

# sales vs defaults

colors <- c('original datapoints'= 'black', 'linear model'='#990033', 'loess'= 'blue')

ggplot(
  data = data, 
  aes(x=sales_mil_log)) +
  geom_point(aes( y=as.numeric(default), color="original datapoints"),
    size=1,  shape=20, stroke=2, fill="black" ) +
  geom_smooth(aes(y=as.numeric(default), color= 'linear model'), 
    method = "lm", formula = y ~ poly(x,2), se = F, size=1.5)+
  geom_smooth(aes(y=as.numeric(default), color= 'loess'),
    method="loess", se=F, size=1.5, span=1) +
  labs(
    x = "sales_mil_log",y = "default", 
    color = '', title = 'Sales vs default probabilities') +
  scale_color_manual(values = colors) + 
  theme_bw()

# Finding a relationship between sales growth and default using original
#   and winsorized/clipped sales values

# original sales data

ggplot(
  data = data, 
  aes(x=d1_sales_mil_log, y=as.numeric(default))) +
  geom_point(
    size=0.1,  shape=20, stroke=2, fill='blue', color='black') +
  geom_smooth(
    method="loess", se=F, colour='#990033', size=1.5, span=0.9) +
  labs(
    x = "Growth rate (Diff of ln sales)",y = "default", 
    title = 'Annual growth in sales vs defaults') +
  theme_bw() +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-5,10, 5))

# winsorized/clipped sales data

ggplot(
  data = data, 
  aes(x=d1_sales_mil_log_mod, y=as.numeric(default))) +
  geom_point(
    size=0.1,  shape=20, stroke=2, fill='blue', color='black') +
  geom_smooth(
    method="loess", se=F, colour='#990033', size=1.5, span=0.9) +
  labs(
    x = "Growth rate (Diff of ln sales)",y = "default", 
    title = 'Annual growth in sales vs defaults - winsorized (clipped) data') +
  theme_bw() +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-5,10, 5))

# Where did we clip the data?

ggplot(data = data, aes(x=d1_sales_mil_log, y=d1_sales_mil_log_mod)) +
  geom_point(size=0.1,  shape=20, stroke=2, fill='blue', color='blue') +
  labs(x = "Growth rate (Diff of ln sales) (original)",y = "Growth rate (Diff of ln sales) (winsorized)") +
  theme_bw() +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5, 1)) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3, 1))


###############################################################################################
#
# MODELLING PREP

# define variable sets 
# note: we use the factor variable 'ind2_cat'

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", 
              "extra_profit_loss", "fixed_assets","inc_bef_tax", 
              "intang_assets", "inventories", "liq_assets", "material_exp", 
              "personnel_exp","profit_loss_year", "sales", "share_eq", "subscribed_cap")
qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear")
engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", 
            "curr_assets_bs","share_eq_bs", "subscribed_cap_bs", 
            "intang_assets_bs", "extra_exp_pl","extra_inc_pl", 
            "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")
engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE),
             grep("*flag_high$", names(data), value = TRUE),
             grep("*flag_error$", names(data), value = TRUE),
             grep("*flag_zero$", names(data), value = TRUE))
d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_sq",
         "flag_low_d1_sales_mil_log", "flag_high_d1_sales_mil_log")
hr <- c("female", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "labor_avg_mod",
        "flag_miss_labor_avg", "foreign_management")
firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")

# interactions for logit & LASSO

interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", 
                   "ind2_cat*labor_avg_mod")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", 
                   "sales_mil_log*foreign_management")

X1 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", 
        "profit_loss_year_pl", "ind2_cat")
X2 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", 
        "profit_loss_year_pl", "fixed_assets_bs","share_eq_bs",
        "curr_liab_bs ",   "curr_liab_bs_flag_high ", 
        "curr_liab_bs_flag_error",  "age","foreign_management" , "ind2_cat")
X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar,d1)
X4 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, 
        engvar3, d1, hr, qualityvars)
X5 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, 
        engvar3, d1, hr, qualityvars, interactions1, interactions2)

# for LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, 
               engvar3, d1, hr, firm, qualityvars, interactions1, interactions2)

# for RF (no interactions, no modified features)
rfvars  <-  c("sales_mil", "d1_sales_mil_log", rawvars, hr, firm, qualityvars)


###############################################################################################
#
# MODELLING

# using model X2
# linear probability

ols_modelx2 <- lm(formula(paste0("default ~", paste0(X2, collapse = " + "))),
                data = data)
summary(ols_modelx2)

# logit

glm_modelx2 <- glm(formula(paste0("default ~", paste0(X2, collapse = " + "))),
                   data = data, family = "binomial")
summary(glm_modelx2)

# logit marginal effects

mx2 <- margins(glm_modelx2, vce = "none")

glm_coeffs <- summary(glm_modelx2) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(mx2)[,c("factor","AME")])

glm_coeffs



# steps
# 1. Probabilities: logit & LASSO, compare CV RMSE
# 2. Classification
#   a) find the threshold value
#   b) calculate the matching confusion matrix, ROC & AUC
#   c) refresh calibration curve
# 3. Use a user-defined loss function
#   a) cross-validation of your loss function using the Youden-index
#   b) find the optimal threshold for your loss function
# 4. CART & random forest in two ways:
#   4.1 averaging terminal leaf probabilities
#   4.2 classification by majority vote

set.seed(20230208)

train_indices <- as.integer(createDataPartition(data$default, p = 0.8, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

Hmisc::describe(data$default_f)
Hmisc::describe(data_train$default_f)
Hmisc::describe(data_holdout$default_f)


# STEP I: PROBABILITIES
# logit models 

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
# note: the 'metric' argument for the **train()** function specifies what 
#   summary metric will be used to select the optimal model. By default, 
#   possible values are "RMSE" and "Rsquared" for regression and "Accuracy" 
#   and "Kappa" for classification. 
#   Custom performance metrics can be used via the 'summaryFunction' argument
#   of in 'trainControl()'. 

# all logit models 

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

# calculate metrics and store them in containers

logit_models <- list()
CV_RMSE_folds <- list()
CV_AUC_folds <- list()

# run models and get RMSE for each fold for each model

for (model_name in names(logit_model_vars)) {

  features <- logit_model_vars[[model_name]]
  
  print(paste0('Model: ', model_name, ', number of features: ', length(features)))
  
  set.seed(20230208)
  glm_model <- train(
    formula(paste0("default_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )

  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]
  print(paste0('CV RMSE: ', glm_model$resample$RMSE))
  print('  ')
}

# lasso 

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(20230208)
system.time({
  logit_lasso_model <- train(
    formula(paste0("default_f ~", paste0(logitvars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))

CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]

# calculate AUC for each model for each fold

for (model_name in names(logit_models)) {
  
  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    # get the prediction from each fold
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    # calculate the roc curve
    roc_obj <- roc(cv_fold$obs, cv_fold$default, quiet = TRUE)
    # save the AUC value
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }
  
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))
}


# STEP 2 CLASSIFICATION
# no loss function
# calibration plot, confusion matrix, ROC, AUC
# we are using logit model 'X4' for prediction metrics demo

best_logit_no_loss <- logit_models[["X4"]]

logit_predicted_probabilities_holdout <- predict(
  best_logit_no_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_no_loss_pred"] <- logit_predicted_probabilities_holdout[,"default"]
RMSE(data_holdout[, "best_logit_no_loss_pred", drop=TRUE], data_holdout$default)

# 2.1 calibration plot: how did we do in predicting the probabilities?

create_calibration_plot(
  data_holdout, 
  prob_var = "best_logit_no_loss_pred", 
  actual_var = "default",
  n_bins = 10)

# 2.2 confusion matrix
# default threshold: 0.5

logit_class_prediction <- predict(best_logit_no_loss, newdata = data_holdout)
summary(logit_class_prediction)

# positive = "yes": explicitly specify the positive case
cm_object_1a <- confusionMatrix(logit_class_prediction, as.factor(data_holdout$default_f), positive = "default")

cm_object_1a
cm_1a <- cm_object_1a$table


holdout_prediction <-
  ifelse(
    data_holdout$best_logit_no_loss_pred < 0.5, "no_default", "default"
    ) %>%
  factor(levels = c("no_default", "default"))

cm_object_1b <- confusionMatrix(holdout_prediction,as.factor(data_holdout$default_f))
cm_1b <- cm_object_1b$table


# a sensible choice for threshold is the mean of predicted probabilities

mean_predicted_default_prob <- mean(data_holdout$best_logit_no_loss_pred)
mean_predicted_default_prob
holdout_prediction <-
  ifelse(data_holdout$best_logit_no_loss_pred < mean_predicted_default_prob, "no_default", "default") %>%
  factor(levels = c("no_default", "default"))
cm_object_2 <- confusionMatrix(holdout_prediction,as.factor(data_holdout$default_f))
cm2 <- cm_object_2$table
cm2


# 2.3 plot ROC curve

# a) generate ROC curve by hand using calculated TPR & FPR values

thresholds <- seq(0.05, 0.75, by = 0.05)

cm <- list()
true_positive_rates <- c()
false_positive_rates <- c()
for (thr in thresholds) {
  # get holdout prediction
  holdout_prediction <- ifelse(data_holdout[,"best_logit_no_loss_pred"] < thr, "no_default", "default") %>%
    factor(levels = c("no_default", "default"))
  # create confusion matrix
  cm_thr <- confusionMatrix(holdout_prediction,as.factor(data_holdout$default_f))$table
  cm[[as.character(thr)]] <- cm_thr
  # Categorize to true positive/false positive
  true_positive_rates <- c(true_positive_rates, cm_thr["default", "default"] /
                             (cm_thr["default", "default"] + cm_thr["no_default", "default"]))
  false_positive_rates <- c(false_positive_rates, cm_thr["default", "no_default"] /
                              (cm_thr["default", "no_default"] + cm_thr["no_default", "no_default"]))
}

tpr_fpr_for_thresholds <- tibble(
  "threshold" = thresholds,
  "true_positive_rate"  = true_positive_rates,
  "false_positive_rate" = false_positive_rates
)

ggplot(
  data = tpr_fpr_for_thresholds,
  aes(x = false_positive_rate, y = true_positive_rate)) +
  labs(x = "False positive rate (1 - Specificity)", y = "True positive rate (Sensitivity)") +
  geom_point(size=2, alpha=0.8) +
  scale_x_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  theme_bw() +
  theme(legend.position ="right") +
  theme(legend.title = element_text(size = 4), 
        legend.text = element_text(size = 4),
        legend.key.size = unit(.4, "cm")) 

# b) autogenerate ROC curve using roc() function attributes

roc_obj_holdout <- roc(data_holdout$default, data_holdout$best_logit_no_loss_pred, quiet = T)

roc_df <- cbind(roc_obj_holdout$sensitivities, roc_obj_holdout$specificities) %>% 
  as.data.frame() %>%
  mutate(FPR = 1 - V2)

roc_df <- roc_df %>% rename('TPR' = 'V1', 'TNR' = 'V2')

print(paste0('Model AUC is ', round(roc_obj_holdout$auc, 3), '.'))

ggplot(data = roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color='black', size = 1) +
  xlab("False Positive Rate (1-Specifity)") +
  ylab("True Positive Rate (Sensitivity)") +
  labs(title = paste0('Model X4 ROC with AUC of ', round(roc_obj_holdout$auc, 3))) + 
  geom_abline(intercept = 0, slope = 1,  linetype = "dashed", size = 1, col = "black") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0, 0.01)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0.01, 0)) +
  theme_bw()

# 2.4 pick the best model based on average CV  RMSE and average AUC for 
#   we have 5 logit models + 1 lasso

CV_RMSE <- list()
CV_AUC <- list()

for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary_no_loss_function <- data.frame(
  "Number of predictors" = unlist(nvars),
  "CV RMSE" = unlist(CV_RMSE),
  "CV AUC" = unlist(CV_AUC))


# STEP 3: USER-DEFINED LOSS FUNCTION
# relative cost of of a false negative classification (as compared with a false positive classification)

FP=1
FN=10
cost = FN/FP

# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))

prevelance = sum(data_train$default)/length(data_train$default)

# ROC curve help us find the optimal threshold with regard to the loss function

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_cv_expected_loss <- list()

for (model_name in names(logit_models)) {
  
  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$default, quiet = TRUE)
    # add the weights (costs) here!
    best_treshold <- coords(roc_obj, "best", 
                            ret="all", 
                            transpose = FALSE,
                            best.method="youden", 
                            best.weights=c(cost, prevelance))
    # save best threshold for each fold and save the expected loss value
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$default)
  }

  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  expected_loss[[model_name]] <- mean(unlist(expected_loss_cv))

  # for fold #5
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  logit_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]

  }

logit_summary_with_loss_function <- data.frame(
      "Avg of optimal thresholds" = unlist(best_tresholds),
      "Threshold for Fold5" = sapply(logit_cv_threshold, function(x) {x$threshold}),
      "Avg expected loss" = unlist(expected_loss),
      "Expected loss for Fold5" = unlist(logit_cv_expected_loss))


# loss plot for model X4

r <- logit_cv_rocs[["X5"]]
best_coords <- logit_cv_threshold[["X5"]]

t <- best_coords$threshold[1]
sp <- best_coords$specificity[1]
se <- best_coords$sensitivity[1]
n <- rowSums(best_coords[c("tn", "tp", "fn", "fp")])[1]

all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
all_coords <- all_coords %>%
  mutate(loss = (fp*FP + fn*FN)/n)
l <- all_coords[all_coords$threshold == t, "loss"]

ggplot(
  data = all_coords, 
  aes(x = threshold, y = loss)) +
  geom_line(color='black', size=1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  geom_vline(xintercept = t , color = 'blue' ) +
  annotate(
    geom = "text", 
    x = t, y= min(all_coords$loss),
    label=paste0("best threshold: ", round(t,2)),
    colour='blue', angle=90, vjust = -1, hjust = -0.5, size = 5) +
  annotate(
    geom = "text", 
    x = t, y= l + 0.1,
    label= round(l, 2), hjust = -0.3, size = 5) +
  theme_bw()


best_logit_with_loss <- logit_models[["X4"]]
best_logit_optimal_treshold <- best_tresholds[["X4"]]

# Predict the probabilities on holdout
logit_predicted_probabilities_holdout      <- predict(best_logit_with_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_with_loss_pred"] <- logit_predicted_probabilities_holdout[,"default"]

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$default, data_holdout[, "best_logit_with_loss_pred", drop=TRUE],quiet = TRUE)

# Get expected loss on holdout:
holdout_treshold <- coords(roc_obj_holdout, x = best_logit_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)
# Calculate the expected loss on holdout sample
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$default)
expected_loss_holdout

# Confusion table on holdout with optimal threshold
holdout_prediction <-
  ifelse(data_holdout$best_logit_with_loss_pred < best_logit_optimal_treshold, "no_default", "default") %>%
  factor(levels = c("no_default", "default"))
cm_object3 <- confusionMatrix(holdout_prediction,as.factor(data_holdout$default_f))

cm_3 = cm_object3$table
cm_3


# STEP 4: CART & random forest

# 4.1 simple CART

data_for_graph <- data_train
levels(data_for_graph$default_f) <- list("stay" = "no_default", "exit" = "default")

set.seed(20230208)
rf_for_graph <- rpart(
    formula = default_f ~ sales_mil + profit_loss_year+ foreign_management,
    data = data_for_graph,
    control = rpart.control(cp = 0.0028, minbucket = 100)
  )

rpart.plot(rf_for_graph, tweak=1, digits=2, extra=107, under = TRUE)

# 4.2.1 random forest for probabilities
# split by Gini, average terminal leaf results for eahc obs over trees

train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE,
  verboseIter = TRUE
)

# optionally, you can go for grid sarch
tune_grid <- expand.grid(
  .mtry = 5, # c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = 15 # c(10, 15)
)

set.seed(20230208)
rf_model_p <- train(
  formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

rf_model_p$results

best_mtry <- rf_model_p$bestTune$mtry
best_min_node_size <- rf_model_p$bestTune$min.node.size

# add metrics (RMSE, AUC) to metric containers

CV_RMSE_folds[["rf_p"]] <- rf_model_p$resample[,c("Resample", "RMSE")]

auc <- list()
for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$default, quiet = TRUE)
  auc[[fold]] <- as.numeric(roc_obj$auc)
}
CV_AUC_folds[["rf_p"]] <- data.frame("Resample" = names(auc),
                                     "AUC" = unlist(auc))

CV_RMSE[["rf_p"]] <- mean(CV_RMSE_folds[["rf_p"]]$RMSE)
CV_AUC[["rf_p"]] <- mean(CV_AUC_folds[["rf_p"]]$AUC)

# use the loss function 

best_tresholds_cv <- list()
expected_loss_cv <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$default, quiet = TRUE)
  best_treshold <- coords(roc_obj, "best", 
                          ret="all", transpose = FALSE,
                          best.method="youden", 
                          best.weights=c(cost, prevelance))
  best_tresholds_cv[[fold]] <- best_treshold$threshold
  expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$default)
}

# average loss over the folds & the last fold

best_tresholds[["rf_p"]] <- mean(unlist(best_tresholds_cv))
expected_loss[["rf_p"]] <- mean(unlist(expected_loss_cv))

rf_summary <- data.frame("CV RMSE" = CV_RMSE[["rf_p"]],
                         "CV AUC" = CV_AUC[["rf_p"]],
                         "Avg of optimal thresholds" = best_tresholds[["rf_p"]],
                         "Threshold for Fold5" = best_treshold$threshold,
                         "Avg expected loss" = expected_loss[["rf_p"]],
                         "Expected loss for Fold5" = expected_loss_cv[[fold]])

rf_summary

# plot loss and roc for fold5 

createLossPlot(roc_obj, best_treshold, "rf_p_loss_plot")
createRocPlotWithOptimal(roc_obj, best_treshold, "rf_p_roc_plot")

# estimate RMSE, AUC & loss on the holdout data

rf_predicted_probabilities_holdout <- predict(rf_model_p, newdata = data_holdout, type = "prob")
data_holdout$rf_p_prediction <- rf_predicted_probabilities_holdout[,"default"]
RMSE(data_holdout$rf_p_prediction, data_holdout$default)

# ROC curve on holdout

roc_obj_holdout <- roc(data_holdout$default, data_holdout[, "rf_p_prediction", drop=TRUE], quiet=TRUE)

# AUC

as.numeric(roc_obj_holdout$auc)

# expected loss on holdout with optimal threshold

holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[["rf_p"]] , input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$default)
expected_loss_holdout


# summary results

nvars[["rf_p"]] <- length(rfvars)

summary_results <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE),
                              "CV AUC" = unlist(CV_AUC),
                              "CV threshold" = unlist(best_tresholds),
                              "CV expected Loss" = unlist(expected_loss))

model_names <- c("Logit X1", "Logit X4",
                 "Logit LASSO","RF probability")
summary_results <- summary_results %>%
  filter(rownames(.) %in% c("X1", "X4", "LASSO", "rf_p"))
rownames(summary_results) <- model_names

summary_results


# 4.2.2 Classification with random forest
# split by Gini, majority for each obs over trees
# each observation is classified as 0 or 1 depending which value has a majority 
# in all of the trees
#
# USE ONLY when wins & losses are symmetric in the classes

train_control <- trainControl(
  method = "cv",
  n = 5,
  verboseIter = TRUE
)

set.seed(20230208)
rf_model_f <- train(
  formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

# calculate expected loss by predicting on the holdout set using our loss function

data_holdout$rf_f_prediction_class <- predict(rf_model_f, newdata = data_holdout, type = "raw")

fp <- sum(data_holdout$rf_f_prediction_class == "default"    & data_holdout$default_f == "no_default")
fn <- sum(data_holdout$rf_f_prediction_class == "no_default" & data_holdout$default_f == "default")

(fp*FP + fn*FN)/length(data_holdout$default)
