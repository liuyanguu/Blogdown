# prepare dataset, and xgboost model for plotting

# Column Water Vapor correction - Modified script for correcting AOD measurements 
# The original script is 07_feature_selection_MOD1a.R, also included in this repo.
suppressPackageStartupMessages({
  library("data.table")
  library("xgboost")
  library("ggplot2")
  library("ggforce")
  library("ggExtra") # for ggMarginal
  library("gridExtra")
  library("here")
})

source(here("Code", "SHAP_funcs.R"))

date0 <- format(Sys.Date(), "%Y_%m_%d")

rferesults_Aqua <-  readRDS(here("Intermediate", "cwv_10by10_aqua_new_f9"))
y_var = "diffcwv"
dataXY_df <- rferesults_Aqua$dataXY_df
dataX <- dataXY_df[,-..y_var]
# hyperparameter tuning results
param_dart <- list(objective = "reg:linear",  # For regression
                   nrounds = 366,
                   eta = 0.018,
                   max_depth = 10,
                   gamma = 0.009,
                   subsample = 0.98,
                   colsample_bytree = 0.86)

xgb_mod <- rfe.fit(X = as.matrix(dataX), Y = as.matrix(dataXY_df[[y_var]]), xgb_param = param_dart)

# to get shap_long directly from model 
shap_values_mod <- shap.score.rank(xgb_model = xgb_mod, X_train = dataX)
pred_mod <- predict(xgb_mod, as.matrix(dataX))
shap_long_mod <- shap.long.data.prep(shap_values_mod, dataX)

# or if the SHAP values were already calculated during cross-validation process, as in my case:
shap_score <- rferesults_Aqua$shap_score
shap_values <- list(
  shap_score = shap_score,
  mean_shap_score = colMeans(abs(shap_score))[order(colMeans(abs(shap_score)), decreasing = T)]
)
shap_long <- shap.long.data.prep(shap_values, dataX)

# variable list 
var_list_a <- rferesults_Aqua$features_rank_full_model

