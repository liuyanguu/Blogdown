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
if (!dir.exists(here("Figure/"))) dir.create(here("Figure/"))
y_var <-  "diffcwv"
date0 <- format(Sys.Date(), "%y_%m_%d")

rferesults_terra <- readRDS(here("Intermediate/cwv_10by10_terra_new_f9"))
dataXY_df <- rferesults_terra$dataXY_df
fwrite(dataXY_df, here("Intermediate/terradataXY.csv"))

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
pred_mod <- predict(xgb_mod, as.matrix(dataX))

# to get shap_long directly from model 
shap_values_mod <- shap.score.rank(xgb_model = xgb_mod, X_train = dataX)
shap_long_mod <- shap.long.data.prep(shap_values_mod, dataX)

# or if the SHAP values were already calculated during cross-validation process, as in my case:
shap_score <- rferesults_terra$shap_score
shap_values <- list(
  shap_score = shap_score,
  mean_shap_score = colMeans(abs(shap_score))[order(colMeans(abs(shap_score)), decreasing = T)]
)
shap_long <- shap.long.data.prep(shap_values, dataX) # the long data from the paper

# variable list 
var_list_a <- rferesults_terra$features_rank_full_model


# Simple sample: Model A --------------------------------------------------

# model A
d <- as.data.table(cbind(Fever = c(0,0,1,1),
                         Cough = c(0,1,0,1),
                         y = c(0,0,0,80)
))

X1 = as.matrix(d[,.(Fever, Cough)])
X2 = as.matrix(d[,.(Cough, Fever)])

set.seed(1234)
m1 = xgboost(
  data = X1, label = d$y,base_score = 0, gamma = 0, eta = 1, lambda = 0,nrounds = 1,objective = "reg:linear", verbose = F)
shap_m <- shap.score.rank(m1, X1)
shap_long_m <- shap.long.data.prep(shap_m, X1)
names(shap_m$shap_score) <- paste0("SHAP.", names(shap_m$shap_score))
output_simple <- cbind(x = X1, y.actual = d$y, y.pred = predict(m1, X1), shap_m$shap_score, BIAS = shap_m$BIAS0)



