# prepare dataset, and xgboost model for plotting

suppressPackageStartupMessages({
  library("SHAPforxgboost")
  # which requires:
  # library("data.table")
  # library("xgboost")
  # library("ggplot2")
  # library("ggforce")
  # library("ggExtra") # for ggMarginal
  # library("gridExtra")
  library("here")
})


# Sample 1. Satellite data ------------------------------------------------
y_var <-  "diffcwv"
date0 <- format(Sys.Date(), "%y_%m_%d")
dataX <- dataXY_df[,-..y_var]
# hyperparameter tuning results
param_dart <- list(objective = "reg:linear",  # For regression
                   nrounds = 366,
                   eta = 0.018,
                   max_depth = 10,
                   gamma = 0.009,
                   subsample = 0.98,
                   colsample_bytree = 0.86)

xgb_mod <- xgboost.fit(X = as.matrix(dataX), Y = as.matrix(dataXY_df[[y_var]]), xgb_param = param_dart)
pred_mod <- predict(xgb_mod, as.matrix(dataX))

# to get shap_long directly from model
shap_values_mod <- shap.values(xgb_model = xgb_mod, X_train = dataX)
shap_long_mod <- shap.prep(shap_values_mod, dataX)

# or if the SHAP values were already calculated during cross-validation process, as in my case:
shap_values <- list(
  shap_score = shap_score,
  mean_shap_score = colMeans(abs(shap_score))[order(colMeans(abs(shap_score)), decreasing = T)]
)
shap_long <- shap.prep(shap_values, dataX) # the long data from the paper


# Sample 2. Model A --------------------------------------------------

d <- as.data.table(cbind(Fever = c(0,0,1,1),
                         Cough = c(0,1,0,1),
                         y = c(0,0,0,80)
))
X1 = as.matrix(d[,.(Fever, Cough)])
m1 = xgboost::xgboost(
  data = X1, label = d$y,base_score = 0, gamma = 0, eta = 1, lambda = 0, nrounds = 1, verbose = F)
shap_m <- shap.values(m1, X1)
shap_long_m <- shap.prep(shap_m, X1)
names(shap_m$shap_score) <- paste0("SHAP.", names(shap_m$shap_score))
output_simple <- cbind(x = X1, y.actual = d$y, y.pred = predict(m1, X1), shap_m$shap_score, BIAS = shap_m$BIAS0)


