
# Sample 2. Model A --------------------------------------------------
d <- data.table::as.data.table(cbind(Fever = c(0,0,1,1),
                         Cough = c(0,1,0,1),
                         y = c(0,0,0,80)
))
X1 = as.matrix(d[,.(Fever, Cough)])
m1 = xgboost::xgboost(
  data = X1, label = d$y,base_score = 0, gamma = 0, eta = 1, lambda = 0, nrounds = 1, verbose = F)
shap_m <- shap.values(m1, X_train = X1)
names(shap_m$shap_score) <- paste0("SHAP.", names(shap_m$shap_score))
output_simple <- cbind(x = X1, y.actual = d$y, y.pred = predict(m1, X1), shap_m$shap_score, BIAS = shap_m$BIAS0)


