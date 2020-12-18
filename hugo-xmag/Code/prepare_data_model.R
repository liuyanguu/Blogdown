
# Sample 2. Model A --------------------------------------------------
d <- data.table::as.data.table(cbind(Fever = c(0,0,1,1), Cough = c(0,1,0,1), y = c(0,0,0,80)))
knitr::kable(d)
X1 = as.matrix(d[,.(Fever, Cough)])
X2 = as.matrix(d[,.(Cough, Fever)])
m1 = xgboost(
  data = X1, label = d$y,base_score = 0, gamma = 0, eta = 1, lambda = 0,nrounds = 1,objective = "reg:linear", verbose = F)
m2 = xgboost(
  data = X2, label = d$y,base_score = 0, gamma = 0, eta = 1, lambda = 0,nrounds = 1,objective = "reg:linear", verbose = F)
xgb.importance(model = m1)
xgb.importance(model = m2)


shap_m1 <- shap.values(m1, X_train = X1)
names(shap_m1$shap_score) <- paste0("SHAP.", names(shap_m1$shap_score))
output_simple1 <- cbind(x = X1, y.actual = d$y, y.pred = predict(m1, X1), shap_m1$shap_score, BIAS = shap_m1$BIAS0)
output_simple1

shap_m2 <- shap.values(m2, X_train = X2)
names(shap_m2$shap_score) <- paste0("SHAP.", names(shap_m2$shap_score))
output_simple2 <- cbind(x = X2, y.actual = d$y, y.pred = predict(m2, X2), shap_m2$shap_score, BIAS = shap_m2$BIAS0)
output_simple2
