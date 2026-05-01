# =============================================================================
# scripts/02_baseline_ml.R
# Modèles ML classiques : Stepwise, LASSO, Elastic Net, CART, Ctree, RF, GBM
# -----------------------------------------------------------------------------
# Auteure  : Sandra Desmair Fogang Lontouo
# Projet   : Prédiction du flux de cyclistes BIXI — Montréal 2019
# Sortie   : outputs/resultats_baseline_ml.csv, outputs/best_gbm_model.rds
# Prérequis: packages.R, R/preprocess.R, R/evaluate.R
# Note     : RF et GBM (grilles complètes) peuvent prendre plusieurs heures
# =============================================================================

source("packages.R")
source("R/preprocess.R")
source("R/evaluate.R")

# =============================================================================
# 0. Préparation des données
# =============================================================================
data <- prepare_data("data/raw/BIXI.RData")
train      <- data$train
validation <- data$validation
test       <- data$test

nb_dep_train <- train$nb_departure
nb_dep_val   <- validation$nb_departure
nb_dep_test  <- test$nb_departure

# Scénario avec cluster géographique (meilleur RMSE validation)
# lat/long exclus — réservés à la partie spatiale (script 03)
train_cluster <- dplyr::select(train,      -location)
val_cluster   <- dplyr::select(validation, -location)
test_cluster  <- dplyr::select(test,       -location)

# Scénario sans variables spatiales — pour Stepwise et Ctree
train_base <- dplyr::select(train,      -location, -latitude, -longitude, -clusters_geo)
val_base   <- dplyr::select(validation, -location, -latitude, -longitude, -clusters_geo)
test_base  <- dplyr::select(test,       -location, -latitude, -longitude, -clusters_geo)

resultats <- list()

# Matrice X pour les modèles pénalisés (LASSO, Elastic Net)
make_X <- function(df) {
  model.matrix(log(nb_departure) ~ ., data = df)[, -1]
}

X_train <- make_X(train_cluster)
X_val   <- make_X(val_cluster)
X_test  <- make_X(test_cluster)
y_log   <- log(nb_dep_train)

n_cores <- parallel::detectCores() - 1

# =============================================================================
# 1. Stepwise AIC — régression linéaire avec sélection de variables
# =============================================================================
lm_full     <- glm(log(nb_departure) ~ ., data = train_base, family = gaussian)
lm_step_aic <- MASS::stepAIC(lm_full, direction = "both", trace = FALSE)

pred_train_step <- exp(predict(lm_step_aic, newdata = train_base))
pred_val_step   <- exp(predict(lm_step_aic, newdata = val_base))
pred_test_step  <- exp(predict(lm_step_aic, newdata = test_base))

resultats[["Stepwise"]] <- performance_summary(
  "Stepwise AIC",
  nb_dep_train, pred_train_step,
  nb_dep_val,   pred_val_step,
  nb_dep_test,  pred_test_step
)

message("Stepwise AIC termine.")

# =============================================================================
# 2. LASSO (validation croisee 10-fold, lambda.1se)
# =============================================================================
set.seed(123)
lasso_cv <- glmnet::cv.glmnet(x = X_train, y = y_log,
                               alpha = 1, nfolds = 10)

pred_train_lasso <- as.vector(exp(predict(lasso_cv, X_train, s = "lambda.1se")))
pred_val_lasso   <- as.vector(exp(predict(lasso_cv, X_val,   s = "lambda.1se")))
pred_test_lasso  <- as.vector(exp(predict(lasso_cv, X_test,  s = "lambda.1se")))

resultats[["LASSO"]] <- performance_summary(
  "LASSO",
  nb_dep_train, pred_train_lasso,
  nb_dep_val,   pred_val_lasso,
  nb_dep_test,  pred_test_lasso
)

message("LASSO termine.")

# =============================================================================
# 3. Elastic Net (alpha selectionne par validation)
# =============================================================================
alphas    <- seq(0.1, 0.9, by = 0.1)
rmse_vals <- sapply(alphas, function(a) {
  m <- glmnet::cv.glmnet(X_train, y_log, alpha = a, nfolds = 10)
  rmse(nb_dep_val,
       as.vector(exp(predict(m, X_val, s = "lambda.1se"))))
})

best_alpha <- alphas[which.min(rmse_vals)]
elastic_cv <- glmnet::cv.glmnet(X_train, y_log,
                                 alpha = best_alpha, nfolds = 10)

pred_train_el <- as.vector(exp(predict(elastic_cv, X_train, s = "lambda.1se")))
pred_val_el   <- as.vector(exp(predict(elastic_cv, X_val,   s = "lambda.1se")))
pred_test_el  <- as.vector(exp(predict(elastic_cv, X_test,  s = "lambda.1se")))

resultats[["Elastic Net"]] <- performance_summary(
  paste0("Elastic Net (alpha=", best_alpha, ")"),
  nb_dep_train, pred_train_el,
  nb_dep_val,   pred_val_el,
  nb_dep_test,  pred_test_el
)

message("Elastic Net termine. Meilleur alpha : ", best_alpha)

# =============================================================================
# 4. CART — grille minsplit x minbucket x maxdepth (cp optimal par validation)
# =============================================================================
grid_cart    <- expand.grid(
  minsplit  = c(5, 10, 20),
  minbucket = c(1, 5, 10),
  maxdepth  = c(5, 10, 15)
)
results_cart <- data.frame()

for (i in seq_len(nrow(grid_cart))) {
  tree_tmp <- rpart::rpart(
    nb_departure ~ ., data = train_cluster,
    control = rpart::rpart.control(
      cp        = 0,
      minsplit  = grid_cart$minsplit[i],
      minbucket = grid_cart$minbucket[i],
      maxdepth  = grid_cart$maxdepth[i]
    )
  )
  cp_vals  <- tree_tmp$cptable[, "CP"]
  best_cp  <- cp_vals[which.min(
    sapply(cp_vals, function(cp) {
      p <- predict(rpart::prune(tree_tmp, cp = cp), val_cluster)
      rmse(nb_dep_val, p)
    })
  )]
  pruned   <- rpart::prune(tree_tmp, cp = best_cp)
  results_cart <- rbind(results_cart,
                        data.frame(grid_cart[i, ],
                                   val_rmse = rmse(nb_dep_val,
                                                   predict(pruned, val_cluster)),
                                   best_cp  = best_cp))
}

best_cart_p <- results_cart[which.min(results_cart$val_rmse), ]
final_cart  <- rpart::rpart(
  nb_departure ~ ., data = train_cluster,
  control = rpart::rpart.control(
    cp        = best_cart_p$best_cp,
    minsplit  = best_cart_p$minsplit,
    minbucket = best_cart_p$minbucket,
    maxdepth  = best_cart_p$maxdepth
  )
)

resultats[["CART"]] <- performance_summary(
  "CART",
  nb_dep_train, predict(final_cart, train_cluster),
  nb_dep_val,   predict(final_cart, val_cluster),
  nb_dep_test,  predict(final_cart, test_cluster)
)

message("CART termine.")

# =============================================================================
# 5. Ctree — arbre d'inference conditionnelle
# =============================================================================
grid_ctree    <- expand.grid(minsplit = c(5, 10, 20),
                              maxdepth = c(5, 10, 15))
results_ctree <- data.frame()

for (i in seq_len(nrow(grid_ctree))) {
  m <- party::ctree(
    nb_departure ~ ., data = train_base,
    controls = party::ctree_control(
      minsplit = grid_ctree$minsplit[i],
      maxdepth = grid_ctree$maxdepth[i]
    )
  )
  results_ctree <- rbind(results_ctree,
                         data.frame(
                           minsplit = grid_ctree$minsplit[i],
                           maxdepth = grid_ctree$maxdepth[i],
                           val_rmse = rmse(nb_dep_val, predict(m, val_base))
                         ))
}

best_ctree_p <- results_ctree[which.min(results_ctree$val_rmse), ]
final_ctree  <- party::ctree(
  nb_departure ~ ., data = train_base,
  controls = party::ctree_control(
    minsplit = best_ctree_p$minsplit,
    maxdepth = best_ctree_p$maxdepth
  )
)

resultats[["Ctree"]] <- performance_summary(
  "Ctree",
  nb_dep_train, predict(final_ctree, train_base),
  nb_dep_val,   predict(final_ctree, val_base),
  nb_dep_test,  predict(final_ctree, test_base)
)

message("Ctree termine.")

# =============================================================================
# 6. Random Forest — grille mtry x ntree (parallele)
# =============================================================================
p       <- ncol(train_cluster) - 1
grid_rf <- expand.grid(mtry  = seq(5, p, by = 2),
                        ntree = c(200, 500, 800, 1000))

cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

rf_results <- foreach::foreach(
  i         = seq_len(nrow(grid_rf)),
  .packages = "randomForest",
  .export   = c("train_cluster", "val_cluster", "nb_dep_val", "grid_rf", "rmse")
) %dopar% {
  rf <- randomForest::randomForest(
    nb_departure ~ .,
    data  = train_cluster,
    ntree = grid_rf$ntree[i],
    mtry  = grid_rf$mtry[i]
  )
  list(mtry     = grid_rf$mtry[i],
       ntree    = grid_rf$ntree[i],
       val_rmse = rmse(nb_dep_val, predict(rf, val_cluster)),
       model    = rf)
}

parallel::stopCluster(cl)

best_rf_idx   <- which.min(sapply(rf_results, `[[`, "val_rmse"))
best_rf       <- rf_results[[best_rf_idx]]$model
best_rf_mtry  <- rf_results[[best_rf_idx]]$mtry
best_rf_ntree <- rf_results[[best_rf_idx]]$ntree

message("Random Forest optimal : ntree=", best_rf_ntree, ", mtry=", best_rf_mtry)

resultats[["Random Forest"]] <- performance_summary(
  paste0("Random Forest (ntree=", best_rf_ntree, ", mtry=", best_rf_mtry, ")"),
  nb_dep_train, predict(best_rf, train_cluster),
  nb_dep_val,   predict(best_rf, val_cluster),
  nb_dep_test,  predict(best_rf, test_cluster)
)

saveRDS(best_rf, "outputs/best_rf_model.rds")

# =============================================================================
# 7. Gradient Boosting L2 — GBM (grille complete, parallele)
# =============================================================================
grid_gbm <- expand.grid(
  n.trees           = c(500, 1000, 2000),
  interaction.depth = c(5, 10, 15, 25, 30),
  shrinkage         = c(0.01, 0.03, 0.05, 0.07, 0.09, 0.1),
  n.minobsinnode    = c(5, 10, 20)
)

cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

gbm_results <- foreach::foreach(
  i         = seq_len(nrow(grid_gbm)),
  .packages = "gbm",
  .export   = c("train_cluster", "val_cluster", "nb_dep_val", "grid_gbm")
) %dopar% {
  model <- gbm::gbm(
    nb_departure ~ .,
    data              = train_cluster,
    distribution      = "gaussian",
    n.trees           = grid_gbm$n.trees[i],
    interaction.depth = grid_gbm$interaction.depth[i],
    shrinkage         = grid_gbm$shrinkage[i],
    n.minobsinnode    = grid_gbm$n.minobsinnode[i]
  )
  pred_val <- predict(model, val_cluster, n.trees = grid_gbm$n.trees[i])
  list(params  = grid_gbm[i, ],
       mse_val = mean((nb_dep_val - pred_val)^2),
       model   = model)
}

parallel::stopCluster(cl)

best_gbm_idx <- which.min(sapply(gbm_results, `[[`, "mse_val"))
best_gbm     <- gbm_results[[best_gbm_idx]]$model
best_params  <- gbm_results[[best_gbm_idx]]$params

message("GBM optimal : n.trees=", best_params$n.trees,
        ", depth=", best_params$interaction.depth,
        ", shrinkage=", best_params$shrinkage)

resultats[["GBM"]] <- performance_summary(
  paste0("GBM (n.trees=", best_params$n.trees,
         ", depth=", best_params$interaction.depth,
         ", shrink=", best_params$shrinkage, ")"),
  nb_dep_train, predict(best_gbm, train_cluster, n.trees = best_params$n.trees),
  nb_dep_val,   predict(best_gbm, val_cluster,   n.trees = best_params$n.trees),
  nb_dep_test,  predict(best_gbm, test_cluster,  n.trees = best_params$n.trees)
)

saveRDS(best_gbm, "outputs/best_gbm_model.rds")

# =============================================================================
# Tableau comparatif final — tous les modeles
# =============================================================================
tableau_final <- compare_models(resultats)
print(tableau_final)

write.csv(tableau_final, "outputs/resultats_baseline_ml.csv", row.names = FALSE)
message("Script 02 termine. Resultats dans outputs/resultats_baseline_ml.csv")
