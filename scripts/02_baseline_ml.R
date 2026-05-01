# =============================================================================
# scripts/02_baseline_ml.R
# Modèles ML classiques : LASSO, CART, Random Forest, GBM
# -----------------------------------------------------------------------------
# Auteure  : Sandra Desmair Fogang Lontouo
# Projet   : Prédiction du flux de cyclistes BIXI — Montréal 2019
# Sortie   : outputs/resultats_baseline_ml.csv, outputs/best_gbm_model.rds
# Prérequis: packages.R, R/preprocess.R, R/evaluate.R
# Note     : le GBM (grille complète) peut prendre plusieurs heures
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

# Scénario retenu : avec cluster géographique (meilleur RMSE validation)
# lat/long exclus — réservés à la partie spatiale (script 03)
train_cluster <- dplyr::select(train,      -location)
val_cluster   <- dplyr::select(validation, -location)
test_cluster  <- dplyr::select(test,       -location)

resultats <- list()

# =============================================================================
# 1. LASSO (scénario cluster — meilleur scénario identifié)
# =============================================================================
make_X <- function(df) {
  model.matrix(log(nb_departure) ~ ., data = df)[, -1]
}

X_train <- make_X(train_cluster)
X_val   <- make_X(val_cluster)
X_test  <- make_X(test_cluster)
y_log   <- log(nb_dep_train)

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

# =============================================================================
# 2. CART (scénario cluster, grille d'hyperparamètres)
# =============================================================================
grid_cart <- expand.grid(
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
  cp_vals   <- tree_tmp$cptable[, "CP"]
  best_cp   <- cp_vals[which.min(
    sapply(cp_vals, function(cp) {
      p <- predict(rpart::prune(tree_tmp, cp = cp), val_cluster)
      rmse(nb_dep_val, p)
    })
  )]
  pruned    <- rpart::prune(tree_tmp, cp = best_cp)
  val_rmse  <- rmse(nb_dep_val, predict(pruned, val_cluster))
  results_cart <- rbind(results_cart,
                        cbind(grid_cart[i, ], val_rmse = val_rmse,
                              best_cp = best_cp))
}

best_cart_params <- results_cart[which.min(results_cart$val_rmse), ]
final_cart <- rpart::rpart(
  nb_departure ~ ., data = train_cluster,
  control = rpart::rpart.control(
    cp        = best_cart_params$best_cp,
    minsplit  = best_cart_params$minsplit,
    minbucket = best_cart_params$minbucket,
    maxdepth  = best_cart_params$maxdepth
  )
)

resultats[["CART"]] <- performance_summary(
  "CART",
  nb_dep_train, predict(final_cart, train_cluster),
  nb_dep_val,   predict(final_cart, val_cluster),
  nb_dep_test,  predict(final_cart, test_cluster)
)

# =============================================================================
# 3. Random Forest (scénario cluster, grille mtry x ntree)
# =============================================================================
p          <- ncol(train_cluster) - 1
mtry_vals  <- seq(5, p, by = 2)
ntree_vals <- c(200, 500, 800, 1000)
grid_rf    <- expand.grid(mtry = mtry_vals, ntree = ntree_vals)

n_cores <- parallel::detectCores() - 1
cl      <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

rf_results <- foreach::foreach(i = seq_len(nrow(grid_rf)),
                                .packages = "randomForest",
                                .export   = c("train_cluster", "val_cluster",
                                              "nb_dep_val", "grid_rf", "rmse")) %dopar% {
  m  <- grid_rf$mtry[i]
  nt <- grid_rf$ntree[i]
  rf <- randomForest::randomForest(nb_departure ~ ., data = train_cluster,
                                   ntree = nt, mtry = m)
  list(mtry = m, ntree = nt,
       val_rmse = rmse(nb_dep_val, predict(rf, val_cluster)),
       model    = rf)
}

parallel::stopCluster(cl)

best_rf_idx  <- which.min(sapply(rf_results, `[[`, "val_rmse"))
best_rf      <- rf_results[[best_rf_idx]]$model
best_rf_mtry <- rf_results[[best_rf_idx]]$mtry
best_rf_ntree<- rf_results[[best_rf_idx]]$ntree

message("Random Forest optimal : ntree=", best_rf_ntree, ", mtry=", best_rf_mtry)

resultats[["Random Forest"]] <- performance_summary(
  paste0("Random Forest (ntree=", best_rf_ntree, ", mtry=", best_rf_mtry, ")"),
  nb_dep_train, predict(best_rf, train_cluster),
  nb_dep_val,   predict(best_rf, val_cluster),
  nb_dep_test,  predict(best_rf, test_cluster)
)

# =============================================================================
# 4. Gradient Boosting — GBM (scénario cluster, grille complète)
# =============================================================================
grid_gbm <- expand.grid(
  n.trees           = c(500, 1000, 2000),
  interaction.depth = c(5, 10, 15, 25, 30),
  shrinkage         = c(0.01, 0.03, 0.05, 0.07, 0.09, 0.1),
  n.minobsinnode    = c(5, 10, 20)
)

cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

gbm_results <- foreach::foreach(i = seq_len(nrow(grid_gbm)),
                                 .packages = "gbm",
                                 .export   = c("train_cluster", "val_cluster",
                                               "nb_dep_val", "grid_gbm")) %dopar% {
  model <- gbm::gbm(
    nb_departure ~ ., data = train_cluster,
    distribution      = "gaussian",
    n.trees           = grid_gbm$n.trees[i],
    interaction.depth = grid_gbm$interaction.depth[i],
    shrinkage         = grid_gbm$shrinkage[i],
    n.minobsinnode    = grid_gbm$n.minobsinnode[i]
  )
  pred_val <- predict(model, val_cluster, n.trees = grid_gbm$n.trees[i])
  mse_val  <- mean((nb_dep_val - pred_val)^2)
  list(params = grid_gbm[i, ], mse_val = mse_val, model = model)
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

# =============================================================================
# Tableau comparatif final
# =============================================================================
tableau_final <- compare_models(resultats)

# Sauvegarder les résultats
saveRDS(tableau_final, "outputs/resultats_baseline_ml.rds")
write.csv(tableau_final, "outputs/resultats_baseline_ml.csv", row.names = FALSE)

# Sauvegarder le meilleur modèle GBM pour prédictions futures
saveRDS(best_gbm, "outputs/best_gbm_model.rds")

message("Script 02 terminé. Résultats dans outputs/")
