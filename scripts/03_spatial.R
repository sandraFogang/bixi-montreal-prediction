# =============================================================================
# scripts/03_spatial.R
# Analyse spatiale : Test de Moran + Regression Kriging (OK, SK, UK)
# -----------------------------------------------------------------------------
# Auteure  : Sandra Desmair Fogang Lontouo
# Projet   : Prédiction du flux de cyclistes BIXI — Montréal 2019
# Sortie   : outputs/resultats_spatial.csv, outputs/lasso_model.rds,
#            outputs/variogram_model.rds, outputs/figures/variogramme.png
# Prérequis: packages.R, R/preprocess.R, R/evaluate.R, R/spatial.R
# Modèle retenu : LASSO + Krigeage Simple (SK)
# =============================================================================

source("packages.R")
source("R/preprocess.R")
source("R/evaluate.R")
source("R/spatial.R")

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

# =============================================================================
# 1. Test de Moran — justification de l'approche spatiale
# =============================================================================
baseline   <- mean(nb_dep_train)
resid_naif <- data.frame(
  location  = train$location,
  resid     = train$nb_departure - baseline,
  longitude = train$longitude,
  latitude  = train$latitude
) %>%
  dplyr::group_by(location) %>%
  dplyr::summarise(resid     = mean(resid),
                   longitude = dplyr::first(longitude),
                   latitude  = dplyr::first(latitude),
                   .groups   = "drop")

coords       <- cbind(resid_naif$longitude, resid_naif$latitude)
moran_result <- test_moran(resid_naif$resid, coords, k = 5)

# Sauvegarder le graphique
png("outputs/figures/test_moran.png", width = 800, height = 500)
plot_moran(moran_result)
dev.off()

message("Test de Moran sauvegardé.")

# =============================================================================
# 2. LASSO — partie déterministe du Regression Kriging
# =============================================================================
data_train <- train %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c, -latitude, -longitude)
data_val   <- validation %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c, -latitude, -longitude)
data_test  <- test %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c, -latitude, -longitude)

make_X <- function(df) model.matrix(log(nb_departure) ~ ., data = df)[, -1]

X_train  <- make_X(data_train)
X_val    <- make_X(data_val)
X_test   <- make_X(data_test)
y_log    <- log(data_train$nb_departure)

set.seed(123)
lasso_cv <- glmnet::cv.glmnet(x = X_train, y = y_log, alpha = 1, nfolds = 10)

pred_train_lasso <- as.vector(exp(predict(lasso_cv, X_train, s = "lambda.1se")))
pred_val_lasso   <- as.vector(exp(predict(lasso_cv, X_val,   s = "lambda.1se")))
pred_test_lasso  <- as.vector(exp(predict(lasso_cv, X_test,  s = "lambda.1se")))

resultats <- list()
resultats[["LASSO"]] <- performance_summary(
  "LASSO",
  nb_dep_train, pred_train_lasso,
  nb_dep_val,   pred_val_lasso,
  nb_dep_test,  pred_test_lasso
)

# =============================================================================
# 3. Variogramme sur les résidus LASSO
# =============================================================================
resid_sp  <- aggregate_residuals(train, nb_dep_train - pred_train_lasso)
vg_result <- fit_best_variogram(resid_sp)

# Graphique variogramme
png("outputs/figures/variogramme.png", width = 700, height = 500)
plot(vg_result$vg_emp, vg_result$best_model,
     main = paste("Variogramme ajusté —", vg_result$best_name),
     xlab = "Distance", ylab = "Semi-variance")
dev.off()

# =============================================================================
# 4. Regression Kriging — OK, SK, UK
# =============================================================================
val_sp  <- prep_spatial(validation)
test_sp <- prep_spatial(test)

for (type in c("OK", "SK", "UK")) {
  krig <- apply_kriging(
    pred_lasso_val  = pred_val_lasso,
    pred_lasso_test = pred_test_lasso,
    resid_sp        = resid_sp,
    val_sp          = val_sp,
    test_sp         = test_sp,
    vg_model        = vg_result$best_model,
    validation      = validation,
    test            = test,
    type            = type
  )
  resultats[[paste0("LASSO + ", type)]] <- data.frame(
    Modele     = paste0("LASSO + ", type),
    RMSE_train = NA,
    RMSE_val   = rmse(nb_dep_val,  krig$pred_val),
    RMSE_test  = rmse(nb_dep_test, krig$pred_test),
    MAE_train  = NA,
    MAE_val    = mae(nb_dep_val,   krig$pred_val),
    MAE_test   = mae(nb_dep_test,  krig$pred_test),
    stringsAsFactors = FALSE
  )
  # Sauvegarder graphique y vs ŷ pour chaque méthode
  p <- plot_yvyhat(nb_dep_val, krig$pred_val,
                   titre   = paste("LASSO +", type),
                   couleur = switch(type, "OK" = "seagreen",
                                    "SK" = "steelblue", "UK" = "darkorange"))
  ggplot2::ggsave(paste0("outputs/figures/yvyhat_", type, ".png"),
                  plot = p, width = 5, height = 4, dpi = 150)
}

# =============================================================================
# Tableau final
# =============================================================================
tableau_final <- compare_models(resultats)
print(tableau_final)

write.csv(tableau_final, "outputs/resultats_spatial.csv", row.names = FALSE)
saveRDS(lasso_cv,        "outputs/lasso_model.rds")
saveRDS(vg_result,       "outputs/variogram_model.rds")

message("Script 03 terminé. Résultats dans outputs/")
