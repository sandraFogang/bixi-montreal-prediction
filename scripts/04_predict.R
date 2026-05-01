# =============================================================================
# scripts/04_predict.R
# Prédictions finales — modèle retenu : LASSO + Krigeage Simple (SK)
# -----------------------------------------------------------------------------
# Auteure  : Sandra Desmair Fogang Lontouo
# Projet   : Prédiction du flux de cyclistes BIXI — Montréal 2019
# Sortie   : outputs/predictions_finales.csv
# Prérequis: avoir exécuté 03_spatial.R au préalable
#            (génère outputs/lasso_model.rds et outputs/variogram_model.rds)
# =============================================================================

source("packages.R")
source("R/preprocess.R")
source("R/evaluate.R")
source("R/spatial.R")

# =============================================================================
# 1. Chargement des modèles et des données
# =============================================================================
lasso_cv  <- readRDS("outputs/lasso_model.rds")
vg_result <- readRDS("outputs/variogram_model.rds")

data           <- prepare_data("data/raw/BIXI.RData")
train          <- data$train
Bixi_a_predire <- data$Bixi_a_predire
nb_dep_train   <- train$nb_departure

# =============================================================================
# 2. Prédictions LASSO sur les données d'entraînement (pour les résidus)
# =============================================================================
data_train <- train %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c, -latitude, -longitude)

make_X <- function(df) model.matrix(log(nb_departure) ~ ., data = df)[, -1]

X_train    <- make_X(data_train)
pred_train <- as.vector(exp(predict(lasso_cv, X_train, s = "lambda.1se")))

# =============================================================================
# 3. Résidus agrégés par station (entrée du krigeage)
# =============================================================================
resid_sp <- aggregate_residuals(train, nb_dep_train - pred_train)

# =============================================================================
# 4. Prédictions LASSO sur les valeurs manquantes
# =============================================================================
data_predire <- Bixi_a_predire %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c, -latitude, -longitude)

X_predire <- model.matrix(~ . - nb_departure, data = data_predire)[, -1]

pred_lasso_manq <- as.vector(
  exp(predict(lasso_cv, newx = X_predire, s = "lambda.1se"))
)

# =============================================================================
# 5. Krigeage Simple (SK) sur les localisations manquantes
# =============================================================================
predire_sp <- Bixi_a_predire %>%
  dplyr::group_by(location) %>%
  dplyr::summarise(
    longitude = dplyr::first(longitude),
    latitude  = dplyr::first(latitude),
    .groups   = "drop"
  ) %>%
  as.data.frame()
sp::coordinates(predire_sp) <- ~longitude + latitude

kriged_manq <- gstat::krige(
  resid ~ 1, resid_sp, predire_sp,
  model       = vg_result$best_model,
  beta        = 0,
  debug.level = 0
)

# Prédiction finale : LASSO + résidu krigé
pred_finale <- pred_lasso_manq +
  kriged_manq$var1.pred[match(Bixi_a_predire$location, predire_sp$location)]

# =============================================================================
# 6. Sauvegarde des résultats
# =============================================================================
resultats_finaux <- data.frame(
  location     = Bixi_a_predire$location,
  nb_departure = pred_finale
)

write.csv(resultats_finaux, "outputs/predictions_finales.csv", row.names = FALSE)
message("Prédictions finales : outputs/predictions_finales.csv")
message("Nombre de valeurs prédites : ", nrow(resultats_finaux))
