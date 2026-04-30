# scripts/04_predict.R
# Prédiction des valeurs manquantes avec le modèle retenu
# Modèle : LASSO + Krigeage Simple (SK)
# Prérequis : avoir exécuté 02_baseline_ml.R et 03_spatial.R

source("packages.R")
source("R/preprocess.R")
source("R/evaluate.R")
source("R/spatial.R")

# --- Charger les modèles sauvegardés ---
lasso_cv   <- readRDS("outputs/lasso_model.rds")
vg_result  <- readRDS("outputs/variogram_model.rds")

# --- Charger les données ---
data <- prepare_data("data/raw/BIXI.RData")
train          <- data$train
Bixi_a_predire <- data$Bixi_a_predire

nb_dep_train <- train$nb_departure

# --- Prédictions LASSO sur les observations manquantes ---
data_predire <- Bixi_a_predire %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c, -latitude, -longitude)

make_X <- function(df) model.matrix(log(nb_departure + 0.001) ~ ., data = df)[, -1]

# Note : nb_departure est NA pour ces observations — on prédit sans la cible
X_predire <- model.matrix(~ . - nb_departure, data = data_predire)[, -1]

pred_lasso_manq <- as.vector(
  exp(predict(lasso_cv, newx = X_predire, s = "lambda.1se"))
)

# --- Appliquer le krigeage SK sur les résidus ---
# Résidus du train agrégés par station
data_train <- train %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c, -latitude, -longitude)
make_X_train <- function(df) model.matrix(log(nb_departure) ~ ., data = df)[, -1]
X_train      <- make_X_train(data_train)
pred_train   <- as.vector(exp(predict(lasso_cv, X_train, s = "lambda.1se")))

resid_sp <- aggregate_residuals(train, nb_dep_train - pred_train)

# Localisations à prédire
predire_sp <- Bixi_a_predire %>%
  dplyr::group_by(location) %>%
  dplyr::summarise(longitude = dplyr::first(longitude),
                   latitude  = dplyr::first(latitude),
                   .groups   = "drop") %>%
  as.data.frame()
sp::coordinates(predire_sp) <- ~longitude + latitude

# Krigeage Simple (SK)
kriged_manq <- gstat::krige(resid ~ 1, resid_sp, predire_sp,
                             model = vg_result$best_model,
                             beta  = 0, debug.level = 0)

pred_finale <- pred_lasso_manq +
  kriged_manq$var1.pred[match(Bixi_a_predire$location, predire_sp$location)]

# --- Résumé et sauvegarde ---
summary(pred_finale)

hist(pred_finale, breaks = 30,
     main = "Distribution des prédictions finales (LASSO + SK)",
     xlab = "Départs prédits", col = "steelblue", border = "white")

resultats_finaux <- data.frame(
  location     = Bixi_a_predire$location,
  nb_departure = pred_finale
)

write.csv(resultats_finaux, "outputs/predictions_finales.csv", row.names = FALSE)
message("Prédictions finales sauvegardées dans outputs/predictions_finales.csv")
message("Nombre de valeurs prédites : ", nrow(resultats_finaux))
