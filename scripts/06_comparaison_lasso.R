

source("packages.R")
source("R/preprocess.R")
source("R/evaluate.R")
source("R/spatial.R")

library(ggplot2)
library(dplyr)
library(patchwork)

# =============================================================
# CHARGEMENT
# =============================================================
load("data/raw/BIXI.RData")
data         <- prepare_data("data/raw/BIXI.RData")
train        <- data$train
validation   <- data$validation
test         <- data$test
nb_dep_train <- train$nb_departure
nb_dep_val   <- validation$nb_departure
nb_dep_test  <- test$nb_departure

spatial <- normalize_coords(Spatial_positions)
spatial <- add_geo_clusters(spatial, k = 5)

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae  <- function(obs, pred) mean(abs(obs - pred))

make_X <- function(df) {
  model.matrix(log(nb_departure) ~ ., data = df)[, -1]
}

# =============================================================
# MODÈLE A — LASSO sans lat/long/cluster (original)
# =============================================================
message("Modele A : LASSO sans lat/long/cluster...")

data_train_A <- train %>%
  dplyr::select(-location, -clusters_geo,
                -mean_temp_c, -latitude, -longitude)
data_val_A   <- validation %>%
  dplyr::select(-location, -clusters_geo,
                -mean_temp_c, -latitude, -longitude)
data_test_A  <- test %>%
  dplyr::select(-location, -clusters_geo,
                -mean_temp_c, -latitude, -longitude)

X_train_A <- make_X(data_train_A)
X_val_A   <- make_X(data_val_A)
X_test_A  <- make_X(data_test_A)

set.seed(123)
lasso_A <- glmnet::cv.glmnet(X_train_A, log(nb_dep_train),
                             alpha = 1, nfolds = 10)

pred_train_A <- as.vector(exp(predict(lasso_A, X_train_A, s = "lambda.1se")))
pred_val_A   <- as.vector(exp(predict(lasso_A, X_val_A,   s = "lambda.1se")))
pred_test_A  <- as.vector(exp(predict(lasso_A, X_test_A,  s = "lambda.1se")))

# =============================================================
# MODÈLE B — LASSO sans lat/long/cluster + Krigeage SK
# =============================================================
message("Modele B : LASSO sans lat/long + SK...")

resid_sp_A  <- aggregate_residuals(train, nb_dep_train - pred_train_A)
vg_A        <- fit_best_variogram(resid_sp_A)
val_sp      <- prep_spatial(validation)
test_sp     <- prep_spatial(test)

krig_B <- apply_kriging(
  pred_val_A, pred_test_A,
  resid_sp_A, val_sp, test_sp,
  vg_A$best_model, validation, test, type = "SK"
)

# =============================================================
# MODÈLE C — LASSO avec lat/long + cluster (nouveau)
# =============================================================
message("Modele C : LASSO avec lat/long/cluster...")

data_train_C <- train %>%
  dplyr::select(-location, -mean_temp_c)
data_val_C   <- validation %>%
  dplyr::select(-location, -mean_temp_c)
data_test_C  <- test %>%
  dplyr::select(-location, -mean_temp_c)

X_train_C <- make_X(data_train_C)
X_val_C   <- make_X(data_val_C)
X_test_C  <- make_X(data_test_C)

set.seed(123)
lasso_C <- glmnet::cv.glmnet(X_train_C, log(nb_dep_train),
                             alpha = 1, nfolds = 10)

pred_train_C <- as.vector(exp(predict(lasso_C, X_train_C, s = "lambda.1se")))
pred_val_C   <- as.vector(exp(predict(lasso_C, X_val_C,   s = "lambda.1se")))
pred_test_C  <- as.vector(exp(predict(lasso_C, X_test_C,  s = "lambda.1se")))

# =============================================================
# MODÈLE D — LASSO avec lat/long + cluster + Krigeage SK
# =============================================================
message("Modele D : LASSO avec lat/long/cluster + SK...")

resid_sp_C <- aggregate_residuals(train, nb_dep_train - pred_train_C)
vg_C       <- fit_best_variogram(resid_sp_C)

krig_D <- apply_kriging(
  pred_val_C, pred_test_C,
  resid_sp_C, val_sp, test_sp,
  vg_C$best_model, validation, test, type = "SK"
)

# =============================================================
# TABLEAU DES PERFORMANCES
# =============================================================
perf <- data.frame(
  Modele = c(
    "A — LASSO\n(sans spatial)",
    "B — LASSO + SK\n(sans spatial)",
    "C — LASSO\n(avec lat/long/cluster)",
    "D — LASSO + SK\n(avec lat/long/cluster)"
  ),
  RMSE_val = c(
    rmse(nb_dep_val,  pred_val_A),
    rmse(nb_dep_val,  krig_B$pred_val),
    rmse(nb_dep_val,  pred_val_C),
    rmse(nb_dep_val,  krig_D$pred_val)
  ),
  RMSE_test = c(
    rmse(nb_dep_test, pred_test_A),
    rmse(nb_dep_test, krig_B$pred_test),
    rmse(nb_dep_test, pred_test_C),
    rmse(nb_dep_test, krig_D$pred_test)
  ),
  MAE_val = c(
    mae(nb_dep_val,  pred_val_A),
    mae(nb_dep_val,  krig_B$pred_val),
    mae(nb_dep_val,  pred_val_C),
    mae(nb_dep_val,  krig_D$pred_val)
  ),
  Type = c("LASSO seul", "LASSO + Krigeage",
           "LASSO seul", "LASSO + Krigeage"),
  Spatial = c("Sans lat/long/cluster", "Sans lat/long/cluster",
              "Avec lat/long/cluster", "Avec lat/long/cluster")
)

print(round(perf[, c("Modele","RMSE_val","RMSE_test","MAE_val")], 4))

# =============================================================
# GRAPHIQUE 1 — Comparaison des 4 modèles (RMSE val et test)
# =============================================================
message("Graphique 1 : Comparaison des 4 modeles...")

perf_long <- perf %>%
  tidyr::pivot_longer(
    cols      = c(RMSE_val, RMSE_test),
    names_to  = "Partition",
    values_to = "RMSE"
  ) %>%
  mutate(
    Partition = recode(Partition,
                       "RMSE_val"  = "Validation",
                       "RMSE_test" = "Test"),
    Modele = factor(Modele, levels = rev(perf$Modele))
  )

couleurs <- c(
  "LASSO seul"      = "#9ecae1",
  "LASSO + Krigeage" = "#e6550d"
)

g1 <- ggplot(perf_long,
             aes(x = RMSE, y = Modele,
                 fill = Type, alpha = Partition)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6) +
  geom_text(aes(label = round(RMSE, 4)),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.3, fontface = "bold") +
  scale_fill_manual(values = couleurs) +
  scale_alpha_manual(values = c("Validation" = 1,
                                "Test"       = 0.55)) +
  scale_x_continuous(limits  = c(0, 0.25),
                     expand  = expansion(mult = c(0, 0.12))) +
  facet_wrap(~Spatial, ncol = 1, scales = "free_y") +
  labs(
    title    = "Impact de lat/long/cluster et du krigeage sur le RMSE",
    subtitle = "Comparaison de 4 configurations — validation et test",
    x        = "RMSE (plus bas = meilleur)",
    y        = NULL,
    fill     = "Modèle",
    alpha    = "Partition",
    caption  = "Le krigeage améliore toujours les prédictions, indépendamment du LASSO de base"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 10),
    plot.caption       = element_text(color = "grey40", size = 9),
    strip.text         = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom"
  )

ggsave("outputs/figures/06_comparaison_4_modeles.png",
       plot = g1, width = 10, height = 7, dpi = 180)
message("  -> Graphique 1 sauvegarde !")

# =============================================================
# GRAPHIQUE 2 — Résidus spatiaux : A vs C
# Montre si lat/long dans LASSO réduit la structure spatiale
# =============================================================
message("Graphique 2 : Residus spatiaux...")

resid_carte <- bind_rows(
  train %>%
    group_by(location) %>%
    summarise(
      resid     = mean(nb_dep_train - pred_train_A[cur_group_id()]),
      longitude = first(longitude),
      latitude  = first(latitude),
      .groups   = "drop"
    ) %>%
    mutate(Modele = "LASSO sans lat/long/cluster"),
  train %>%
    group_by(location) %>%
    summarise(
      resid     = mean(nb_dep_train - pred_train_C[cur_group_id()]),
      longitude = first(longitude),
      latitude  = first(latitude),
      .groups   = "drop"
    ) %>%
    mutate(Modele = "LASSO avec lat/long/cluster")
) %>%
  mutate(
    resid_abs = abs(resid),
    signe     = ifelse(resid > 0, "Sous-estimé", "Sur-estimé")
  )

g2 <- ggplot(resid_carte,
             aes(x = longitude, y = latitude,
                 color = resid, size = resid_abs)) +
  geom_point(alpha = 0.8) +
  scale_color_gradient2(
    low      = "#d73027",
    mid      = "white",
    high     = "#4575b4",
    midpoint = 0,
    name     = "Résidu\nmoyen"
  ) +
  scale_size_continuous(range = c(0.5, 5), guide = "none") +
  facet_wrap(~Modele, ncol = 2) +
  labs(
    title    = "Structure spatiale des résidus",
    subtitle = "Rouge = sous-estimation  |  Bleu = sur-estimation",
    x        = "Longitude",
    y        = "Latitude",
    caption  = "Si les résidus sont aléatoires, le krigeage apportera peu"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    strip.text    = element_text(face = "bold", size = 10),
    panel.grid    = element_line(color = "grey92"),
    legend.position = "right"
  )

ggsave("outputs/figures/07_residus_spatiaux.png",
       plot = g2, width = 12, height = 5, dpi = 180)
message("  -> Graphique 2 sauvegarde !")

# =============================================================
# GRAPHIQUE 3 — Gain du krigeage selon le LASSO de base
# =============================================================
message("Graphique 3 : Gain du krigeage...")

gain <- data.frame(
  Configuration = c("Sans lat/long/cluster", "Avec lat/long/cluster"),
  RMSE_avant    = c(rmse(nb_dep_val, pred_val_A),
                    rmse(nb_dep_val, pred_val_C)),
  RMSE_apres    = c(rmse(nb_dep_val, krig_B$pred_val),
                    rmse(nb_dep_val, krig_D$pred_val))
) %>%
  mutate(
    Gain_absolu  = RMSE_avant - RMSE_apres,
    Gain_pct     = round((Gain_absolu / RMSE_avant) * 100, 2),
    label_avant  = paste0("Avant : ", round(RMSE_avant, 4)),
    label_apres  = paste0("Après : ", round(RMSE_apres, 4)),
    label_gain   = paste0("-", Gain_pct, "%")
  )

g3 <- ggplot(gain) +
  geom_segment(
    aes(x = 0, xend = Gain_absolu,
        y = Configuration, yend = Configuration),
    color     = "#e6550d",
    linewidth = 2,
    arrow     = arrow(length = unit(0.3, "cm"), type = "closed")
  ) +
  geom_text(aes(x = Gain_absolu / 2,
                y = Configuration,
                label = label_gain),
            vjust = -0.8, fontface = "bold",
            size = 5, color = "#e6550d") +
  geom_text(aes(x = 0,
                y = Configuration,
                label = label_avant),
            hjust = 1.1, size = 3.5, color = "grey40") +
  geom_text(aes(x = Gain_absolu,
                y = Configuration,
                label = label_apres),
            hjust = -0.1, size = 3.5, color = "#3182bd") +
  scale_x_continuous(limits = c(-0.01, max(gain$Gain_absolu) * 1.5),
                     expand = expansion(mult = c(0.15, 0.1))) +
  labs(
    title    = "Gain apporté par le Krigeage Simple (SK)",
    subtitle = "Réduction du RMSE sur validation — selon le LASSO de base",
    x        = "Réduction du RMSE",
    y        = NULL,
    caption  = "Le krigeage est plus utile quand le LASSO n'a pas accès aux coordonnées spatiales"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey40",
                                 face  = "italic", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("outputs/figures/08_gain_krigeage_comparaison.png",
       plot = g3, width = 9, height = 5, dpi = 180)
message("  -> Graphique 3 sauvegarde !")

message("")
message("Tous les graphiques de comparaison sont prets !")
message("  06_comparaison_4_modeles.png")
message("  07_residus_spatiaux.png")
message("  08_gain_krigeage_comparaison.png")