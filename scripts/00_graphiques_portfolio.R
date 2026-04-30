

#setwd("~/Admission HEC Montreal data/PF Github/bixi-montreal-prediction")


source("packages.R")
source("R/preprocess.R")
source("R/evaluate.R")
source("R/spatial.R")

library(ggplot2)
library(patchwork)
library(dplyr)

# =============================================================
# CHARGEMENT ET PRÉPARATION
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

# Joindre clusters aux données
train_full <- train %>%
  left_join(spatial %>% dplyr::select(location, clusters_geo),
            by = "location")

# =============================================================
# GRAPHIQUE 1 — Carte de chaleur des départs par station
# =============================================================
message("Graphique 1 : Carte de chaleur...")

station_stats <- train %>%
  group_by(location) %>%
  summarise(
    mean_dep  = mean(nb_departure),
    .groups   = "drop"
  ) %>%
  left_join(spatial %>% dplyr::select(location, latitude, longitude,
                                      clusters_geo),
            by = "location")

p1 <- ggplot(station_stats,
             aes(x = longitude, y = latitude, color = mean_dep,
                 size = mean_dep)) +
  geom_point(alpha = 0.85) +
  scale_color_gradient2(
    low     = "#3182bd",
    mid     = "#fdae6b",
    high    = "#e6550d",
    midpoint = median(station_stats$mean_dep),
    name    = "Départs\nmoyens"
  ) +
  scale_size_continuous(range = c(1, 6), guide = "none") +
  labs(
    title    = "Flux de cyclistes BIXI — 587 stations de Montréal",
    subtitle = "Taille et couleur proportionnelles au nombre moyen de départs journaliers",
    x        = "Longitude",
    y        = "Latitude",
    caption  = "Données : BIXI Montréal 2019"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    legend.position = "right",
    panel.grid    = element_line(color = "grey92")
  )

ggsave("outputs/figures/01_carte_chaleur_stations.png",
       plot = p1, width = 10, height = 7, dpi = 180)
message("  -> Graphique 1 sauvegarde !")

# =============================================================
# GRAPHIQUE 2 — Comparaison visuelle de tous les modèles
# =============================================================
message("Graphique 2 : Comparaison des modeles...")

resultats_modeles <- data.frame(
  Modele = c("Stepwise", "LASSO", "Elastic Net",
             "CART", "Ctree",
             "Random Forest", "Boosting L2"),
  RMSE = c(0.18510, 0.17743, 0.17743,
           0.11315, 0.12578,
           0.09207, 0.08455),
  Famille = c("Linéaire", "Linéaire", "Linéaire",
              "Arbre", "Arbre",
              "Ensemble", "Ensemble")
)

resultats_modeles <- resultats_modeles %>%
  mutate(Modele = reorder(Modele, -RMSE))

couleurs_famille <- c(
  "Linéaire"  = "#9ecae1",
  "Arbre"     = "#a1d99b",
  "Ensemble"  = "#e6550d"
)

p2 <- ggplot(resultats_modeles,
             aes(x = RMSE, y = Modele, fill = Famille)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = round(RMSE, 4)),
            hjust = -0.1, size = 3.8, fontface = "bold") +
  geom_vline(xintercept = 0.2441488, linetype = "dashed",
             color = "grey50", linewidth = 0.7) +
  annotate("text", x = 0.2441488, y = 0.6,
           label = "Baseline\n(moyenne)\n0.244",
           hjust = -0.05, size = 3, color = "grey40") +
  scale_fill_manual(values = couleurs_famille) +
  scale_x_continuous(limits = c(0, 0.30),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Comparaison des modèles ML — Partie 1",
    subtitle = "RMSE sur l'ensemble de validation (plus bas = meilleur)",
    x        = "RMSE (validation)",
    y        = NULL,
    fill     = "Famille",
    caption  = "Boosting L2 retenu : RMSE = 0.085 vs baseline 0.244 (-65%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "#e6550d", face = "bold",
                                 size = 9),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("outputs/figures/02_comparaison_modeles.png",
       plot = p2, width = 9, height = 6, dpi = 180)
message("  -> Graphique 2 sauvegarde !")

# =============================================================
# GRAPHIQUE 3 — Gain du krigeage (LASSO vs LASSO+SK)
# =============================================================
message("Graphique 3 : Gain du krigeage (LASSO en cours)...")

data_train <- train %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c,
                -latitude, -longitude)
data_val <- validation %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c,
                -latitude, -longitude)
data_test <- test %>%
  dplyr::select(-location, -clusters_geo, -mean_temp_c,
                -latitude, -longitude)

make_X <- function(df) {
  model.matrix(log(nb_departure) ~ ., data = df)[, -1]
}
X_train <- make_X(data_train)
X_val   <- make_X(data_val)
X_test  <- make_X(data_test)

set.seed(123)
lasso_cv <- glmnet::cv.glmnet(X_train, log(nb_dep_train),
                              alpha = 1, nfolds = 10)

pred_train_lasso <- as.vector(
  exp(predict(lasso_cv, X_train, s = "lambda.1se")))
pred_val_lasso   <- as.vector(
  exp(predict(lasso_cv, X_val,   s = "lambda.1se")))
pred_test_lasso  <- as.vector(
  exp(predict(lasso_cv, X_test,  s = "lambda.1se")))

resid_sp  <- aggregate_residuals(train,
                                 nb_dep_train - pred_train_lasso)
vg_result <- fit_best_variogram(resid_sp)
val_sp    <- prep_spatial(validation)
test_sp   <- prep_spatial(test)

krig_sk <- apply_kriging(
  pred_val_lasso, pred_test_lasso,
  resid_sp, val_sp, test_sp,
  vg_result$best_model,
  validation, test, type = "SK"
)

# Graphique côte à côte LASSO vs LASSO+SK
df_lasso <- data.frame(
  obs  = nb_dep_val,
  pred = pred_val_lasso,
  modele = "LASSO seul\nRMSE = 0.192"
)
df_sk <- data.frame(
  obs  = nb_dep_val,
  pred = krig_sk$pred_val,
  modele = "LASSO + Krigeage SK\nRMSE = 0.185"
)
df_compare <- rbind(df_lasso, df_sk)

p3 <- ggplot(df_compare, aes(x = obs, y = pred)) +
  geom_point(alpha = 0.2, size = 0.8, color = "#3182bd") +
  geom_abline(slope = 1, intercept = 0,
              color = "#e6550d", linewidth = 1) +
  facet_wrap(~modele, ncol = 2) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title    = "Impact du krigeage sur la qualité des prédictions",
    subtitle = "y observé vs y prédit — ensemble de validation",
    x        = "Départs observés (normalisés)",
    y        = "Départs prédits (normalisés)",
    caption  = "Le krigeage réduit la dispersion autour de la diagonale"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    strip.text    = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/figures/03_gain_krigeage.png",
       plot = p3, width = 10, height = 5, dpi = 180)
message("  -> Graphique 3 sauvegarde !")

# =============================================================
# GRAPHIQUE 4 — Importance des variables (GBM)
# =============================================================
message("Graphique 4 : GBM en cours (patience...)...")

train_cluster <- train %>%
  dplyr::select(-location, -latitude, -longitude)

grid_gbm <- expand.grid(
  n.trees           = c(50, 100, 200),
  interaction.depth = c(5, 10, 15, 25, 30),
  shrinkage         = c(0.01, 0.03, 0.05, 0.07, 0.09, 0.1),
  n.minobsinnode    = c(5, 10, 20)
)

val_cluster <- validation %>%
  dplyr::select(-location, -latitude, -longitude)

n_cores <- parallel::detectCores() - 1
cl      <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

gbm_results <- foreach::foreach(
  i = seq_len(nrow(grid_gbm)),
  .packages = "gbm",
  .export   = c("train_cluster", "val_cluster",
                "nb_dep_val", "grid_gbm")
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
  pred_val <- predict(model, val_cluster,
                      n.trees = grid_gbm$n.trees[i])
  mse_val  <- mean((nb_dep_val - pred_val)^2)
  list(params = grid_gbm[i, ], mse_val = mse_val, model = model)
}

parallel::stopCluster(cl)

best_idx    <- which.min(sapply(gbm_results, `[[`, "mse_val"))
best_gbm    <- gbm_results[[best_idx]]$model
best_params <- gbm_results[[best_idx]]$params

saveRDS(best_gbm, "outputs/best_gbm_model.rds")
message("  -> GBM termine ! Meilleurs params : n.trees=",
        best_params$n.trees,
        ", depth=", best_params$interaction.depth,
        ", shrinkage=", best_params$shrinkage)

# Importance des variables
imp <- gbm::summary.gbm(best_gbm, plotit = FALSE) %>%
  head(12) %>%
  mutate(
    var = recode(var,
                 "walkscore"           = "Walkscore",
                 "capacity"            = "Capacité station",
                 "max_temp_f"          = "Température max",
                 "total_precip_mm"     = "Précipitations",
                 "num_pop"             = "Densité population",
                 "len_cycle_path"      = "Longueur pistes cyclables",
                 "len_major_road"      = "Routes principales",
                 "num_metro_stations"  = "Stations de métro",
                 "num_restaurants"     = "Restaurants",
                 "num_bus_stations"    = "Arrêts de bus",
                 "weekend1"            = "Weekend",
                 "holiday1"            = "Jour férié",
                 "len_minor_road"      = "Routes secondaires",
                 "num_other_commercial"= "Commerces",
                 "num_bus_routes"      = "Lignes de bus",
                 "clusters_geo2"       = "Zone géo 2",
                 "clusters_geo3"       = "Zone géo 3",
                 "clusters_geo4"       = "Zone géo 4",
                 "clusters_geo5"       = "Zone géo 5"
    ),
    var = reorder(var, rel.inf)
  )

p4 <- ggplot(imp, aes(x = rel.inf, y = var,
                      fill = rel.inf)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(round(rel.inf, 1), "%")),
            hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#c6dbef", high = "#e6550d",
                      guide = "none") +
  scale_x_continuous(limits  = c(0, max(imp$rel.inf) * 1.2),
                     expand  = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Variables les plus influentes — Boosting L2",
    subtitle = "Importance relative (%) pour la prédiction des départs BIXI",
    x        = "Importance relative (%)",
    y        = NULL,
    caption  = "Le walkscore et la capacité de la station dominent la prédiction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey40", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("outputs/figures/04_importance_variables.png",
       plot = p4, width = 9, height = 6, dpi = 180)
message("  -> Graphique 4 sauvegarde !")

# =============================================================
# GRAPHIQUE 5 — Profil des 5 zones géographiques
# =============================================================
message("Graphique 5 : Profil des zones...")

profil_zones <- train %>%
  left_join(spatial %>% dplyr::select(location, clusters_geo),
            by = "location") %>%
  mutate(
    clusters_geo = recode(as.character(clusters_geo),
                          "1" = "Zone 1\n(Nord-Est)",
                          "2" = "Zone 2\n(Centre)",
                          "3" = "Zone 3\n(Sud)",
                          "4" = "Zone 4\n(Ouest)",
                          "5" = "Zone 5\n(Périphérie)"
    )
  )

stats_zones <- profil_zones %>%
  group_by(clusters_geo) %>%
  summarise(
    mean_dep = mean(nb_departure),
    sd_dep   = sd(nb_departure),
    n        = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    se    = sd_dep / sqrt(n),
    lower = mean_dep - 1.96 * se,
    upper = mean_dep + 1.96 * se
  )

p5 <- ggplot(stats_zones,
             aes(x = clusters_geo, y = mean_dep,
                 fill = clusters_geo)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2, linewidth = 0.7) +
  geom_text(aes(label = round(mean_dep, 3)),
            vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Départs moyens par zone géographique",
    subtitle = "Barres d'erreur : intervalle de confiance 95%",
    x        = NULL,
    y        = "Nombre moyen de départs (normalisé)",
    caption  = "Le centre-ville concentre significativement plus de départs"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey40", size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("outputs/figures/05_profil_zones.png",
       plot = p5, width = 9, height = 6, dpi = 180)
message("  -> Graphique 5 sauvegarde !")

# =============================================================
# RÉSUMÉ FINAL
# =============================================================
message("")
message("Tous les graphiques sont dans outputs/figures/ :")
message("  01_carte_chaleur_stations.png")
message("  02_comparaison_modeles.png")
message("  03_gain_krigeage.png")
message("  04_importance_variables.png")
message("  05_profil_zones.png")


