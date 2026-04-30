# R/spatial.R
# Fonctions pour l'analyse spatiale : test de Moran, variogramme, krigeage
# Utilisées dans scripts/03_spatial.R

#' Test de Moran sur les résidus d'un modèle
#'
#' @param residus Vecteur de résidus agrégés par station
#' @param coords Matrix nx2 avec longitude et latitude des stations
#' @param k Nombre de voisins les plus proches (défaut : 5)
#' @return Liste avec moran_test et moran_mc (permutations)
test_moran <- function(residus, coords, k = 5) {
  lw <- spdep::nb2listw(
    spdep::knn2nb(spdep::knearneigh(coords, k = k)),
    style = "W"
  )
  moran_test <- spdep::moran.test(residus, lw)
  set.seed(123)
  moran_mc   <- spdep::moran.mc(residus, lw, nsim = 999)

  message("Test de Moran — Statistique I = ",
          round(moran_test$estimate[1], 4),
          " | p-value = ", round(moran_test$p.value, 6))

  list(test = moran_test, mc = moran_mc, lw = lw)
}


#' Visualiser le test de Moran par permutation
#'
#' @param moran_result Résultat de test_moran()
#' @return Graphique base R (histogramme + ligne observée)
plot_moran <- function(moran_result) {
  sim_vals <- moran_result$mc$res[-length(moran_result$mc$res)]
  i_obs    <- moran_result$mc$statistic
  hist(sim_vals,
       breaks = 40, col = "steelblue", border = "white",
       main = "Test de Moran par permutation (999 simulations)",
       xlab = "Statistique I de Moran simulée", ylab = "Fréquence",
       xlim = c(min(sim_vals) - 0.05, i_obs + 0.05))
  abline(v = i_obs, col = "red", lwd = 2, lty = 2)
  legend("topright",
         legend = paste("I observé =", round(i_obs, 3)),
         col = "red", lty = 2, lwd = 2, bty = "n")
}


#' Agréger les résidus par station et convertir en objet spatial
#'
#' @param df data.frame avec colonnes location, longitude, latitude
#' @param residus Vecteur de résidus (même longueur que nrow(df))
#' @return SpatialPointsDataFrame agrégé par station
aggregate_residuals <- function(df, residus) {
  resid_agg <- data.frame(
    location  = df$location,
    resid     = residus,
    longitude = df$longitude,
    latitude  = df$latitude
  ) %>%
    dplyr::group_by(location) %>%
    dplyr::summarise(
      resid     = mean(resid),
      longitude = dplyr::first(longitude),
      latitude  = dplyr::first(latitude),
      .groups   = "drop"
    ) %>%
    as.data.frame()
  sp::coordinates(resid_agg) <- ~longitude + latitude
  resid_agg
}


#' Ajuster un variogramme empirique et sélectionner le meilleur modèle
#'
#' @param resid_sp SpatialPointsDataFrame avec colonne resid
#' @return Liste avec modèle retenu et data.frame des SSE
fit_best_variogram <- function(resid_sp) {
  vg_emp <- gstat::variogram(resid ~ 1, resid_sp)

  vg_exp <- gstat::fit.variogram(vg_emp, gstat::vgm("Exp"))
  vg_sph <- gstat::fit.variogram(vg_emp, gstat::vgm("Sph"))
  vg_gau <- gstat::fit.variogram(vg_emp, gstat::vgm("Gau"))
  vg_mat <- gstat::fit.variogram(vg_emp, gstat::vgm("Mat"))

  sse_df <- data.frame(
    model = c("Exp", "Sph", "Gau", "Mat"),
    SSE   = c(attr(vg_exp, "SSErr"), attr(vg_sph, "SSErr"),
              attr(vg_gau, "SSErr"), attr(vg_mat, "SSErr"))
  )

  best_name <- sse_df$model[which.min(sse_df$SSE)]
  best_vg   <- switch(best_name,
                      "Exp" = vg_exp, "Sph" = vg_sph,
                      "Gau" = vg_gau, "Mat" = vg_mat)

  message("Meilleur modèle de variogramme : ", best_name,
          " (SSE = ", round(min(sse_df$SSE), 6), ")")

  list(vg_emp = vg_emp, best_model = best_vg,
       best_name = best_name, sse_df = sse_df)
}


#' Convertir un data.frame en SpatialPointsDataFrame (1 point par station)
#'
#' @param df data.frame avec colonnes location, longitude, latitude
#' @return SpatialPointsDataFrame
prep_spatial <- function(df) {
  out <- df %>%
    dplyr::group_by(location) %>%
    dplyr::summarise(
      longitude = dplyr::first(longitude),
      latitude  = dplyr::first(latitude),
      .groups   = "drop"
    ) %>%
    as.data.frame()
  sp::coordinates(out) <- ~longitude + latitude
  out
}


#' Appliquer le Regression Kriging (LASSO + krigeage des résidus)
#'
#' @param pred_lasso_val, pred_lasso_test Prédictions LASSO
#' @param resid_sp Résidus agrégés (SpatialPointsDataFrame)
#' @param val_sp, test_sp Localisations cibles (SpatialPointsDataFrame)
#' @param vg_model Modèle de variogramme ajusté
#' @param validation, test data.frames originaux (pour le match de locations)
#' @param type "OK", "SK", ou "UK"
#' @return Liste avec pred_val et pred_test après krigeage
apply_kriging <- function(pred_lasso_val, pred_lasso_test,
                          resid_sp, val_sp, test_sp,
                          vg_model, validation, test,
                          type = "SK") {

  if (type == "OK") {
    kriged_val  <- gstat::krige(resid ~ 1, resid_sp, val_sp,
                                model = vg_model, debug.level = 0)
    kriged_test <- gstat::krige(resid ~ 1, resid_sp, test_sp,
                                model = vg_model, debug.level = 0)

  } else if (type == "SK") {
    kriged_val  <- gstat::krige(resid ~ 1, resid_sp, val_sp,
                                model = vg_model, beta = 0, debug.level = 0)
    kriged_test <- gstat::krige(resid ~ 1, resid_sp, test_sp,
                                model = vg_model, beta = 0, debug.level = 0)

  } else if (type == "UK") {
    # Krigeage Universel — tendance modélisée par lat/long
    coords_resid        <- as.data.frame(sp::coordinates(resid_sp))
    names(coords_resid) <- c("longitude", "latitude")
    resid_uk            <- as.data.frame(resid_sp)
    resid_uk$longitude  <- coords_resid$longitude
    resid_uk$latitude   <- coords_resid$latitude
    sp::coordinates(resid_uk) <- ~longitude + latitude

    val_uk  <- prep_spatial(validation)
    test_uk <- prep_spatial(test)

    kriged_val  <- gstat::krige(resid ~ longitude + latitude,
                                resid_uk, val_uk,
                                model = vg_model, debug.level = 0)
    kriged_test <- gstat::krige(resid ~ longitude + latitude,
                                resid_uk, test_uk,
                                model = vg_model, debug.level = 0)
    val_sp  <- val_uk
    test_sp <- test_uk
  }

  pred_val  <- pred_lasso_val +
    kriged_val$var1.pred[match(validation$location, val_sp$location)]
  pred_test <- pred_lasso_test +
    kriged_test$var1.pred[match(test$location, test_sp$location)]

  list(pred_val = pred_val, pred_test = pred_test)
}
