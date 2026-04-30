# R/evaluate.R
# Fonctions de métriques et comparaison de modèles
# Utilisées dans tous les scripts d'analyse

#' Calcul du RMSE
#' @param obs Valeurs observées
#' @param pred Valeurs prédites
#' @return RMSE (numeric)
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))


#' Calcul du MAE
#' @param obs Valeurs observées
#' @param pred Valeurs prédites
#' @return MAE (numeric)
mae <- function(obs, pred) mean(abs(obs - pred))


#' Résumé des performances train/validation/test pour un modèle
#'
#' @param nom Nom du modèle (string)
#' @param y_train, y_val, y_test Valeurs observées
#' @param pred_train, pred_val, pred_test Valeurs prédites
#' @return data.frame avec RMSE et MAE pour chaque partition
performance_summary <- function(nom,
                                y_train, pred_train,
                                y_val,   pred_val,
                                y_test,  pred_test) {
  data.frame(
    Modele     = nom,
    RMSE_train = rmse(y_train, pred_train),
    RMSE_val   = rmse(y_val,   pred_val),
    RMSE_test  = rmse(y_test,  pred_test),
    MAE_train  = mae(y_train,  pred_train),
    MAE_val    = mae(y_val,    pred_val),
    MAE_test   = mae(y_test,   pred_test),
    stringsAsFactors = FALSE
  )
}


#' Afficher un tableau comparatif de tous les modèles
#'
#' @param perf_list Liste de data.frames issus de performance_summary()
#' @return data.frame combiné, trié par RMSE_val croissant
compare_models <- function(perf_list) {
  combined <- do.call(rbind, perf_list)
  combined <- combined[order(combined$RMSE_val), ]
  message("Meilleur modèle : ", combined$Modele[1],
          " (RMSE_val = ", round(combined$RMSE_val[1], 5), ")")
  round(combined, 5)
}


#' Graphique y observé vs y prédit
#'
#' @param y_obs Valeurs observées
#' @param y_pred Valeurs prédites
#' @param titre Titre du graphique
#' @param couleur Couleur des points (défaut : steelblue)
#' @return ggplot object
plot_yvyhat <- function(y_obs, y_pred, titre, couleur = "steelblue") {
  df_plot <- data.frame(y_obs = y_obs, y_pred = y_pred)
  ggplot2::ggplot(df_plot, ggplot2::aes(x = y_obs, y = y_pred)) +
    ggplot2::geom_point(alpha = 0.3, color = couleur, size = 0.8) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         color = "red", linewidth = 0.8) +
    ggplot2::annotate("text", x = 0.05, y = 0.92,
                      label = paste0("RMSE = ", round(rmse(y_obs, y_pred), 4),
                                     "\nMAE  = ", round(mae(y_obs, y_pred), 4)),
                      hjust = 0, size = 3.2) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = titre, x = "y observé", y = "y prédit") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
}
