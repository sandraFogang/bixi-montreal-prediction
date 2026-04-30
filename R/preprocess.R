# R/preprocess.R
# Fonctions de nettoyage, transformation et préparation des données BIXI
# Appelé par tous les scripts d'analyse

#' Charger et préparer les données BIXI brutes
#'
#' @param path Chemin vers le fichier BIXI.RData
#' @return Liste avec Bixi_datar (observations connues) et Bixi_a_predire
load_bixi <- function(path = "data/raw/BIXI.RData") {
  load(path, envir = .GlobalEnv)
  message("Données BIXI chargées : ", nrow(Bixi_data), " observations.")
  list(
    bixi    = Bixi_data,
    spatial = Spatial_positions
  )
}


#' Créer les variables temporelles (weekend, saison)
#'
#' @param df data.frame contenant une colonne `time`
#' @return df enrichi avec jour, mois, weekend, saison
add_time_features <- function(df) {
  df$jour    <- lubridate::wday(df$time)
  df$mois    <- lubridate::month(df$time)
  df$weekend <- as.factor(ifelse(df$jour >= 6, "weekend", "semaine"))
  df$saison  <- factor(ifelse(df$mois <= 5, "Printemps",
                       ifelse(df$mois <= 8, "Eté", "Automne")))
  df$holiday <- as.factor(df$holiday)
  df
}


#' Standardisation min-max des coordonnées lat/long
#'
#' @param spatial_df data.frame avec colonnes latitude et longitude
#' @return spatial_df avec colonnes latitude et longitude normalisées entre 0 et 1
normalize_coords <- function(spatial_df) {
  spatial_df$latitude  <- (spatial_df$latitude  - min(spatial_df$latitude)) /
    (max(spatial_df$latitude)  - min(spatial_df$latitude))
  spatial_df$longitude <- (spatial_df$longitude - min(spatial_df$longitude)) /
    (max(spatial_df$longitude) - min(spatial_df$longitude))
  spatial_df
}


#' Clustering géographique des stations (hiérarchique Ward.D2, k=5)
#'
#' @param spatial_df data.frame avec colonnes latitude et longitude
#' @param k Nombre de clusters (défaut : 5)
#' @return spatial_df avec colonne clusters_geo ajoutée
add_geo_clusters <- function(spatial_df, k = 5) {
  geo    <- spatial_df[, c("latitude", "longitude")]
  hc_geo <- hclust(dist(geo), method = "ward.D2")
  spatial_df$clusters_geo <- as.factor(cutree(hc_geo, k = k))
  message("Clustering géographique : ", k, " zones créées.")
  spatial_df
}


#' Transformations additionnelles des variables
#'
#' @param df data.frame principal
#' @return df avec variables transformées
transform_features <- function(df) {
  # num_university : binaire (présence/absence)
  df$num_university <- as.factor(ifelse(df$num_university > 0, 1, 0))
  # total_precip_mm : distribution asymétrique → log-transformation
  df$total_precip_mm <- log(df$total_precip_mm + 0.01)
  df
}


#' Pipeline complet de préparation des données
#'
#' @param path Chemin vers BIXI.RData
#' @return Liste avec train, validation, test et Bixi_a_predire
prepare_data <- function(path = "data/raw/BIXI.RData") {

  # 1. Chargement
  raw <- load_bixi(path)
  bixi    <- raw$bixi
  spatial <- raw$spatial

  # 2. Séparer observations connues vs à prédire
  Bixi_a_predire <- bixi[is.na(bixi$nb_departure), ]
  Bixi_datar     <- bixi[!is.na(bixi$nb_departure), ]

  # 3. Variables temporelles
  Bixi_datar <- add_time_features(Bixi_datar)

  # 4. Clustering géographique + normalisation coordonnées
  spatial <- normalize_coords(spatial)
  spatial <- add_geo_clusters(spatial, k = 5)

  # 5. Jointure données + spatial
  Bixi_util <- Bixi_datar %>%
    dplyr::select(-mois, -jour, -time) %>%
    dplyr::left_join(spatial, by = "location")

  # 6. Transformations
  Bixi_util <- transform_features(Bixi_util)

  # 7. Stations avec peu d'observations (forcées dans le train)
  obs_par_station  <- Bixi_util %>%
    dplyr::group_by(location) %>%
    dplyr::summarise(n_obs = n(), .groups = "drop")
  stations_peu_obs <- obs_par_station %>%
    dplyr::filter(n_obs < 30) %>%
    dplyr::pull(location)

  # 8. Split stratifié 80/10/10 au niveau station
  set.seed(123)
  df       <- as.data.frame(Bixi_util)
  df_split <- df %>% dplyr::filter(!location %in% stations_peu_obs)

  loc_df <- df_split %>%
    dplyr::group_by(location, clusters_geo) %>%
    dplyr::summarise(nb = n(), .groups = "drop") %>%
    dplyr::group_by(location) %>%
    dplyr::slice_max(nb, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(cluster_majoritaire = clusters_geo)

  train_idx  <- caret::createDataPartition(loc_df$cluster_majoritaire,
                                           p = 0.8, list = FALSE)
  train_locs <- loc_df$location[train_idx]
  temp       <- loc_df[-train_idx, ]
  val_idx    <- caret::createDataPartition(temp$cluster_majoritaire,
                                           p = 0.5, list = FALSE)
  val_locs   <- temp$location[val_idx]
  test_locs  <- temp$location[-val_idx]

  train      <- df[df$location %in% c(train_locs, stations_peu_obs), ]
  validation <- df[df$location %in% val_locs, ]
  test       <- df[df$location %in% test_locs, ]

  message("Split terminé — Train: ", nrow(train),
          " | Val: ", nrow(validation),
          " | Test: ", nrow(test))

  list(
    train          = train,
    validation     = validation,
    test           = test,
    Bixi_a_predire = Bixi_a_predire,
    spatial        = spatial
  )
}
