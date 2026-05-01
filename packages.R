# =============================================================================
# packages.R
# Installation et chargement de tous les packages nécessaires
# -----------------------------------------------------------------------------
# Auteure  : Sandra Desmair Fogang Lontouo
# Projet   : Prédiction du flux de cyclistes BIXI — Montréal 2019
# Usage    : source("packages.R") — à exécuter en premier
# =============================================================================

packages <- c(
  # Manipulation de données
  "dplyr", "lubridate", "tidyr", "fastDummies",

  # Visualisation
  "ggplot2", "patchwork", "RColorBrewer", "leaflet",

  # Modèles ML
  "glmnet", "rpart", "rpart.plot", "party",
  "randomForest", "gbm", "MASS", "adabag", "caret",

  # Analyse spatiale
  "spdep", "sp", "gstat",

  # Traitement parallèle
  "doParallel", "parallel", "foreach"
)

# Installer les packages manquants
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(packages, install_if_missing))

# Charger tous les packages
invisible(lapply(packages, library, character.only = TRUE))

message("Tous les packages sont chargés avec succès.")
