# packages.R — Installation et chargement de tous les packages nécessaires
# Lancer ce fichier en premier : source("packages.R")

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
