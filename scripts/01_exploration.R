# =============================================================================
# scripts/01_exploration.R
# Analyse exploratoire des données BIXI
# -----------------------------------------------------------------------------
# Auteure  : Sandra Desmair Fogang Lontouo
# Projet   : Prédiction du flux de cyclistes BIXI — Montréal 2019
# Sortie   : graphiques dans outputs/figures/
# Prérequis: packages.R, R/preprocess.R
# =============================================================================

source("packages.R")
source("R/preprocess.R")

# Chargement brut pour l'EDA (avant nettoyage complet)
load("data/raw/BIXI.RData")

Bixi_datar <- Bixi_data[!is.na(Bixi_data$nb_departure), ]
Bixi_datar <- add_time_features(Bixi_datar)

# Thème commun
small_theme <- theme_minimal(base_size = 8) +
  theme(axis.text  = element_text(size = 7),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 9))

# --- Distributions des variables clés ---
p1 <- ggplot(Bixi_datar, aes(nb_departure)) +
  geom_histogram(fill = "tomato", bins = 30) +
  labs(x = "Nombre de départs", y = "Fréquence") + small_theme

p2 <- ggplot(Bixi_datar, aes(mean_temp_c)) +
  geom_histogram(fill = "skyblue", bins = 30) +
  labs(x = "Température moyenne (°C)", y = "Fréquence") + small_theme

p3 <- ggplot(Bixi_datar, aes(total_precip_mm)) +
  geom_histogram(fill = "gold", bins = 30) +
  labs(x = "Précipitations totales (mm)", y = "Fréquence") + small_theme

p_hist <- p1 | p2 | p3
ggsave("outputs/figures/distributions_variables.png",
       plot = p_hist, width = 12, height = 4, dpi = 150)

# --- Départs par jour de semaine vs weekend ---
p4 <- ggplot(Bixi_datar, aes(x = factor(jour), y = nb_departure)) +
  geom_boxplot(fill = "lightgreen") + small_theme +
  labs(x = "Jour de la semaine", y = "Nombre de départs")

p5 <- ggplot(Bixi_datar, aes(x = weekend, y = nb_departure)) +
  geom_boxplot(fill = "skyblue") + small_theme +
  labs(x = "Semaine vs Weekend", y = "Nombre de départs")

ggsave("outputs/figures/departs_par_jour.png",
       plot = p4 | p5, width = 10, height = 4, dpi = 150)

# --- Départs par saison ---
p6 <- ggplot(Bixi_datar, aes(x = saison, y = nb_departure)) +
  geom_boxplot(fill = "seagreen") + small_theme +
  labs(x = "Saison", y = "Nombre de départs")

ggsave("outputs/figures/departs_par_saison.png",
       plot = p6, width = 6, height = 4, dpi = 150)

# --- Carte interactive des clusters géographiques ---
spatial <- normalize_coords(Spatial_positions)
spatial <- add_geo_clusters(spatial, k = 5)

geo_cluster <- spatial %>%
  dplyr::select(location, latitude, longitude, clusters_geo)

palette_clusters <- leaflet::colorFactor(
  palette = "Set1",
  domain  = as.factor(geo_cluster$clusters_geo)
)

carte <- leaflet::leaflet(geo_cluster) %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addCircleMarkers(
    lng         = ~longitude,
    lat         = ~latitude,
    radius      = 5,
    color       = ~palette_clusters(as.factor(clusters_geo)),
    stroke      = FALSE,
    fillOpacity = 0.8,
    popup       = ~paste("<b>Station:</b>", location,
                         "<br><b>Cluster:</b>", clusters_geo)
  ) %>%
  leaflet::addLegend(
    "bottomright",
    pal    = palette_clusters,
    values = ~as.factor(clusters_geo),
    title  = "Zone géographique",
    opacity = 1
  )

# Sauvegarder la carte en HTML
htmlwidgets::saveWidget(carte,
                        "outputs/figures/carte_clusters.html",
                        selfcontained = TRUE)

message("Exploration terminée. Graphiques sauvegardés dans outputs/figures/")
