# Données — Source et accès

## Données utilisées dans ce projet

### BIXI.RData (données du cours — non redistribuables)

Les données `BIXI.RData` sont fournies dans le cadre du cours  
**MATH 70611 — Méthodes avancées en exploitation de données, HEC Montréal**.

Elles ne peuvent pas être redistribuées publiquement.

Le fichier contient deux objets R :

| Objet | Description |
|-------|-------------|
| `Bixi_data` | Nombre de départs journaliers pour 587 stations BIXI sur 196 jours (avril–octobre 2019). Inclut 17 prédicteurs (12 spatiaux + 5 temporels). Toutes les variables sont normalisées entre 0 et 1 (min-max). |
| `Spatial_positions` | Coordonnées GPS (latitude, longitude) des 587 stations BIXI. |

---

## Données BIXI publiques

Pour reproduire une analyse similaire avec des données publiques :

- **Données ouvertes BIXI Montréal :**  
  https://bixi.com/fr/donnees-ouvertes

- **Portail données ouvertes Ville de Montréal :**  
  https://donnees.montreal.ca/

Ces données publiques couvrent les trajets individuels (origine, destination,
durée) et non les départs agrégés par station, mais permettent de reconstituer
une variable similaire.
