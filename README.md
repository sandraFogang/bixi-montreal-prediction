# Prédiction du flux de cyclistes BIXI à Montréal
### Machine Learning & Régression Krigeage — Consultant Ville de Montréal

![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r)
![Status](https://img.shields.io/badge/status-completed-brightgreen)
![License](https://img.shields.io/badge/license-MIT-green)

---

## Objectif

Dans le cadre d'un mandat simulé pour la **Ville de Montréal**, ce projet 
vise à prédire le **nombre de départs journaliers BIXI par station** afin de :

- optimiser l'allocation des vélos
- anticiper les pics de demande
- planifier le développement du réseau de pistes cyclables

---

## Données

- **587 stations BIXI**
- **196 jours (avril - octobre 2019)**
- **~115 000 observations**
- **17 variables explicatives :**
  - 12 variables spatiales (infrastructures, urbanisme)
  - 5 variables temporelles (météo, saison)

---

## Méthodologie

### 1. Modèles classiques (baseline ML)

Hypothèse : indépendance des observations.

- Régression linéaire (Stepwise AIC/BIC)
- LASSO / Elastic Net
- Random Forest
- Gradient Boosting (GBM)
- CART / Ctree

### 2. Analyse spatiale

Test de Moran sur les résidus des modèles classiques → détection d'une 
**forte autocorrélation spatiale**.  
Conclusion : les modèles classiques sont insuffisants — une modélisation 
spatiale explicite est nécessaire.

### 3. Modèle final — Regression Kriging

Approche hybride en deux composantes :

- **Partie déterministe** : LASSO (sélection de variables, validation 
  croisée 10-fold)
- **Partie spatiale** : krigeage des résidus via ajustement d'un 
  variogramme

$$y = f(X) + \varepsilon_{spatial}$$

Trois variantes de krigeage comparées : Ordinaire (OK), Simple (SK), 
Universel (UK).

---

## Résultats clés

| Modèle | Performance |
|--------|-------------|
| Modèles classiques (Stepwise, LASSO, RF, GBM) | Bonne performance individuelle mais résidus spatialement corrélés |
| **LASSO + Krigeage Simple (SK)** | **Meilleure performance** — capture la dépendance spatiale résiduelle |

Le test de Moran confirme une autocorrélation spatiale significative dans 
les résidus des modèles classiques. Le Regression Kriging réduit cette 
dépendance et améliore la précision des prédictions.

**Modèle retenu : LASSO + Krigeage Simple (SK)**

---

## Insights principaux

- Forte **dépendance spatiale** entre stations proches
- La **température** est le facteur temporel dominant
- Le **walkscore et la densité urbaine** sont les variables spatiales 
  les plus influentes
- Le centre-ville concentre la majorité des départs

---

## Structure du repo