# RIMEH – Base de données + code R

## Périmètre
- 16 pays
- 2000–2025
- Rendements boursiers quotidiens
- GPR mensuel
- Chocs pétroliers structurels
- Contrôles macro-financiers
- GARCH / GARCH-MIDAS / panel de robustesse

## Fichiers à remplir

### 1. data/raw/daily_data.csv
Colonnes:
date,country,stock_price,return,RV,exporter

`stock_price` est la seule colonne qui doit être fournie au départ;
le script calcule `return` et `RV`.

### 2. data/raw/monthly_data.csv
Colonnes:
date,country,GPRT,GPRA,SUP,DEM,PRE,VIX,interest_rate,exchange_rate,GDP,inflation,exporter

Les colonnes SUP/DEM/PRE sont à remplir après l'identification SVAR validée.

### 3. data/oil/oil_svar_input.csv
Colonnes:
date,global_activity,global_oil_production,real_oil_price

## Sources à documenter dans le mémoire
- GPR: Caldara & Iacoviello, site officiel du GPR.
- GPRT = threats; GPRA = acts.
- Prix/production pétrole: série retenue et documentation.
- Activité mondiale: série Kilian / source retenue.
- Marchés boursiers: source homogène sur les 16 pays.
- VIX: CBOE/FRED.
- Taux, change, inflation, PIB: sources nationales/FRED/Banque mondiale/FMI selon disponibilité.

## Attention méthodologique
Le script `03_svar_oil.R` contient une identification récursive uniquement comme
PLACEHOLDER technique. Elle ne doit pas être présentée comme l'identification
finale de Kilian dans le mémoire. Les restrictions structurelles finales doivent
être fixées avant d'extraire SUP, DEM et PRE.

## GARCH-MIDAS
Le package CRAN `rumidas` est utilisé pour la partie GARCH-MIDAS. Il accepte
notamment GM, GM2M, GMX, DAGM et DAGMX. Pour une équation longue avec de nombreux
régresseurs mensuels, le projet combine des estimations GARCH-MIDAS ciblées et
une équation de long terme en panel comme robustesse/complément.

## Ordre d'exécution
1. Remplir les CSV.
2. `01_prepare_data.R`
3. `02_garch.R`
4. Valider l'identification SVAR puis produire SUP/DEM/PRE.
5. `03_svar_oil.R`
6. `04_garch_midas.R`
7. `05_panel_long_run.R`

## Classification initiale
La classification des 16 pays suit le fichier de spécification du projet.
Elle doit être documentée et vérifiée avec une définition cohérente du statut
exportateur/importateur sur 2000–2025, en particulier pour les pays dont le statut
a changé au cours de la période.
