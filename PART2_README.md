# RIMEH — Partie 2 : SVAR pétrolier de Kilian

## Objectif

Construire les trois chocs pétroliers utilisés ensuite dans le GARCH-MIDAS :

- `SUP` : choc d'offre pétrolière
- `DEM` : choc de demande globale
- `PRE` : choc de demande spécifique au pétrole (oil-specific / precautionary demand)

## Base méthodologique

Le modèle suit Kilian (2009), *Not All Oil Price Shocks Are Alike*, American Economic Review, 99(3), 1053–1069.

Le vecteur mensuel est :

`z_t = (Delta oil production_t, global real activity_t, Delta real oil price_t)'`

L'identification contemporaine est récursive :

`A0 = [1 0 0; a21 1 0; a31 a32 1]`

Le premier choc est interprété comme un choc d'offre, le deuxième comme un choc de demande globale et le troisième comme un choc de demande spécifique au pétrole.

## Sources

- EIA : `INTL.57-1-WORL-TBPD.M`, World crude oil production, monthly.
- FRED/Dallas Fed : `IGREA`, updated Kilian index of global real economic activity.
- FRED : `CPIAUCSL` pour le déflateur.
- Prix : priorité au coût d'acquisition des raffineurs si le fichier EIA local est fourni ; sinon WTI FRED comme fallback transparent.

## Exécution

1. Obtenir une clé API EIA gratuite.
2. Dans R/RStudio :

```r
Sys.setenv(EIA_API_KEY = "VOTRE_CLE_EIA")
source("scripts/02_download_svar_inputs_FINAL.R")
source("scripts/03_svar_oil_FINAL.R")
```

3. Vérifier :

- `results/tables/SVAR_data_audit.csv`
- `results/tables/SVAR_structural_summary.txt`
- `results/tables/SVAR_serial_test.txt`
- `data/oil/oil_structural_shocks.csv`

## Sortie principale

`data/oil/oil_structural_shocks.csv`

avec :

`date | SUP | DEM | PRE`

## Convention de signe

`SUP > 0` = innovation positive de l'offre pétrolière.

Une perturbation/réduction inattendue de l'offre est donc un choc `SUP < 0`.

Cette convention doit rester identique dans le GARCH-MIDAS et dans l'interprétation du mémoire.
