# RIMEH — Partie 1 : données réelles 2000–2025

## Exécution
Depuis le dossier `RIMEH_ECONOMETRIE` :

```r
source("scripts/01_download_clean_real_data.R")
```

Le script télécharge et nettoie :
- indices boursiers quotidiens des 16 pays via Yahoo Finance;
- rendements quotidiens et volatilité réalisée mensuelle;
- VIX quotidien via FRED;
- WTI quotidien via FRED;
- IGREA mensuel via FRED;
- GPRT et GPRA mensuels via le fichier officiel Caldara-Iacoviello;
- PIB/inflation annuels via World Bank WDI.

## Important
Le script ne remplace aucune donnée manquante par une valeur inventée. Les échecs de ticker et les périodes disponibles sont consignés dans `data/raw/download_audit.csv`.

La production mondiale mensuelle de pétrole n'est pas fabriquée ici : elle sera ajoutée dans la prochaine étape du SVAR depuis une source documentée.

Le PIB et l'inflation WDI sont annuels. Ils ne sont pas artificiellement dupliqués sur 12 mois. Leur passage vers le long terme GARCH-MIDAS doit être traité explicitement à l'étape économétrique.
