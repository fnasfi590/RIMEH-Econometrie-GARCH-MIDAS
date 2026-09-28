# RIMEH — Partie 3 : GARCH, GARCH-MIDAS et hétérogénéité

## Objectif
Cette partie prend les rendements quotidiens des 16 marchés et les combine avec les variables mensuelles GPRT/GPRA, VIX, contrôles macroéconomiques et les trois chocs pétroliers issus du SVAR de Kilian (`SUP`, `DEM`, `PRE`).

## Ordre d'exécution
Depuis la racine `RIMEH_ECONOMETRIE` :

```r
source("RIMEH_PART3_MASTER.R")
```

Le master lance :

1. `06_prepare_part3_data.R` — fusion et audit des données ;
2. `07_garch_full.R` — GARCH(1,1) Student-t pour les 16 pays ;
3. `08_garch_midas_models.R` — GARCH-MIDAS parsimonieux avec une ou deux variables MIDAS ;
4. `09_panel_long_run.R` — équation long-run multivariée à effets fixes, complémentaire ;
5. `10_robustness.R` — interactions, asymétrie, importateurs/exportateurs.

## Modèles
### GARCH
`σ²_t = ω + α ε²_(t-1) + β σ²_(t-1)`.

### GARCH-MIDAS
`σ²_t = τ_m g_t`, avec une composante courte fréquence et une composante longue fréquence expliquée par des variables mensuelles. `rumidas` est utilisé pour les spécifications à une/deux variables MIDAS.

### Interaction géopolitique-pétrole
`GPRT_SUP = GPRT × SUP`.

### Hétérogénéité
`SUP_EXPORTER = SUP × exporter`.

### Asymétrie
`SUP_positive = max(SUP,0)` et `SUP_negative = min(SUP,0)`.

## Interprétation à ne pas automatiser
- `SUP > 0` = innovation positive d'offre ; une rupture/réduction inattendue de l'offre est `SUP < 0`.
- Un coefficient positif de `GPRT_SUP` signifie que l'association entre un choc d'offre positif et la volatilité augmente avec le GPRT. L'interprétation économique doit tenir compte du signe de `SUP` et de la convention d'identification.
- Les coefficients du panel long-run ne doivent pas être présentés comme les coefficients d'une unique équation GARCH-MIDAS multivariée.

## Sorties principales
- `results/models/garch_all_countries.rds`
- `results/models/garch_midas/*.rds`
- `results/models/panel_long_run_models.rds`
- `results/tables/garch_coefficients.csv`
- `results/tables/garch_persistence.csv`
- `results/tables/garch_midas_run_audit.csv`
- `results/tables/panel_long_run_coefficients.csv`
- `results/tables/robustness_coefficients.csv`

## Conditions
La Partie 3 suppose que la Partie 1 et la Partie 2 ont déjà été exécutées et que `data/oil/oil_structural_shocks.csv` existe. Les valeurs manquantes ne sont jamais remplacées par des nombres inventés.
