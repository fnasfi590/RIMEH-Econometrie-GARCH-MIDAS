# RIMEH - Partie 2 complète
# 1) Définir la clé EIA dans la session R
# Sys.setenv(EIA_API_KEY = "VOTRE_CLE_EIA")
# 2) Télécharger/préparer les données SVAR
source("scripts/02_download_svar_inputs_FINAL.R")
# 3) Estimer le SVAR et créer SUP/DEM/PRE
source("scripts/03_svar_oil_FINAL.R")
