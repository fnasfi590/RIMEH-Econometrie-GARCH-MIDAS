# ============================================================
# RIMEH - FINALISATION COMPLETE
# Coefficients + H1-H9 + Diagnostics + Synthese + Figures
# ============================================================

options(stringsAsFactors = FALSE)

root <- normalizePath(".", winslash="/", mustWork=TRUE)

dir.create("results/analysis/RIMEH_FINAL/15_FINALISATION", recursive=TRUE, showWarnings=FALSE)
out <- "results/analysis/RIMEH_FINAL/15_FINALISATION"

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(openxlsx)
  library(ggplot2)
})

cat("\n============================================================\n")
cat(" RIMEH - FINALISATION COMPLETE\n")
cat("============================================================\n")

# ------------------------------------------------------------
# 1. COEFFICIENTS DES 76 MODELES
# ------------------------------------------------------------

coef_file <- "results/analysis/RIMEH_FINAL/07_COEFFICIENTS_76_MODELES.csv"

coef <- read.csv(coef_file, check.names=FALSE)

cat("\n[1] Coefficients :", nrow(coef), "lignes\n")
cat("Colonnes :", paste(names(coef), collapse=" | "), "\n")

# Normalisation des noms
names(coef) <- make.names(names(coef), unique=TRUE)

# Recherche automatique des colonnes
model_col <- names(coef)[grepl("^Modele$|model", names(coef), ignore.case=TRUE)][1]
param_col <- names(coef)[grepl("Param", names(coef), ignore.case=TRUE)][1]
est_col   <- names(coef)[grepl("Estimate|Estimation", names(coef), ignore.case=TRUE)][1]
se_col    <- names(coef)[grepl("Std.*Error|SE", names(coef), ignore.case=TRUE)][1]
t_col     <- names(coef)[grepl("t.*value|Statistique", names(coef), ignore.case=TRUE)][1]
p_col     <- names(coef)[grepl("Pr|P.value|P_value", names(coef), ignore.case=TRUE)][1]

if(any(is.na(c(model_col,param_col,est_col,se_col,t_col,p_col)))) {
  stop("Colonnes coefficient introuvables. Vérifier 07_COEFFICIENTS_76_MODELES.csv")
}

coef2 <- coef %>%
  transmute(
    Modele = .data[[model_col]],
    Parametre = .data[[param_col]],
    Estimate = as.numeric(.data[[est_col]]),
    SE = as.numeric(.data[[se_col]]),
    Statistique = as.numeric(.data[[t_col]]),
    P_value = as.numeric(.data[[p_col]])
  ) %>%
  mutate(
    Pays = substr(Modele,1,3),
    Specification = sub("^[A-Z]{3}_","",Modele),
    Sig_10 = P_value < .10,
    Sig_05 = P_value < .05,
    Sig_01 = P_value < .01
  )

write.csv(
  coef2,
  file.path(out,"15_COEFFICIENTS_76_MODELES_COMPLETS.csv"),
  row.names=FALSE,
  na="."
)

# ------------------------------------------------------------
# 2. PARAMETRES MIDAS
# ------------------------------------------------------------

midas <- coef2 %>%
  filter(Parametre %in% c("theta","theta_1","theta_2")) %>%
  arrange(Pays, Specification, Parametre)

write.csv(
  midas,
  file.path(out,"16_PARAMETRES_MIDAS_76_MODELES.csv"),
  row.names=FALSE,
  na="."
)

# Tableau large
midas_wide <- midas %>%
  select(Modele,Pays,Specification,Parametre,Estimate,SE,Statistique,P_value) %>%
  pivot_wider(
    names_from=Parametre,
    values_from=c(Estimate,SE,Statistique,P_value),
    names_sep="_"
  )

write.csv(
  midas_wide,
  file.path(out,"17_SYNTHESE_PARAMETRES_MIDAS.csv"),
  row.names=FALSE,
  na="."
)

# ------------------------------------------------------------
# 3. SIGNIFICATIVITE DES PARAMETRES
# ------------------------------------------------------------

sig_summary <- coef2 %>%
  group_by(Parametre) %>%
  summarise(
    N=n(),
    Min=min(Estimate,na.rm=TRUE),
    Median=median(Estimate,na.rm=TRUE),
    Mean=mean(Estimate,na.rm=TRUE),
    Max=max(Estimate,na.rm=TRUE),
    SD=sd(Estimate,na.rm=TRUE),
    Sig_10=sum(P_value<.10,na.rm=TRUE),
    Sig_05=sum(P_value<.05,na.rm=TRUE),
    Sig_01=sum(P_value<.01,na.rm=TRUE),
    .groups="drop"
  )

write.csv(
  sig_summary,
  file.path(out,"18_SIGNIFICATIVITE_PARAMETRES.csv"),
  row.names=FALSE
)

# ------------------------------------------------------------
# 4. SYNTHESE PAR PAYS
# ------------------------------------------------------------

country_summary <- coef2 %>%
  group_by(Pays,Parametre) %>%
  summarise(
    N=n(),
    Sig_05=sum(P_value<.05,na.rm=TRUE),
    Sig_10=sum(P_value<.10,na.rm=TRUE),
    Estimate_median=median(Estimate,na.rm=TRUE),
    Estimate_mean=mean(Estimate,na.rm=TRUE),
    P_median=median(P_value,na.rm=TRUE),
    .groups="drop"
  )

write.csv(
  country_summary,
  file.path(out,"19_SYNTHESE_COEFFICIENTS_PAR_PAYS.csv"),
  row.names=FALSE
)

# ------------------------------------------------------------
# 5. SYNTHESE PAR SPECIFICATION
# ------------------------------------------------------------

spec_summary <- coef2 %>%
  group_by(Specification,Parametre) %>%
  summarise(
    N=n(),
    Sig_05=sum(P_value<.05,na.rm=TRUE),
    Sig_10=sum(P_value<.10,na.rm=TRUE),
    Estimate_median=median(Estimate,na.rm=TRUE),
    Estimate_mean=mean(Estimate,na.rm=TRUE),
    P_median=median(P_value,na.rm=TRUE),
    .groups="drop"
  )

write.csv(
  spec_summary,
  file.path(out,"20_SYNTHESE_COEFFICIENTS_PAR_SPECIFICATION.csv"),
  row.names=FALSE
)

# ------------------------------------------------------------
# 6. DIAGNOSTICS POST-ESTIMATION
# ------------------------------------------------------------

diag_file <- "results/analysis/RIMEH_FINAL/14_DIAGNOSTICS_POST_ESTIMATION_76.csv"

diag <- read.csv(diag_file, check.names=FALSE)

diag <- diag %>%
  mutate(
    Pays=substr(Modele,1,3),
    Specification=sub("^[A-Z]{3}_","",Modele),
    LB_sig=LB_residus_p < .05,
    LB2_sig=LB_residus2_p < .05,
    ARCH_sig=ARCH_LM_p < .05
  )

diag_country <- diag %>%
  group_by(Pays) %>%
  summarise(
    Modeles=n(),
    LB_residus_sig=sum(LB_sig,na.rm=TRUE),
    LB_residus2_sig=sum(LB2_sig,na.rm=TRUE),
    ARCH_LM_sig=sum(ARCH_sig,na.rm=TRUE),
    Mean_residus_median=median(Mean_residus,na.rm=TRUE),
    SD_residus_median=median(SD_residus,na.rm=TRUE),
    .groups="drop"
  )

write.csv(
  diag_country,
  file.path(out,"21_DIAGNOSTICS_PAR_PAYS.csv"),
  row.names=FALSE
)

diag_spec <- diag %>%
  group_by(Specification) %>%
  summarise(
    Modeles=n(),
    LB_residus_sig=sum(LB_sig,na.rm=TRUE),
    LB_residus2_sig=sum(LB2_sig,na.rm=TRUE),
    ARCH_LM_sig=sum(ARCH_sig,na.rm=TRUE),
    Mean_residus_median=median(Mean_residus,na.rm=TRUE),
    SD_residus_median=median(SD_residus,na.rm=TRUE),
    .groups="drop"
  )

write.csv(
  diag_spec,
  file.path(out,"22_DIAGNOSTICS_PAR_SPECIFICATION.csv"),
  row.names=FALSE
)

# Cas atypiques
atyp <- diag %>%
  filter(
    abs(Mean_residus) > 0.10 |
    SD_residus > 2 |
    SD_residus < 0.5 |
    is.infinite(Mean_residus) |
    is.infinite(SD_residus)
  )

write.csv(
  atyp,
  file.path(out,"23_CAS_ATYPIQUES_DIAGNOSTICS.csv"),
  row.names=FALSE
)

# ------------------------------------------------------------
# 7. H1-H9 EXISTANTS
# ------------------------------------------------------------

hfile <- "results/analysis/FINAL_TABLEAU_H1_H9_FORMEL_V2.csv"

if(file.exists(hfile)) {

  h <- read.csv(hfile, check.names=FALSE)

  write.csv(
    h,
    file.path(out,"24_H1_H9_COMPLET_165_LIGNES.csv"),
    row.names=FALSE
  )

  # Recherche automatique de la colonne hypothèse
  hcol <- names(h)[grepl("Hypothese|Hypothesis",names(h),ignore.case=TRUE)][1]
  scol <- names(h)[grepl("Statut|Status",names(h),ignore.case=TRUE)][1]

  if(!is.na(hcol)) {

    if(!is.na(scol)) {
      h_summary <- h %>%
        group_by(.data[[hcol]], .data[[scol]]) %>%
        summarise(N=n(),.groups="drop")

      write.csv(
        h_summary,
        file.path(out,"25_H1_H9_SYNTHESE_STATUT.csv"),
        row.names=FALSE
      )
    }

    h_counts <- h %>%
      group_by(.data[[hcol]]) %>%
      summarise(
        N=n(),
        .groups="drop"
      )

    write.csv(
      h_counts,
      file.path(out,"26_H1_H9_NOMBRE_LIGNES.csv"),
      row.names=FALSE
    )
  }

} else {
  warning("Fichier H1-H9 introuvable")
}

# ------------------------------------------------------------
# 8. MODELES / AIC / BIC / LOGLIK
# ------------------------------------------------------------

model_file <- "results/analysis/RIMEH_FINAL/06_MODELES_76.csv"

if(file.exists(model_file)) {

  models <- read.csv(model_file,check.names=FALSE)

  write.csv(
    models,
    file.path(out,"27_MODELES_76_COMPLETS.csv"),
    row.names=FALSE
  )
}

# ------------------------------------------------------------
# 9. STATISTIQUES GLOBALES
# ------------------------------------------------------------

global_diag <- data.frame(
  Indicateur=c(
    "Modeles",
    "Diagnostics_OK",
    "Erreurs",
    "LB_residus_significatifs",
    "LB_residus2_significatifs",
    "ARCH_LM_significatifs",
    "Median_mean_residus",
    "Median_SD_residus"
  ),
  Valeur=c(
    nrow(diag),
    sum(diag$Statut=="OK",na.rm=TRUE),
    sum(diag$Statut!="OK",na.rm=TRUE),
    sum(diag$LB_sig,na.rm=TRUE),
    sum(diag$LB2_sig,na.rm=TRUE),
    sum(diag$ARCH_sig,na.rm=TRUE),
    median(diag$Mean_residus,na.rm=TRUE),
    median(diag$SD_residus,na.rm=TRUE)
  )
)

write.csv(
  global_diag,
  file.path(out,"28_SYNTHESE_GLOBALE.csv"),
  row.names=FALSE
)

# ------------------------------------------------------------
# 10. FIGURE 1 - SIGNIFICATIVITE DES PARAMETRES MIDAS
# ------------------------------------------------------------

p1 <- midas %>%
  mutate(
    Significatif=ifelse(P_value<.05,"p < 5%","p >= 5%")
  ) %>%
  count(Parametre,Significatif)

g1 <- ggplot(p1,aes(x=Parametre,y=n,fill=Significatif)) +
  geom_col(position="dodge") +
  labs(
    title="Significativité des paramètres MIDAS",
    x="Paramètre",
    y="Nombre de modèles"
  ) +
  theme_minimal()

ggsave(
  file.path(out,"FIGURE_01_SIGNIFICATIVITE_MIDAS.png"),
  g1,width=9,height=6,dpi=300
)

# ------------------------------------------------------------
# 11. FIGURE 2 - DIAGNOSTICS PAR PAYS
# ------------------------------------------------------------

p2 <- diag_country %>%
  pivot_longer(
    cols=c(LB_residus_sig,LB_residus2_sig,ARCH_LM_sig),
    names_to="Test",
    values_to="Nombre"
  )

g2 <- ggplot(p2,aes(x=Pays,y=Nombre,fill=Test)) +
  geom_col(position="dodge") +
  labs(
    title="Diagnostics post-estimation par pays",
    x="Pays",
    y="Nombre de modèles avec p < 5%"
  ) +
  theme_minimal()

ggsave(
  file.path(out,"FIGURE_02_DIAGNOSTICS_PAR_PAYS.png"),
  g2,width=10,height=6,dpi=300
)

# ------------------------------------------------------------
# 12. FIGURE 3 - DISTRIBUTION DES SD RESIDUS
# ------------------------------------------------------------

g3 <- ggplot(diag,aes(x=SD_residus)) +
  geom_histogram(bins=20) +
  geom_vline(xintercept=1,linetype="dashed") +
  labs(
    title="Distribution de l'écart-type des résidus standardisés",
    x="SD des résidus",
    y="Nombre de modèles"
  ) +
  theme_minimal()

ggsave(
  file.path(out,"FIGURE_03_SD_RESIDUS.png"),
  g3,width=9,height=6,dpi=300
)

# ------------------------------------------------------------
# 13. FICHIER ROBUSTESSE - PLAN SANS INVENTER DE RESULTATS
# ------------------------------------------------------------

robustesse <- data.frame(
  Test=c(
    "Specification principale",
    "MIDAS K=6",
    "MIDAS K=12",
    "Distribution alternative",
    "Comparaison GPRT vs GPRA"
  ),
  Statut=c(
    "TERMINE",
    "A ESTIMER",
    "A ESTIMER",
    "A ESTIMER",
    "ANALYSE PRINCIPALE DISPONIBLE"
  ),
  Specification=c(
    "K=3; Beta; Student-t; skew=NO",
    "K=6",
    "K=12",
    "A definir selon rumidas",
    "Comparaison des coefficients existants"
  )
)

write.csv(
  robustesse,
  file.path(out,"29_PLAN_ROBUSTESSE.csv"),
  row.names=FALSE
)

# ------------------------------------------------------------
# 14. RAPPORT FINAL
# ------------------------------------------------------------

sink(file.path(out,"30_RAPPORT_FINALISATION.txt"))

cat("============================================================\n")
cat("RIMEH - RAPPORT FINALISATION\n")
cat("============================================================\n\n")

cat("MODELES\n")
cat("-------\n")
cat("Nombre de modeles :",nrow(diag),"\n")
cat("Diagnostics OK :",sum(diag$Statut=="OK",na.rm=TRUE),"\n")
cat("Erreurs :",sum(diag$Statut!="OK",na.rm=TRUE),"\n\n")

cat("DIAGNOSTICS\n")
cat("-----------\n")
cat("LB residus p<5% :",sum(diag$LB_sig,na.rm=TRUE),"/",nrow(diag),"\n")
cat("LB residus² p<5% :",sum(diag$LB2_sig,na.rm=TRUE),"/",nrow(diag),"\n")
cat("ARCH-LM p<5% :",sum(diag$ARCH_sig,na.rm=TRUE),"/",nrow(diag),"\n")
cat("Mediane moyenne residus :",median(diag$Mean_residus,na.rm=TRUE),"\n")
cat("Mediane SD residus :",median(diag$SD_residus,na.rm=TRUE),"\n\n")

cat("PARAMETRES MIDAS\n")
cat("----------------\n")
print(sig_summary)

cat("\n\nDIAGNOSTICS PAR PAYS\n")
print(diag_country)

cat("\n\nDIAGNOSTICS PAR SPECIFICATION\n")
print(diag_spec)

cat("\n\nCAS ATYPIQUES\n")
print(atyp)

cat("\n\nROBUSTESSE\n")
print(robustesse)

cat("\n============================================================\n")
cat("FIN\n")
cat("============================================================\n")

sink()

# ------------------------------------------------------------
# 15. EXCEL FINAL
# ------------------------------------------------------------

wb <- createWorkbook()

add_sheet <- function(wb,name,data) {
  addWorksheet(wb,name)
  writeData(wb,name,data)
  freezePane(wb,name,firstRow=TRUE)
}

add_sheet(wb,"MIDAS",midas)
add_sheet(wb,"MIDAS_WIDE",midas_wide)
add_sheet(wb,"Significativite",sig_summary)
add_sheet(wb,"Pays",country_summary)
add_sheet(wb,"Specifications",spec_summary)
add_sheet(wb,"Diagnostics",diag)
add_sheet(wb,"Diag_Pays",diag_country)
add_sheet(wb,"Diag_Spec",diag_spec)
add_sheet(wb,"Atypiques",atyp)
add_sheet(wb,"Global",global_diag)
add_sheet(wb,"Robustesse",robustesse)

if(exists("h")) {
  add_sheet(wb,"H1_H9",h)
}

saveWorkbook(
  wb,
  file.path(out,"RIMEH_FINALISATION_COMPLETE.xlsx"),
  overwrite=TRUE
)

cat("\n============================================================\n")
cat(" FIN DE LA FINALISATION\n")
cat("============================================================\n")
cat("Dossier :",out,"\n")
cat("Excel : RIMEH_FINALISATION_COMPLETE.xlsx\n")
cat("============================================================\n")
