library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

cat("\n============================================\n")
cat("ANALYSE COMPLETE GARCH-MIDAS + H1-H9\n")
cat("============================================\n")

dir.create("results/analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

coef_file <- "results/tables/garch_midas_all_results.csv"
summary_file <- "results/tables/garch_midas_model_summary.csv"

if (!file.exists(coef_file)) stop("Fichier introuvable: ", coef_file)
if (!file.exists(summary_file)) stop("Fichier introuvable: ", summary_file)

coef <- read_csv(coef_file, show_col_types = FALSE)
model_summary <- read_csv(summary_file, show_col_types = FALSE)

cat("\nFichiers charges avec succes.\n")
cat("Lignes coefficients :", nrow(coef), "\n")
cat("Modeles :", n_distinct(paste(coef$country, coef$model)), "\n")

# ============================================================
# 1. SIGNIFICATIVITE DES COEFFICIENTS
# ============================================================

coef_analysis <- coef %>%
  mutate(
    significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    significant_5pct = p_value < 0.05,
    significant_10pct = p_value < 0.10,
    positive = Estimate > 0,
    negative = Estimate < 0
  )

write_csv(
  coef_analysis,
  "results/analysis/garch_midas_coefficients_analysis.csv"
)

# ============================================================
# 2. TABLEAU DES PARAMETRES PRINCIPAUX
# ============================================================

main_parameters <- coef_analysis %>%
  filter(parameter %in% c(
    "alpha", "beta", "m",
    "theta_1", "theta_2", "shape"
  )) %>%
  select(
    country, model, parameter,
    Estimate, Std_Error, t_value, p_value,
    significance, significant_5pct,
    observations, start_date, end_date,
    loglik, AIC, BIC, MSE_percent, QLIKE
  )

write_csv(
  main_parameters,
  "results/analysis/table_parameters_principaux.csv"
)

# ============================================================
# 3. ? / ?
# ============================================================

alpha_beta <- coef_analysis %>%
  filter(parameter %in% c("alpha", "beta")) %>%
  select(
    country, model, parameter,
    Estimate, p_value, significance
  )

write_csv(
  alpha_beta,
  "results/analysis/table_alpha_beta.csv"
)

# ============================================================
# 4. ? : VARIABLES MIDAS
# ============================================================

theta <- coef_analysis %>%
  filter(parameter %in% c("theta_1", "theta_2")) %>%
  select(
    country, model, parameter,
    Estimate, p_value, significance,
    significant_5pct
  )

write_csv(
  theta,
  "results/analysis/table_theta_MIDAS.csv"
)

# ============================================================
# 5. STATISTIQUES PAR TYPE DE MODELE
# ============================================================

model_stats <- coef_analysis %>%
  group_by(model, parameter) %>%
  summarise(
    N = n(),
    mean_estimate = mean(Estimate, na.rm = TRUE),
    median_estimate = median(Estimate, na.rm = TRUE),
    sd_estimate = sd(Estimate, na.rm = TRUE),
    min_estimate = min(Estimate, na.rm = TRUE),
    max_estimate = max(Estimate, na.rm = TRUE),
    significant_5pct = sum(p_value < 0.05, na.rm = TRUE),
    significant_10pct = sum(p_value < 0.10, na.rm = TRUE),
    positive = sum(Estimate > 0, na.rm = TRUE),
    negative = sum(Estimate < 0, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  model_stats,
  "results/analysis/table_statistiques_par_modele.csv"
)

# ============================================================
# 6. STATISTIQUES ? ? ? PAR PAYS
# ============================================================

country_stats <- coef_analysis %>%
  filter(parameter %in% c(
    "alpha", "beta", "theta_1", "theta_2"
  )) %>%
  group_by(country, parameter) %>%
  summarise(
    N = n(),
    mean_estimate = mean(Estimate, na.rm = TRUE),
    significant_5pct = sum(p_value < 0.05, na.rm = TRUE),
    significant_10pct = sum(p_value < 0.10, na.rm = TRUE),
    positive = sum(Estimate > 0, na.rm = TRUE),
    negative = sum(Estimate < 0, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  country_stats,
  "results/analysis/table_statistiques_par_pays.csv"
)

# ============================================================
# 7. COMPARAISON GPRT / GPRA
# ============================================================

gpr_comparison <- coef_analysis %>%
  filter(
    model %in% c(
      "GPRT_SUP",
      "GPRA_SUP",
      "GPRT_DEM",
      "GPRT_PRE",
      "GPRT_VIX"
    ),
    parameter %in% c("theta_1", "theta_2")
  ) %>%
  mutate(
    GPR_type = case_when(
      grepl("^GPRT", model) ~ "GPRT",
      grepl("^GPRA", model) ~ "GPRA",
      TRUE ~ "OTHER"
    )
  ) %>%
  select(
    country, model, GPR_type,
    parameter, Estimate,
    p_value, significance
  )

write_csv(
  gpr_comparison,
  "results/analysis/table_comparaison_GPRT_GPRA.csv"
)

# ============================================================
# 8. COMPARAISON SUP / DEM / PRE
# ============================================================

shock_comparison <- coef_analysis %>%
  filter(
    model %in% c(
      "GPRT_SUP",
      "GPRT_DEM",
      "GPRT_PRE",
      "SUP_DEM",
      "SUP_PRE"
    ),
    parameter %in% c("theta_1", "theta_2")
  ) %>%
  mutate(
    shock_type = case_when(
      grepl("SUP", model) ~ "SUP",
      grepl("DEM", model) ~ "DEM",
      grepl("PRE", model) ~ "PRE",
      TRUE ~ "OTHER"
    )
  ) %>%
  select(
    country, model, shock_type,
    parameter, Estimate,
    p_value, significance
  )

write_csv(
  shock_comparison,
  "results/analysis/table_SUP_DEM_PRE.csv"
)

# ============================================================
# 9. PERFORMANCES DES MODELES
# ============================================================

performance <- model_summary %>%
  select(
    country, model,
    observations,
    start_date, end_date,
    loglik, AIC, BIC,
    MSE_percent, QLIKE
  )

write_csv(
  performance,
  "results/analysis/table_performance_modeles.csv"
)

# ============================================================
# 10. MEILLEUR QLIKE PAR PAYS
# IMPORTANT : comparaison uniquement a l'interieur du pays
# ============================================================

best_qlike_country <- performance %>%
  group_by(country) %>%
  slice_min(
    order_by = QLIKE,
    n = 1,
    with_ties = TRUE
  ) %>%
  ungroup()

write_csv(
  best_qlike_country,
  "results/analysis/table_Qlike_min_par_pays.csv"
)

# ============================================================
# 11. MEILLEUR AIC PAR PAYS
# ============================================================

best_aic_country <- performance %>%
  group_by(country) %>%
  slice_min(
    order_by = AIC,
    n = 1,
    with_ties = TRUE
  ) %>%
  ungroup()

write_csv(
  best_aic_country,
  "results/analysis/table_AIC_min_par_pays.csv"
)

# ============================================================
# 12. H1 : GPRT -> effet positif
# ============================================================

h1 <- coef_analysis %>%
  filter(
    model %in% c(
      "GPRT_SUP",
      "GPRT_DEM",
      "GPRT_PRE",
      "GPRT_VIX"
    ),
    parameter == "theta_1"
  ) %>%
  mutate(
    hypothesis = "H1",
    expected_sign = "POSITIF",
    statistical_result = case_when(
      p_value < 0.05 & Estimate > 0 ~ "CONFIRMATION_STATISTIQUE",
      p_value < 0.05 & Estimate < 0 ~ "EFFET_SIGNIFICATIF_OPPOSE",
      p_value >= 0.05 ~ "NON_SIGNIFICATIF",
      TRUE ~ "NON_DETERMINE"
    )
  )

# ============================================================
# 13. H2 : GPRA -> effet positif
# ============================================================

h2 <- coef_analysis %>%
  filter(
    model == "GPRA_SUP",
    parameter == "theta_1"
  ) %>%
  mutate(
    hypothesis = "H2",
    expected_sign = "POSITIF",
    statistical_result = case_when(
      p_value < 0.05 & Estimate > 0 ~ "CONFIRMATION_STATISTIQUE",
      p_value < 0.05 & Estimate < 0 ~ "EFFET_SIGNIFICATIF_OPPOSE",
      p_value >= 0.05 ~ "NON_SIGNIFICATIF",
      TRUE ~ "NON_DETERMINE"
    )
  )

# ============================================================
# 14. H3 : SUP
# ============================================================

h3 <- coef_analysis %>%
  filter(
    model %in% c(
      "GPRT_SUP",
      "GPRA_SUP",
      "SUP_DEM",
      "SUP_PRE"
    ),
    parameter %in% c("theta_1", "theta_2")
  ) %>%
  mutate(
    hypothesis = "H3",
    expected_sign = "POSITIF",
    statistical_result = case_when(
      p_value < 0.05 & Estimate > 0 ~ "CONFIRMATION_STATISTIQUE",
      p_value < 0.05 & Estimate < 0 ~ "EFFET_SIGNIFICATIF_OPPOSE",
      p_value >= 0.05 ~ "NON_SIGNIFICATIF",
      TRUE ~ "NON_DETERMINE"
    )
  )

# ============================================================
# 15. H4 : DEM
# ============================================================

h4 <- coef_analysis %>%
  filter(
    model %in% c(
      "GPRT_DEM",
      "SUP_DEM"
    ),
    parameter %in% c("theta_1", "theta_2")
  ) %>%
  mutate(
    hypothesis = "H4",
    expected_sign = "DIFFERENCIE",
    statistical_result = case_when(
      p_value < 0.05 ~ "EFFET_SIGNIFICATIF",
      TRUE ~ "NON_SIGNIFICATIF"
    )
  )

# ============================================================
# 16. H5 : PRE
# ============================================================

h5 <- coef_analysis %>%
  filter(
    model %in% c(
      "GPRT_PRE",
      "SUP_PRE"
    ),
    parameter %in% c("theta_1", "theta_2")
  ) %>%
  mutate(
    hypothesis = "H5",
    expected_sign = "POSITIF",
    statistical_result = case_when(
      p_value < 0.05 & Estimate > 0 ~ "CONFIRMATION_STATISTIQUE",
      p_value < 0.05 & Estimate < 0 ~ "EFFET_SIGNIFICATIF_OPPOSE",
      p_value >= 0.05 ~ "NON_SIGNIFICATIF",
      TRUE ~ "NON_DETERMINE"
    )
  )

# ============================================================
# 17. H6 : VIX
# ============================================================

h6 <- coef_analysis %>%
  filter(
    model == "GPRT_VIX",
    parameter %in% c("theta_1", "theta_2")
  ) %>%
  mutate(
    hypothesis = "H6",
    expected_sign = "POSITIF",
    statistical_result = case_when(
      p_value < 0.05 & Estimate > 0 ~ "CONFIRMATION_STATISTIQUE",
      p_value < 0.05 & Estimate < 0 ~ "EFFET_SIGNIFICATIF_OPPOSE",
      p_value >= 0.05 ~ "NON_SIGNIFICATIF",
      TRUE ~ "NON_DETERMINE"
    )
  )

# ============================================================
# 18. H7-H9 : NE PAS INVENTER
# ============================================================

h7 <- data.frame(
  hypothesis = "H7",
  status = "NON_TESTABLE_AVEC_MODELES_ACTUELS",
  reason = "Aucune interaction GPRT_x_SUP explicite dans les 76 modeles"
)

h8 <- data.frame(
  hypothesis = "H8",
  status = "NON_TESTABLE_AVEC_MODELES_ACTUELS",
  reason = "Aucune interaction SUP_x_EXPORTER explicite dans les 76 modeles"
)

h9 <- data.frame(
  hypothesis = "H9",
  status = "A_DEFINIR",
  reason = "Formulation exacte de H9 absente des fichiers GARCH-MIDAS"
)

# ============================================================
# 19. TABLEAU GLOBAL H1-H6
# ============================================================

hypotheses_1_6 <- bind_rows(
  h1, h2, h3, h4, h5, h6
) %>%
  select(
    hypothesis,
    country,
    model,
    parameter,
    Estimate,
    p_value,
    significance,
    expected_sign,
    statistical_result
  )

write_csv(
  hypotheses_1_6,
  "results/analysis/table_tests_H1_H6.csv"
)

# ============================================================
# 20. SYNTHESE H1-H9
# ============================================================

count_h <- function(x, h) {
  x %>%
    filter(hypothesis == h) %>%
    summarise(
      N = n(),
      significant_5pct = sum(p_value < 0.05, na.rm = TRUE),
      positive_significant = sum(
        p_value < 0.05 & Estimate > 0,
        na.rm = TRUE
      ),
      negative_significant = sum(
        p_value < 0.05 & Estimate < 0,
        na.rm = TRUE
      )
    )
}

h_summary <- bind_rows(
  count_h(h1, "H1"),
  count_h(h2, "H2"),
  count_h(h3, "H3"),
  count_h(h4, "H4"),
  count_h(h5, "H5"),
  count_h(h6, "H6")
)

h_manual <- data.frame(
  hypothesis = c("H7", "H8", "H9"),
  N = NA,
  significant_5pct = NA,
  positive_significant = NA,
  negative_significant = NA
)

h_summary_final <- bind_rows(
  h_summary,
  h_manual
)

write_csv(
  h_summary_final,
  "results/analysis/table_synthese_H1_H9.csv"
)

# ============================================================
# 21. GRAPHIQUE ? PAR PAYS
# ============================================================

beta_plot_data <- coef_analysis %>%
  filter(parameter == "beta")

p1 <- ggplot(
  beta_plot_data,
  aes(
    x = reorder(country, Estimate),
    y = Estimate
  )
) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Persistance de la volatilite : coefficient beta",
    x = "Pays",
    y = "Beta"
  ) +
  theme_minimal()

ggsave(
  "results/figures/beta_par_pays.png",
  p1,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 22. GRAPHIQUE ?
# ============================================================

theta_plot_data <- coef_analysis %>%
  filter(
    parameter %in% c("theta_1", "theta_2")
  )

p2 <- ggplot(
  theta_plot_data,
  aes(
    x = country,
    y = Estimate,
    fill = parameter
  )
) +
  geom_col(
    position = "dodge"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Coefficients theta de la composante MIDAS",
    x = "Pays",
    y = "Estimate",
    fill = "Parametre"
  ) +
  theme_minimal()

ggsave(
  "results/figures/theta_par_pays.png",
  p2,
  width = 12,
  height = 7,
  dpi = 300
)

# ============================================================
# 23. RAPPORT TEXTE AUTOMATIQUE
# ============================================================

sink("results/analysis/garch_midas_analysis_report.txt")

cat("============================================================\n")
cat("RAPPORT AUTOMATIQUE - GARCH-MIDAS\n")
cat("============================================================\n\n")

cat("Nombre de modeles :", n_distinct(
  paste(coef$country, coef$model)
), "\n")

cat("Nombre de lignes de coefficients :", nrow(coef), "\n")
cat("Nombre de pays :", n_distinct(coef$country), "\n\n")

cat("------------------------------------------------------------\n")
cat("ALPHA\n")
cat("------------------------------------------------------------\n")

alpha_report <- coef_analysis %>%
  filter(parameter == "alpha") %>%
  summarise(
    N = n(),
    significant_5pct = sum(p_value < 0.05),
    mean = mean(Estimate),
    min = min(Estimate),
    max = max(Estimate)
  )

print(alpha_report)

cat("\n------------------------------------------------------------\n")
cat("BETA\n")
cat("------------------------------------------------------------\n")

beta_report <- coef_analysis %>%
  filter(parameter == "beta") %>%
  summarise(
    N = n(),
    significant_5pct = sum(p_value < 0.05),
    mean = mean(Estimate),
    min = min(Estimate),
    max = max(Estimate)
  )

print(beta_report)

cat("\n------------------------------------------------------------\n")
cat("THETA\n")
cat("------------------------------------------------------------\n")

theta_report <- coef_analysis %>%
  filter(parameter %in% c("theta_1", "theta_2")) %>%
  summarise(
    N = n(),
    significant_5pct = sum(p_value < 0.05),
    significant_10pct = sum(p_value < 0.10),
    positive = sum(Estimate > 0),
    negative = sum(Estimate < 0)
  )

print(theta_report)

cat("\n------------------------------------------------------------\n")
cat("HYPOTHESES\n")
cat("------------------------------------------------------------\n")

print(h_summary_final)

cat("\n------------------------------------------------------------\n")
cat("MODELES PAR PAYS - QLIKE MINIMUM\n")
cat("------------------------------------------------------------\n")

print(best_qlike_country)

cat("\n============================================================\n")
cat("FIN DU RAPPORT\n")
cat("============================================================\n")

sink()

# ============================================================
# 24. RESUME CONSOLE
# ============================================================

cat("\n============================================\n")
cat("ANALYSE TERMINEE AVEC SUCCES\n")
cat("============================================\n")

cat("\nFICHIERS PRINCIPAUX CREES :\n")

cat("\n[1] Coefficients :\n")
cat("results/analysis/garch_midas_coefficients_analysis.csv\n")

cat("\n[2] Parametres principaux :\n")
cat("results/analysis/table_parameters_principaux.csv\n")

cat("\n[3] Alpha/Beta :\n")
cat("results/analysis/table_alpha_beta.csv\n")

cat("\n[4] Theta MIDAS :\n")
cat("results/analysis/table_theta_MIDAS.csv\n")

cat("\n[5] GPRT vs GPRA :\n")
cat("results/analysis/table_comparaison_GPRT_GPRA.csv\n")

cat("\n[6] SUP/DEM/PRE :\n")
cat("results/analysis/table_SUP_DEM_PRE.csv\n")

cat("\n[7] Performances :\n")
cat("results/analysis/table_performance_modeles.csv\n")

cat("\n[8] H1-H6 :\n")
cat("results/analysis/table_tests_H1_H6.csv\n")

cat("\n[9] H1-H9 :\n")
cat("results/analysis/table_synthese_H1_H9.csv\n")

cat("\n[10] Rapport texte :\n")
cat("results/analysis/garch_midas_analysis_report.txt\n")

cat("\n[11] Graphiques :\n")
cat("results/figures/beta_par_pays.png\n")
cat("results/figures/theta_par_pays.png\n")

cat("\n============================================\n")
