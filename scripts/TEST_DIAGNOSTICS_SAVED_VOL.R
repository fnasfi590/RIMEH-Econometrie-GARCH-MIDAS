suppressPackageStartupMessages({
  library(xts)
  library(FinTS)
})

ROOT <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

cat("============================================\n")
cat("TEST DIAGNOSTICS - VOLATILITE SAUVEGARDEE\n")
cat("============================================\n")

# ---------------------------------------------------------
# MODELE
# ---------------------------------------------------------
rds <- file.path(ROOT, "results/models/garch_midas/DEU_GPRA_SUP.rds")
m <- readRDS(rds)

cat("\nMODELE : DEU_GPRA_SUP\n")
cat("OBS MODELE :", m$obs, "\n")
cat("PERIODE :", as.character(m$period[1]), "->", as.character(m$period[2]), "\n")

# ---------------------------------------------------------
# DONNEES DAILY
# ---------------------------------------------------------
daily <- read.csv(
  file.path(ROOT, "data/processed/daily_clean.csv"),
  stringsAsFactors = FALSE
)

daily$date <- as.Date(daily$date)

cat("\nDAILY : ", nrow(daily), " lignes\n")
cat("DATES :", as.character(min(daily$date)), "->",
    as.character(max(daily$date)), "\n")

# détecter la colonne pays
cat("COLONNES DAILY :\n")
print(names(daily))

# ---------------------------------------------------------
# FILTRE PERIODE
# ---------------------------------------------------------
d <- daily[
  daily$country == "DEU" &
  daily$date >= m$period[1] &
  daily$date <= m$period[2],
]

cat("\nDAILY DEU FILTRE :", nrow(d), "\n")
cat("DATES :", as.character(min(d$date)), "->",
    as.character(max(d$date)), "\n")

# ---------------------------------------------------------
# RETOURS
# ---------------------------------------------------------
cat("\nSTRUCTURE RETURN :\n")
print(summary(d$return))

r <- as.numeric(d$return)

# ---------------------------------------------------------
# VOLATILITE SAUVEGARDEE
# ---------------------------------------------------------
sigma <- as.numeric(m$est_vol_in_s)

cat("\nVOLATILITE SAUVEGARDEE :", length(sigma), "\n")
cat("RETURN :", length(r), "\n")

cat("\nVOL SUMMARY :\n")
print(summary(sigma))

# ---------------------------------------------------------
# ALIGNEMENT
# ---------------------------------------------------------
n <- min(length(r), length(sigma))

r <- r[seq_len(n)]
sigma <- sigma[seq_len(n)]

valid <- is.finite(r) &
         is.finite(sigma) &
         sigma > 0

cat("\nOBS ALIGNEES :", n, "\n")
cat("OBS VALIDES :", sum(valid), "\n")
cat("OBS INVALIDES :", sum(!valid), "\n")

r <- r[valid]
sigma <- sigma[valid]

# ---------------------------------------------------------
# RESIDUS STANDARDISES
# ---------------------------------------------------------
z <- r / sigma

cat("\n============================================\n")
cat("RESIDUS STANDARDISES\n")
cat("============================================\n")

print(summary(z))

cat("\nMEAN :", mean(z), "\n")
cat("SD   :", sd(z), "\n")
cat("MIN  :", min(z), "\n")
cat("MAX  :", max(z), "\n")

# ---------------------------------------------------------
# LJUNG BOX RESIDUS
# ---------------------------------------------------------
cat("\n============================================\n")
cat("LJUNG-BOX RESIDUS\n")
cat("============================================\n")

lb1 <- Box.test(
  z,
  lag = 12,
  type = "Ljung-Box"
)

print(lb1)

# ---------------------------------------------------------
# LJUNG BOX RESIDUS CARRES
# ---------------------------------------------------------
cat("\n============================================\n")
cat("LJUNG-BOX RESIDUS CARRES\n")
cat("============================================\n")

lb2 <- Box.test(
  z^2,
  lag = 12,
  type = "Ljung-Box"
)

print(lb2)

# ---------------------------------------------------------
# ARCH LM
# ---------------------------------------------------------
cat("\n============================================\n")
cat("ARCH-LM RESIDUS\n")
cat("============================================\n")

arch <- ArchTest(
  z,
  lags = 12
)

print(arch)

# ---------------------------------------------------------
# RESULTAT
# ---------------------------------------------------------
cat("\n============================================\n")
cat("RESULTAT FINAL\n")
cat("============================================\n")

cat("LB_RESIDUS_PVALUE      :", lb1$p.value, "\n")
cat("LB_RESIDUS2_PVALUE     :", lb2$p.value, "\n")
cat("ARCH_LM_PVALUE         :", arch$p.value, "\n")
cat("LOGLIK                  :", m$loglik, "\n")
cat("AIC                     :", unname(m$inf_criteria["AIC"]), "\n")
cat("BIC                     :", unname(m$inf_criteria["BIC"]), "\n")

cat("\nFIN\n")