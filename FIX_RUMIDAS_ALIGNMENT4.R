options(repos=c(CRAN="https://cloud.r-project.org"))
setwd("C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE")

f <- "scripts/08_garch_midas_models.R"
x <- readLines(f, warn=FALSE)

cat("=== CORRECTION RUMIDAS V4 ===\n")

# K plus petit pour respecter la profondeur temporelle disponible.
x <- gsub("K=12", "K=3", x, fixed=TRUE)
x <- gsub("K = 12", "K = 3", x, fixed=TRUE)

# Remplacer uniquement la conversion yearmon -> Date qui provoquait l'erreur.
old <- 'required_start <- as.Date(zoo::as.yearmon(mv_start) + (K_use + 1)/12)'
new <- 'required_start <- seq.Date(as.Date(mv_start), by="month", length.out=K_use+2)[K_use+2]'

n <- sum(x == old)

cat("Ancienne ligne trouvee :", n, "\n")

if (n > 0) {
  x[x == old] <- new
} else {
  # Recherche plus robuste si l'espacement differe.
  ii <- grep("required_start <-", x)
  if (length(ii) > 0) {
    cat("Ligne required_start trouvee :", ii[1], "\n")
    x[ii[1]] <- new
  } else {
    stop("Impossible de trouver la ligne required_start.")
  }
}

writeLines(x, f, useBytes=TRUE)

cat("Nouvelle ligne :\n")
print(x[grep("required_start <-", x)])

cat("\n=== PREPARATION DES DONNEES ===\n")
source("scripts/06_prepare_part3_data.R")

cat("\n=== LANCEMENT GARCH-MIDAS ===\n")
source("scripts/08_garch_midas_models.R")

cat("\n=== FIN ===\n")