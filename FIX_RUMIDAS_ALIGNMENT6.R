options(repos=c(CRAN="https://cloud.r-project.org"))
setwd("C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE")

f <- "scripts/08_garch_midas_models.R"
x <- readLines(f, warn=FALSE)

cat("=== CORRECTION RUMIDAS V6 ===\n")

# K=3
x <- gsub("K=12", "K=3", x, fixed=TRUE)
x <- gsub("K = 12", "K = 3", x, fixed=TRUE)

# Localiser les lignes du bloc actuel
kline <- grep("^\\s*K_use <- K\\s*$", x)
mmline <- grep("^\\s*mm <- lapply\\(", x)

if (length(kline) == 0) stop("K_use <- K introuvable.")
if (length(mmline) == 0) stop("mm <- lapply introuvable.")

kline <- kline[1]
mmline <- mmline[1]

# Nouveau bloc d'alignement
newblock <- c(
  "    K_use <- K",
  "    ",
  "    # Recuperer les dates mensuelles sans perdre la classe Date.",
  "    mv_starts_num <- sapply(mats, function(z) as.numeric(zoo::index(z)[1]))",
  "    mv_start <- as.Date(max(mv_starts_num, na.rm=TRUE), origin=\"1970-01-01\")",
  "    ",
  "    # rumidas exige que mv commence au moins K+1 periodes avant x.",
  "    # Donc x doit commencer K+1 mois apres mv_start.",
  "    required_start <- seq.Date(mv_start, by=\"month\", length.out=K_use+2)[K_use+2]",
  "    ",
  "    r_start <- as.Date(zoo::index(r)[1])",
  "    if (r_start < required_start) {",
  "      r <- r[paste0(format(required_start, \"%Y-%m-%d\"), \"/\")]",
  "    }",
  "    ",
  "    if (NROW(r) < 100) {",
  "      stop(\"Pas assez de rendements quotidiens apres alignement.\")",
  "    }",
  "    ",
  "    cat(\"  mv_start =\", format(mv_start), \"\\n\")",
  "    cat(\"  required_start =\", format(required_start), \"\\n\")",
  "    cat(\"  r_start_final =\", format(as.Date(zoo::index(r)[1])), \"\\n\")",
  "    cat(\"  daily observations =\", NROW(r), \"\\n\")"
)

x <- c(
  x[seq_len(kline - 1)],
  newblock,
  x[mmline:length(x)]
)

writeLines(x, f, useBytes=TRUE)

cat("\n=== VERIFICATION DU BLOC ===\n")
y <- readLines(f, warn=FALSE)
jj <- grep("mv_starts_num|mv_start <-|required_start <-|r_start_final|mm <- lapply", y)
print(y[pmax(1,min(jj)-2):min(length(y),max(jj)+3)])

cat("\n=== PREPARATION DES DONNEES ===\n")
source("scripts/06_prepare_part3_data.R")

cat("\n=== LANCEMENT GARCH-MIDAS ===\n")
source("scripts/08_garch_midas_models.R")

cat("\n=== FIN GARCH-MIDAS ===\n")