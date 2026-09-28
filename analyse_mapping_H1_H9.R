base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

f <- file.path(base, "results/analysis/FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv")

d <- read.csv(
  f,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("\n")
cat("============================================================\n")
cat("MAPPING EXACT DES HYPOTHESES H1-H3\n")
cat("============================================================\n")

cat("\nMODELES DISPONIBLES :\n")
print(sort(unique(d$model)))

cat("\n\nTABLE MODELE / SHOCK / TARGET :\n")

x <- unique(d[, c("model","SHOCK","TARGET")])
x <- x[order(x$model), ]

print(x, row.names = FALSE)

cat("\n\n============================================================\n")
cat("NOMBRE DE COEFFICIENTS PAR MODELE\n")
cat("============================================================\n")

tab_model <- aggregate(
  p_value ~ model + parameter,
  data = d,
  FUN = length
)

names(tab_model)[3] <- "N"

print(tab_model, row.names = FALSE)

cat("\n\n============================================================\n")
cat("RESULTATS DES COEFFICIENTS THETA PAR MODELE\n")
cat("============================================================\n")

theta <- d[
  d$parameter %in% c("theta","theta_1","theta_2"),
  c(
    "country",
    "model",
    "parameter",
    "Estimate",
    "Std_Error",
    "t_value",
    "p_value",
    "SHOCK",
    "TARGET"
  )
]

theta <- theta[order(theta$model, theta$country, theta$parameter), ]

print(theta, row.names = FALSE)

cat("\n\n============================================================\n")
cat("RESUME PAR MODELE\n")
cat("============================================================\n")

models <- sort(unique(theta$model))

for (m in models) {

  z <- theta[theta$model == m, ]

  cat("\n------------------------------------------------------------\n")
  cat("MODELE :", m, "\n")
  cat("SHOCK  :", paste(unique(z$SHOCK), collapse=", "), "\n")
  cat("TARGET :", paste(unique(z$TARGET), collapse=", "), "\n")
  cat("N coefficients :", nrow(z), "\n")

  cat("\nSignificatifs a 5% :", sum(z$p_value < 0.05), "/", nrow(z), "\n")
  cat("Positifs :", sum(z$Estimate > 0), "\n")
  cat("Negatifs :", sum(z$Estimate < 0), "\n")

  cat("\nMoyenne Estimate :", mean(z$Estimate, na.rm=TRUE), "\n")
  cat("Mediane Estimate :", median(z$Estimate, na.rm=TRUE), "\n")

  cat("Moyenne p-value :", mean(z$p_value, na.rm=TRUE), "\n")
}

cat("\n\n============================================================\n")
cat("TESTS INDIVIDUELS SIGNIFICATIFS A 5%\n")
cat("============================================================\n")

sig <- theta[theta$p_value < 0.05, ]

print(
  sig[
    order(sig$model, sig$country, sig$parameter),
    c(
      "country",
      "model",
      "parameter",
      "Estimate",
      "Std_Error",
      "t_value",
      "p_value",
      "SHOCK",
      "TARGET"
    )
  ],
  row.names = FALSE
)

cat("\n\n============================================================\n")
cat("FIN\n")
cat("============================================================\n")
