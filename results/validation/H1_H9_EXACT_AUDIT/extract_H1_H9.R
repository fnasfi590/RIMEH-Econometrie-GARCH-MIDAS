base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

outdir <- file.path(base, "results/validation/H1_H9_EXACT_AUDIT")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

outfile <- file.path(outdir, "H1_H9_ALL_EXACT_RESULTS.txt")

files <- c(
  H1_H3_CORRECTED = file.path(base, "results/analysis/FINAL_H1_H3_CORRECTED.csv"),
  H1_H3_ROBUST = file.path(base, "results/analysis/FINAL_H1_H3_TABLE_ROBUST.csv"),

  H4_FINAL = file.path(base, "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H4.csv"),
  H4_FORMAL = file.path(base, "results/analysis/FORMAL_H1_H9/FORMAL_H4.csv"),

  H5_DETAIL = file.path(base, "results/analysis/FINAL_H5_DETAIL.csv"),
  H5_ROBUST = file.path(base, "results/analysis/FINAL_H5_HORIZON_DETAIL_ROBUST.csv"),

  H6_DETAIL = file.path(base, "results/analysis/FINAL_H6_DETAIL.csv"),
  H6_ROBUST = file.path(base, "results/analysis/FINAL_H6_HORIZON_DETAIL_ROBUST.csv"),

  H7_DETAIL = file.path(base, "results/analysis/FINAL_H7_DETAIL_ROBUST.csv"),

  H8_FINAL = file.path(base, "results/analysis/FINAL_FORMAL_H1_H9/FINAL_H7_H8.csv"),

  H9_DETAIL = file.path(base, "results/analysis/FINAL_H9_DETAIL.csv"),
  H9_ROBUST = file.path(base, "results/analysis/FINAL_H9_DETAIL_ROBUST.csv"),
  H9_FORMAL = file.path(base, "results/analysis/FORMAL_H1_H9/H9_GPRT_VS_GPRA_DETAIL.csv"),

  H5_H6_PAIRED = file.path(
    base,
    "results/analysis/FORMAL_H1_H9/H5_H6_PAIRED_THETA_DETAIL.csv"
  )
)

sink(outfile, split = TRUE)

cat("===============================================================\n")
cat("RIMEH - RESULTATS EXACTS DES TESTS H1-H9\n")
cat("===============================================================\n\n")

for (nm in names(files)) {

  cat("\n\n###############################################################\n")
  cat("FICHIER :", nm, "\n")
  cat("CHEMIN  :", files[[nm]], "\n")
  cat("###############################################################\n")

  if (!file.exists(files[[nm]])) {
    cat("*** FICHIER ABSENT ***\n")
    next
  }

  x <- tryCatch(
    read.csv(
      files[[nm]],
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(x)) {
    cat("*** ERREUR DE LECTURE ***\n")
    next
  }

  cat("\nDIMENSIONS :", nrow(x), "lignes x", ncol(x), "colonnes\n\n")

  cat("COLONNES EXACTES :\n")

  for (j in seq_along(names(x))) {
    cat(sprintf("%02d : %s\n", j, names(x)[j]))
  }

  cat("\n\nCONTENU COMPLET :\n")
  print(x, row.names = FALSE)

  important <- grep(
    "estimate|estimation|theta|p.?value|p.value|se|std|stat|wald|test|status|reason|hypothesis|country|gprt|gpra",
    names(x),
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(important) > 0) {

    cat("\n\nCOLONNES IMPORTANTES DETECTEES :\n")
    print(important)

    cat("\n\nEXTRACTION DES COLONNES IMPORTANTES :\n")
    print(x[, important, drop = FALSE], row.names = FALSE)
  }
}

sink()

cat("\n===============================================================\n")
cat("RAPPORT TERMINE\n")
cat("===============================================================\n")
cat("Fichier cree :\n")
cat(outfile, "\n")
