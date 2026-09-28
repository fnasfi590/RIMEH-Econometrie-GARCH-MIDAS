lines <- readLines("scripts/08_garch_midas_models.R", warn = FALSE)
marker <- 'countries <- sort(unique(daily[["country"]]))'
cut <- which(trimws(lines) == marker)[1]

eval(parse(text = paste(lines[1:(cut - 1)], collapse = "\n")),
     envir = .GlobalEnv)

tests <- list(
  MEX_GPRT_SUP = c("MEX","GPRT_SUP"),
  NOR_SUP_PRE  = c("NOR","SUP_PRE"),
  USA_GPRA_SUP = c("USA","GPRA_SUP")
)

for (nm_test in names(tests)) {

  cc <- tests[[nm_test]][1]
  nm <- tests[[nm_test]][2]

  vars <- switch(
    nm,
    GPRT_SUP = c("GPRT","SUP"),
    SUP_PRE  = c("SUP","PRE"),
    GPRA_SUP = c("GPRA","SUP")
  )

  cat("\n========================================\n")
  cat("FINAL K=2 ESTIMATION:", cc, "/", nm, "\n")
  cat("========================================\n")

  fit <- tryCatch(
    fit_pair(cc, vars, K = 2),
    error = function(e) {
      cat("FAILED:", cc, "/", nm, ":", e$message, "\n")
      NULL
    }
  )

  if (!is.null(fit)) {

    rds_file <- file.path(
      "results/models/garch_midas",
      paste0(cc, "_", nm, ".rds")
    )

    txt_file <- file.path(
      "results/tables",
      paste0("GARCH_MIDAS_", cc, "_", nm, ".txt")
    )

    saveRDS(fit, rds_file)

    txt <- capture.output(summary(fit))
    writeLines(txt, txt_file)

    cat("SAVED:", rds_file, "\n")
    cat("SAVED:", txt_file, "\n")
  }
}

cat("\n========================================\n")
cat("3 K=2 MODELS SAVED\n")
cat("========================================\n")
