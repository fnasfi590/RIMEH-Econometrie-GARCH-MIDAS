lines <- readLines("scripts/08_garch_midas_models.R", warn = FALSE)

marker <- 'countries <- sort(unique(daily[["country"]]))'
cut <- which(trimws(lines) == marker)[1]

if (is.na(cut)) {
  stop("Impossible de trouver le d?but de la boucle principale.")
}

eval(parse(text = paste(lines[1:(cut - 1)], collapse = "\n")),
     envir = .GlobalEnv)

countries <- sort(unique(daily[["country"]]))

specs <- list(
  GPRT_SUP = c("GPRT","SUP"),
  GPRA_SUP = c("GPRA","SUP"),
  GPRT_DEM = c("GPRT","DEM"),
  GPRT_PRE = c("GPRT","PRE"),
  GPRT_VIX = c("GPRT","VIX"),
  SUP_DEM = c("SUP","DEM"),
  SUP_PRE = c("SUP","PRE")
)

audit_file <- "results/tables/garch_midas_run_audit.csv"

audit_rows <- list()

for (cc in countries) {
  for (nm in names(specs)) {

    vars <- specs[[nm]]
    rds_file <- file.path(
      "results/models/garch_midas",
      paste0(cc, "_", nm, ".rds")
    )

    txt_file <- file.path(
      "results/tables",
      paste0("GARCH_MIDAS_", cc, "_", nm, ".txt")
    )

    if (file.exists(rds_file)) {

      cat("\nSKIP EXISTING:", cc, "/", nm, "\n")

      audit_rows[[paste(cc, nm)]] <- tibble::tibble(
        country = cc,
        model = nm,
        var1 = vars[1],
        var2 = vars[2],
        status = "EXISTING"
      )

      next
    }

    cat("\n========================================\n")
    cat("ESTIMATING:", cc, "/", nm, "\n")
    cat("========================================\n")

    fit <- tryCatch(
      fit_pair(cc, vars, K = 3),
      error = function(e) {
        message("MIDAS failed ", cc, "/", nm, ": ", e$message)
        NULL
      }
    )

    if (!is.null(fit)) {

      saveRDS(fit, rds_file)

      txt <- capture.output(summary(fit))
      writeLines(txt, txt_file)

      audit_rows[[paste(cc, nm)]] <- tibble::tibble(
        country = cc,
        model = nm,
        var1 = vars[1],
        var2 = vars[2],
        status = "OK"
      )

      cat("SUCCESS:", cc, "/", nm, "\n")

    } else {

      audit_rows[[paste(cc, nm)]] <- tibble::tibble(
        country = cc,
        model = nm,
        var1 = vars[1],
        var2 = vars[2],
        status = "FAILED"
      )

      cat("FAILED:", cc, "/", nm, "\n")
    }

    readr::write_csv(
      dplyr::bind_rows(audit_rows),
      audit_file
    )
  }
}

readr::write_csv(
  dplyr::bind_rows(audit_rows),
  audit_file
)

cat("\n========================================\n")
cat("GARCH-MIDAS RESUME COMPLETED\n")
cat("========================================\n")
cat("Audit:", audit_file, "\n")
