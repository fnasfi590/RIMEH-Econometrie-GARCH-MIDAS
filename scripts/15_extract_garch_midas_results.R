library(tidyverse)
library(rumidas)

model_dir <- "results/models/garch_midas"
out_file <- "results/tables/garch_midas_all_results.csv"
audit_file <- "results/tables/garch_midas_extraction_audit.csv"

dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)

cat("========================================\n")
cat("GARCH-MIDAS RESULT EXTRACTION\n")
cat("========================================\n")
cat("RDS models found:", length(files), "\n\n")

extract_model <- function(f) {

  nm <- tools::file_path_sans_ext(basename(f))
  parts <- strsplit(nm, "_")[[1]]
  country <- parts[1]
  model <- paste(parts[-1], collapse = "_")

  cat("Processing:", country, "/", model, "\n")

  fit <- tryCatch(
    readRDS(f),
    error = function(e) {
      cat("  READ ERROR:", e$message, "\n")
      NULL
    }
  )

  if (is.null(fit)) {
    return(tibble(
      country = country,
      model = model,
      parameter = NA_character_,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      extraction_status = "READ_ERROR"
    ))
  }

  co <- tryCatch(
    as.data.frame(coef(summary(fit))),
    error = function(e) NULL
  )

  if (!is.null(co) && nrow(co) > 0) {

    co$parameter <- rownames(co)
    rownames(co) <- NULL

    old_names <- names(co)

    if (ncol(co) >= 5) {
      names(co)[1:4] <- c(
        "estimate",
        "std_error",
        "statistic",
        "p_value"
      )
    }

    if (!"estimate" %in% names(co))
      co$estimate <- NA_real_

    if (!"std_error" %in% names(co))
      co$std_error <- NA_real_

    if (!"statistic" %in% names(co))
      co$statistic <- NA_real_

    if (!"p_value" %in% names(co))
      co$p_value <- NA_real_

    return(tibble(
      country = country,
      model = model,
      parameter = as.character(co$parameter),
      estimate = suppressWarnings(as.numeric(co$estimate)),
      std_error = suppressWarnings(as.numeric(co$std_error)),
      statistic = suppressWarnings(as.numeric(co$statistic)),
      p_value = suppressWarnings(as.numeric(co$p_value)),
      extraction_status = "OK"
    ))
  }

  beta <- tryCatch(
    coef(fit),
    error = function(e) NULL
  )

  if (!is.null(beta)) {

    beta <- as.numeric(beta)
    beta_names <- names(coef(fit))

    if (is.null(beta_names))
      beta_names <- paste0("parameter_", seq_along(beta))

    return(tibble(
      country = country,
      model = model,
      parameter = beta_names,
      estimate = beta,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      extraction_status = "COEF_ONLY"
    ))
  }

  tibble(
    country = country,
    model = model,
    parameter = NA_character_,
    estimate = NA_real_,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    extraction_status = "EXTRACTION_FAILED"
  )
}

results <- purrr::map_dfr(files, extract_model)

readr::write_csv(results, out_file)

audit <- results |>
  dplyr::group_by(country, model) |>
  dplyr::summarise(
    n_parameters = sum(!is.na(parameter)),
    extraction_status = paste(unique(extraction_status), collapse = "; "),
    .groups = "drop"
  )

readr::write_csv(audit, audit_file)

cat("\n========================================\n")
cat("GARCH-MIDAS EXTRACTION COMPLETED\n")
cat("========================================\n")
cat("RDS files found:", length(files), "\n")
cat("Models extracted:", dplyr::n_distinct(results$country, results$model), "\n")
cat("Rows:", nrow(results), "\n")
cat("Output:", out_file, "\n")
cat("Audit:", audit_file, "\n\n")

cat("STATUS COUNTS\n")
print(audit |> dplyr::count(extraction_status))

cat("\nPARAMETER COUNTS\n")
print(audit, n = Inf)

cat("\n========================================\n")
cat("END\n")
cat("========================================\n")
