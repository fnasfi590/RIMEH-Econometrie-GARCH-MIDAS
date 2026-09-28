library(dplyr)
library(readr)

dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

files <- list.files(
  "results/models/garch_midas",
  pattern = "\\.rds$",
  full.names = TRUE
)

all_results <- list()
audit <- list()

for (f in files) {

  id <- tools::file_path_sans_ext(basename(f))
  parts <- strsplit(id, "_")[[1]]

  country <- parts[1]
  model_name <- paste(parts[-1], collapse = "_")

  fit <- tryCatch(readRDS(f), error = function(e) NULL)

  if (is.null(fit) || is.null(fit$rob_coef_mat)) {
    audit[[id]] <- data.frame(
      file = basename(f),
      country = country,
      model = model_name,
      status = "FAILED",
      n_parameters = 0
    )
    next
  }

  coefmat <- as.data.frame(fit$rob_coef_mat)

  coefmat$parameter <- rownames(coefmat)
  rownames(coefmat) <- NULL

  coefmat <- coefmat %>%
    dplyr::select(
      parameter,
      Estimate,
      `Std. Error`,
      `t value`,
      `Pr(>|t|)`
    )

  coefmat$country <- country
  coefmat$model <- model_name
  coefmat$observations <- fit$obs
  coefmat$start_date <- as.character(fit$period[1])
  coefmat$end_date <- as.character(fit$period[2])
  coefmat$loglik <- fit$loglik
  coefmat$AIC <- unname(fit$inf_criteria["AIC"])
  coefmat$BIC <- unname(fit$inf_criteria["BIC"])
  coefmat$MSE_percent <- unname(fit$loss_in_s["MSE(%)"])
  coefmat$QLIKE <- unname(fit$loss_in_s["QLIKE"])

  all_results[[id]] <- coefmat

  audit[[id]] <- data.frame(
    file = basename(f),
    country = country,
    model = model_name,
    status = "OK",
    n_parameters = nrow(coefmat),
    observations = fit$obs,
    start_date = as.character(fit$period[1]),
    end_date = as.character(fit$period[2])
  )
}

results <- dplyr::bind_rows(all_results)
audit_df <- dplyr::bind_rows(audit)

write_csv(
  results,
  "results/tables/garch_midas_all_results.csv"
)

write_csv(
  audit_df,
  "results/tables/garch_midas_extraction_audit.csv"
)

summary_results <- results %>%
  dplyr::group_by(country, model) %>%
  dplyr::summarise(
    observations = dplyr::first(observations),
    start_date = dplyr::first(start_date),
    end_date = dplyr::first(end_date),
    loglik = dplyr::first(loglik),
    AIC = dplyr::first(AIC),
    BIC = dplyr::first(BIC),
    MSE_percent = dplyr::first(MSE_percent),
    QLIKE = dplyr::first(QLIKE),
    .groups = "drop"
  )

write_csv(
  summary_results,
  "results/tables/garch_midas_model_summary.csv"
)

cat("\n========================================\n")
cat("EXTRACTION TERMINEE\n")
cat("========================================\n")
cat("RDS files found :", length(files), "\n")
cat("Models extracted:", length(unique(paste(results$country, results$model))), "\n")
cat("Coefficient rows:", nrow(results), "\n")
cat("Successful      :", sum(audit_df$status == "OK"), "\n")
cat("Failed          :", sum(audit_df$status == "FAILED"), "\n\n")

cat("STATUS:\n")
print(table(audit_df$status))

cat("\nFICHIERS CREES:\n")
cat("1. results/tables/garch_midas_all_results.csv\n")
cat("2. results/tables/garch_midas_model_summary.csv\n")
cat("3. results/tables/garch_midas_extraction_audit.csv\n")

cat("\nMODELES:\n")
print(
  audit_df %>%
    dplyr::select(country, model, status, n_parameters)
)

cat("\n========================================\n")
