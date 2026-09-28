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

  cm <- fit$rob_coef_mat

  results_one <- data.frame(
    parameter = rownames(cm),
    Estimate = as.numeric(cm[["Estimate"]]),
    Std_Error = as.numeric(cm[["Std. Error"]]),
    t_value = as.numeric(cm[["t value"]]),
    p_value = as.numeric(cm[["Pr(>|t|)"]]),
    country = country,
    model = model_name,
    observations = as.numeric(fit$obs),
    start_date = as.character(fit$period[1]),
    end_date = as.character(fit$period[2]),
    loglik = as.numeric(fit$loglik),
    AIC = as.numeric(fit$inf_criteria["AIC"]),
    BIC = as.numeric(fit$inf_criteria["BIC"]),
    MSE_percent = as.numeric(fit$loss_in_s["MSE(%)"]),
    QLIKE = as.numeric(fit$loss_in_s["QLIKE"]),
    stringsAsFactors = FALSE
  )

  all_results[[id]] <- results_one

  audit[[id]] <- data.frame(
    file = basename(f),
    country = country,
    model = model_name,
    status = "OK",
    n_parameters = nrow(results_one),
    observations = as.numeric(fit$obs),
    start_date = as.character(fit$period[1]),
    end_date = as.character(fit$period[2]),
    stringsAsFactors = FALSE
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
  group_by(country, model) %>%
  summarise(
    observations = first(observations),
    start_date = first(start_date),
    end_date = first(end_date),
    loglik = first(loglik),
    AIC = first(AIC),
    BIC = first(BIC),
    MSE_percent = first(MSE_percent),
    QLIKE = first(QLIKE),
    .groups = "drop"
  )

write_csv(
  summary_results,
  "results/tables/garch_midas_model_summary.csv"
)

cat("\n========================================\n")
cat("GARCH-MIDAS EXTRACTION COMPLETED\n")
cat("========================================\n")
cat("RDS files found :", length(files), "\n")
cat("Models extracted:", length(unique(paste(results$country, results$model))), "\n")
cat("Coefficient rows:", nrow(results), "\n")
cat("Successful      :", sum(audit_df$status == "OK"), "\n")
cat("Failed          :", sum(audit_df$status == "FAILED"), "\n\n")

cat("STATUS COUNTS:\n")
print(table(audit_df$status))

cat("\nFILES CREATED:\n")
cat("results/tables/garch_midas_all_results.csv\n")
cat("results/tables/garch_midas_model_summary.csv\n")
cat("results/tables/garch_midas_extraction_audit.csv\n")

cat("\n========================================\n")
