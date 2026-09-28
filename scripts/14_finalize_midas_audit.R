audit_file <- "results/tables/garch_midas_run_audit.csv"

audit <- readr::read_csv(audit_file, show_col_types = FALSE)

audit <- audit |>
  dplyr::mutate(
    status = dplyr::case_when(
      country == "MEX" & model == "GPRT_SUP" ~ "OK",
      country == "NOR" & model == "SUP_PRE"  ~ "OK",
      country == "USA" & model == "GPRA_SUP" ~ "OK",
      country == "GBR" & model == "SUP_PRE"  ~ "FAILED_NUMERICAL",
      TRUE ~ status
    )
  )

readr::write_csv(audit, audit_file)

cat("\n========================================\n")
cat("FINAL GARCH-MIDAS AUDIT\n")
cat("========================================\n")
print(table(audit$status))
cat("\nTotal models:", nrow(audit), "\n")
cat("Successful:", sum(audit$status %in% c("OK","EXISTING")), "\n")
cat("Numerical failure:", sum(audit$status == "FAILED_NUMERICAL"), "\n")
cat("\nGBR/SUP_PRE status:\n")
print(audit[audit$country == "GBR" & audit$model == "SUP_PRE", ])
