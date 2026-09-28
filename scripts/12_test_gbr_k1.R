lines <- readLines("scripts/08_garch_midas_models.R", warn = FALSE)
marker <- 'countries <- sort(unique(daily[["country"]]))'
cut <- which(trimws(lines) == marker)[1]

eval(parse(text = paste(lines[1:(cut - 1)], collapse = "\n")),
     envir = .GlobalEnv)

cat("\n========================================\n")
cat("TEST K=1 : GBR / SUP_PRE\n")
cat("========================================\n")

fit <- tryCatch(
  fit_pair("GBR", c("SUP","PRE"), K = 1),
  error = function(e) {
    cat("FAILED K=1:", e$message, "\n")
    NULL
  }
)

if (!is.null(fit)) {
  cat("SUCCESS K=1: GBR / SUP_PRE\n")
}
