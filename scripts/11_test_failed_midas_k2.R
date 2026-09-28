lines <- readLines("scripts/08_garch_midas_models.R", warn = FALSE)
marker <- 'countries <- sort(unique(daily[["country"]]))'
cut <- which(trimws(lines) == marker)[1]

eval(parse(text = paste(lines[1:(cut - 1)], collapse = "\n")),
     envir = .GlobalEnv)

tests <- list(
  c("GBR","SUP_PRE"),
  c("MEX","GPRT_SUP"),
  c("NOR","SUP_PRE"),
  c("USA","GPRA_SUP")
)

for (x in tests) {
  cc <- x[1]
  nm <- x[2]

  vars <- switch(
    nm,
    SUP_PRE = c("SUP","PRE"),
    GPRT_SUP = c("GPRT","SUP"),
    GPRA_SUP = c("GPRA","SUP")
  )

  cat("\n========================================\n")
  cat("TEST K=2 :", cc, "/", nm, "\n")
  cat("========================================\n")

  fit <- tryCatch(
    fit_pair(cc, vars, K = 2),
    error = function(e) {
      cat("FAILED K=2:", e$message, "\n")
      NULL
    }
  )

  if (!is.null(fit)) {
    cat("SUCCESS K=2:", cc, "/", nm, "\n")
  }
}
