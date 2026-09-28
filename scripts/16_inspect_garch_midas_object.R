fit <- readRDS("results/models/garch_midas/DEU_GPRT_SUP.rds")

cat("========================================\n")
cat("INSPECTION OBJET RUMIDAS\n")
cat("========================================\n\n")

cat("CLASS:\n")
print(class(fit))

cat("\nNAMES:\n")
print(names(fit))

cat("\nSTRUCTURE:\n")
str(fit, max.level = 2)

cat("\n========================================\n")
cat("COEFFICIENTS / SUMMARY\n")
cat("========================================\n")

cat("\ncoef(fit):\n")
print(tryCatch(coef(fit), error=function(e) paste("ERROR:", e$message)))

cat("\nsummary(fit):\n")
print(tryCatch(summary(fit), error=function(e) paste("ERROR:", e$message)))

cat("\n========================================\n")
cat("INTERNAL OBJECTS\n")
cat("========================================\n")

if (is.list(fit)) {
  for (nm in names(fit)) {
    cat("\n---", nm, "---\n")
    obj <- fit[[nm]]
    cat("class:", paste(class(obj), collapse=", "), "\n")
    cat("length:", length(obj), "\n")
    if (is.numeric(obj) || is.matrix(obj) || is.data.frame(obj)) {
      print(obj)
    } else if (is.list(obj)) {
      print(names(obj))
    }
  }
}

cat("\n========================================\n")
cat("END INSPECTION\n")
cat("========================================\n")
