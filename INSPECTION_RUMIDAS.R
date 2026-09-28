setwd("C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE")

library(rumidas)

fit <- readRDS("results/models/garch_midas/PAK_GPRT_DEM.rds")

cat("\n============================================================\n")
cat(" RIMEH - INSPECTION PROFONDE RUMIDAS\n")
cat("============================================================\n\n")

cat("--- OBJET FIT ---\n")
cat("class :", paste(class(fit), collapse=", "), "\n")
cat("names :\n")
print(names(fit))

cat("\n--- MODELE ---\n")
cat("model :", fit$model, "\n")
cat("obs   :", fit$obs, "\n")
cat("logLik:", fit$loglik, "\n")

cat("\n--- COEFFICIENTS ---\n")
print(fit$rob_coef_mat)

cat("\n--- VOLATILITE ---\n")
cat("class est_vol_in_s :\n")
print(class(fit$est_vol_in_s))

cat("\nattributes est_vol_in_s :\n")
print(attributes(fit$est_vol_in_s))

cat("\nsummary est_vol_in_s :\n")
print(summary(as.numeric(fit$est_vol_in_s)))

cat("\n--- LONG RUN ---\n")
cat("class est_lr_in_s :\n")
print(class(fit$est_lr_in_s))

cat("\nsummary est_lr_in_s :\n")
print(summary(as.numeric(fit$est_lr_in_s)))

cat("\n--- FORMULES RUMIDAS DISPONIBLES ---\n")

cat("\nGM2M conditional volatility:\n")
print(rumidas:::GM_2M_COND_VOL)

cat("\nGM2M log likelihood:\n")
print(rumidas:::GM_2M_LOG_LIK)

cat("\nGM conditional volatility:\n")
print(rumidas:::GM_COND_VOL)

cat("\nGM log likelihood:\n")
print(rumidas:::GM_LOG_LIK)

cat("\n--- UGMFIT ---\n")
print(rumidas::ugmfit)

cat("\n============================================================\n")
cat(" FIN INSPECTION\n")
cat("============================================================\n")
