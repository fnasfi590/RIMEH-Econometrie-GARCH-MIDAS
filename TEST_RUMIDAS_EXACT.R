library(rumidas)
library(xts)
library(zoo)

f <- "results/models/garch_midas/PAK_GPRT_DEM.rds"
fit <- readRDS(f)

cat("\n========================================\n")
cat("CONTROLE EXACT RUMIDAS : PAK_GPRT_DEM\n")
cat("========================================\n")

cat("\nClasse : "); print(class(fit))
cat("\nModele : "); print(fit$model)
cat("\nObservations : "); print(fit$obs)
cat("\nLogLik sauvegarde : "); print(fit$loglik)

cat("\nCoefficients :\n")
print(fit$rob_coef_mat)

cat("\nStructure est_vol_in_s :\n")
print(summary(fit$est_vol_in_s))

cat("\nFonction exacte utilisee :\n")
print(rumidas:::GM_2M_cond_vol_no_skew)

cat("\nFonction likelihood exacte :\n")
print(rumidas:::GM_2M_loglik_no_skew)

cat("\n========================================\n")
cat("FIN CONTROLE\n")
cat("========================================\n")