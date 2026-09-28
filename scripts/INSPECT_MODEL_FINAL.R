rds <- "results/models/garch_midas/DEU_GPRA_SUP.rds"

cat("============================================\n")
cat("INSPECTION MODELE RIMEH\n")
cat("============================================\n\n")

m <- readRDS(rds)

cat("CLASS:\n")
print(class(m))

cat("\nNOMS OBJET:\n")
print(names(m))

cat("\nSTRUCTURE:\n")
str(m, max.level = 2)

cat("\n\nCOEFFICIENTS:\n")
print(m$rob_coef_mat)

cat("\n\nOBS:\n")
print(m$obs)

cat("\nPERIOD:\n")
print(m$period)

cat("\nLOGLIK:\n")
print(m$loglik)

cat("\nINFO CRITERIA:\n")
print(m$inf_criteria)

cat("\nEST VOL:\n")
print(head(m$est_vol_in_s))
print(tail(m$est_vol_in_s))

cat("\nLONG RUN:\n")
print(head(m$est_lr_in_s))
print(tail(m$est_lr_in_s))

cat("\nLOSS:\n")
print(m$loss_in_s)

cat("\n============================================\n")
cat("FIN\n")
cat("============================================\n")