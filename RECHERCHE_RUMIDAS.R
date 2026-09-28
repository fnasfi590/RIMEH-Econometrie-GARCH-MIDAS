setwd("C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE")

library(rumidas)

cat("\n============================================================\n")
cat(" RIMEH - RECHERCHE FONCTIONS INTERNES RUMIDAS\n")
cat("============================================================\n")

cat("\n--- VERSION RUMIDAS ---\n")
print(packageVersion("rumidas"))

cat("\n--- OBJETS INTERNES CONTENANT GM ---\n")
objs <- ls(getNamespace("rumidas"), all.names = TRUE)
gm_objs <- objs[grepl("GM|gm|LIK|lik|VOL|vol|COND|cond", objs)]
print(gm_objs)

cat("\n--- SOURCE UGMFIT ---\n")
print(getAnywhere("ugmfit"))

cat("\n--- OBJETS AVEC LOG LIKELIHOOD ---\n")
lik_objs <- objs[grepl("LIK|lik|LL|ll", objs)]
print(lik_objs)

cat("\n--- OBJETS AVEC CONDITIONAL VOLATILITY ---\n")
vol_objs <- objs[grepl("VOL|vol|COND|cond", objs)]
print(vol_objs)

cat("\n--- OBJETS AVEC DISTRIBUTION ---\n")
dist_objs <- objs[grepl("DIST|dist|STD|std|STUDENT|student", objs)]
print(dist_objs)

cat("\n============================================================\n")
cat(" FIN\n")
cat("============================================================\n")
