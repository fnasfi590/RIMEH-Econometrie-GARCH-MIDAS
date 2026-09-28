library(readr)
library(dplyr)

P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

mc <- read_csv(
  file.path(P,"data","processed","monthly_clean.csv"),
  show_col_types=FALSE
)

mm <- read_csv(
  file.path(P,"data","processed","monthly_model_data.csv"),
  show_col_types=FALSE
)

cat("\n============================================================\n")
cat("AUDIT CHAINE RV\n")
cat("============================================================\n")

cat("\n=== MONTHLY_CLEAN ===\n")
cat("Dimensions:", nrow(mc), "x", ncol(mc), "\n")
cat("Colonnes RV:\n")
print(names(mc)[grepl("^RV", names(mc), ignore.case=TRUE)])

cat("RV present:", "RV" %in% names(mc), "\n")

if ("RV" %in% names(mc)) {

  cat("RV valides:", sum(is.finite(mc$RV)), "/", nrow(mc), "\n")
  cat("RV min:", min(mc$RV, na.rm=TRUE), "\n")
  cat("RV median:", median(mc$RV, na.rm=TRUE), "\n")
  cat("RV mean:", mean(mc$RV, na.rm=TRUE), "\n")
  cat("RV max:", max(mc$RV, na.rm=TRUE), "\n")

  cat("\n--- SAU 2006-05 monthly_clean ---\n")
  print(
    mc %>%
      filter(country=="SAU", date==as.Date("2006-05-01")) %>%
      select(country,date,RV,n_obs)
  )

  cat("\n--- SAU 2008-10 monthly_clean ---\n")
  print(
    mc %>%
      filter(country=="SAU", date==as.Date("2008-10-01")) %>%
      select(country,date,RV,n_obs)
  )

  cat("\n--- SAU premiers mois ---\n")
  print(
    mc %>%
      filter(country=="SAU") %>%
      select(country,date,RV,n_obs) %>%
      head(20)
  )
}

cat("\n============================================================\n")
cat("MONTHLY_MODEL_DATA\n")
cat("============================================================\n")

cat("Dimensions:", nrow(mm), "x", ncol(mm), "\n")

cat("Colonnes RV:\n")
print(names(mm)[grepl("^RV", names(mm), ignore.case=TRUE)])

cat("\n--- SAU 2006-05 monthly_model_data ---\n")
print(
  mm %>%
    filter(country=="SAU", date==as.Date("2006-05-01"))
)

cat("\n--- SAU 2008-10 monthly_model_data ---\n")
print(
  mm %>%
    filter(country=="SAU", date==as.Date("2008-10-01"))
)

cat("\n============================================================\n")
cat("COMPARAISON\n")
cat("============================================================\n")

if ("RV" %in% names(mc)) {

  a <- mc %>%
    select(country,date,RV,n_obs)

  b <- mm %>%
    select(country,date,any_of(c("RV.x","RV.y","RV")))

  cmp <- inner_join(a,b,by=c("country","date"))

  cat("Observations jointes:", nrow(cmp), "\n")

  if ("RV.x" %in% names(cmp)) {

    cat(
      "Correlation RV / RV.x:",
      cor(cmp$RV, cmp$RV.x, use="complete.obs"),
      "\n"
    )

    cat(
      "Difference moyenne:",
      mean(cmp$RV-cmp$RV.x, na.rm=TRUE),
      "\n"
    )

    cat("\n--- Plus grandes differences ---\n")

    print(
      cmp %>%
        mutate(diff=abs(RV-RV.x)) %>%
        arrange(desc(diff)) %>%
        select(country,date,RV,RV.x,diff) %>%
        head(20)
    )
  }

  if ("RV.y" %in% names(cmp)) {

    cat(
      "\nCorrelation RV / RV.y:",
      cor(cmp$RV, cmp$RV.y, use="complete.obs"),
      "\n"
    )
  }
}

cat("\n============================================================\n")
cat("FIN AUDIT\n")
cat("============================================================\n")