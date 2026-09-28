library(readr)
library(dplyr)

P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

mc <- read_csv(
  file.path(P,"data","processed","monthly_clean.csv"),
  show_col_types=FALSE
)

cat("\n============================================================\n")
cat("AUDIT RV.X / RV.Y DANS MONTHLY_CLEAN\n")
cat("============================================================\n")

cat("\nColonnes :\n")
print(names(mc))

cat("\nTypes RV.x / RV.y :\n")
print(
  mc %>%
    select(any_of(c("country","date","stock_price","return","RV.x","RV.y","n_obs","n_daily"))) %>%
    summarise(
      across(
        everything(),
        ~ if(is.numeric(.x)) paste(class(.x), collapse=",") else paste(class(.x),collapse=",")
      )
    )
)

cat("\n============================================================\n")
cat("SAU - 2006-05\n")
cat("============================================================\n")

print(
  mc %>%
    filter(country=="SAU",date==as.Date("2006-05-01")) %>%
    select(any_of(c(
      "country","date","RV.x","RV.y","n_obs","n_daily"
    )))
)

cat("\n============================================================\n")
cat("SAU - 2008-10\n")
cat("============================================================\n")

print(
  mc %>%
    filter(country=="SAU",date==as.Date("2008-10-01")) %>%
    select(any_of(c(
      "country","date","RV.x","RV.y","n_obs","n_daily"
    )))
)

cat("\n============================================================\n")
cat("COMPARAISON RV.X / RV.Y\n")
cat("============================================================\n")

if(all(c("RV.x","RV.y") %in% names(mc))){

  z <- mc %>%
    filter(is.finite(RV.x),is.finite(RV.y)) %>%
    mutate(
      ratio = RV.x/RV.y,
      diff = RV.x-RV.y,
      absdiff = abs(RV.x-RV.y)
    )

  cat("N:",nrow(z),"\n")
  cat("Correlation:",cor(z$RV.x,z$RV.y),"\n")
  cat("Moyenne RV.x:",mean(z$RV.x),"\n")
  cat("Moyenne RV.y:",mean(z$RV.y),"\n")
  cat("Mediane RV.x:",median(z$RV.x),"\n")
  cat("Mediane RV.y:",median(z$RV.y),"\n")
  cat("Ratio moyen:",mean(z$ratio),"\n")
  cat("Ratio median:",median(z$ratio),"\n")

  cat("\n--- 30 plus grandes differences ---\n")

  print(
    z %>%
      arrange(desc(absdiff)) %>%
      select(country,date,RV.x,RV.y,ratio,diff,absdiff) %>%
      head(30)
  )

  cat("\n--- 30 plus petits ratios RV.x/RV.y ---\n")

  print(
    z %>%
      arrange(ratio) %>%
      select(country,date,RV.x,RV.y,ratio) %>%
      head(30)
  )
}

cat("\n============================================================\n")
cat("FIN\n")
cat("============================================================\n")