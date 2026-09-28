library(readr)
library(dplyr)

f <- "data/raw/BOJ_FM02_MONTHLY.csv"

boj <- read_csv(
  f,
  skip=8,
  col_names=c("date","end_month","average"),
  show_col_types=FALSE
)

boj <- boj |>
  mutate(
    date=as.Date(
      paste0(trimws(as.character(date)),"/01"),
      format="%Y/%m/%d"
    ),
    end_month=as.numeric(end_month),
    average=as.numeric(average)
  ) |>
  filter(
    date>=as.Date("2000-01-01"),
    date<=as.Date("2025-12-01")
  )

cat("\n============================================================\n")
cat(" BOJ JPN - END OF MONTH\n")
cat("============================================================\n")

cat("\nNombre observations :",nrow(boj),"\n")
cat("Observations valides :",sum(is.finite(boj$end_month)),"\n")

cat("\n--- 2000-01 -> 2001-06 ---\n")
print(
  boj |>
    filter(
      date>=as.Date("2000-01-01"),
      date<=as.Date("2001-06-01")
    ),
  n=30
)

cat("\n--- 2006-01 -> 2006-08 ---\n")
print(
  boj |>
    filter(
      date>=as.Date("2006-01-01"),
      date<=as.Date("2006-08-01")
    )
)

cat("\n--- 2013-01 -> 2016-10 ---\n")
print(
  boj |>
    filter(
      date>=as.Date("2013-01-01"),
      date<=as.Date("2016-10-01")
    ),
  n=100
)

cat("\n============================================================\n")
cat(" FIN\n")
cat("============================================================\n")
