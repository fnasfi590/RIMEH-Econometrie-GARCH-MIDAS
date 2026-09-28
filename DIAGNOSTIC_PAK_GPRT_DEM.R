setwd("C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE")
library(readr)
library(dplyr)
library(xts)
library(zoo)
library(rumidas)

cat("\n============================================================\n")
cat(" RIMEH - REPRODUCTION PAK_GPRT_DEM\n")
cat("============================================================\n")

daily <- read_csv(
  "data/processed/daily_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date))

monthly <- read_csv(
  "data/processed/monthly_model_data.csv",
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date))

cc <- "PAK"
vars <- c("GPRT", "DEM")
K <- 3

d <- daily %>%
  filter(country == cc, is.finite(return)) %>%
  arrange(date)

m <- monthly %>%
  filter(country == cc) %>%
  arrange(date)

mats <- lapply(vars, function(v) {
  z <- m %>%
    select(date, value = all_of(v)) %>%
    tidyr::drop_na()

  xts(z$value, order.by = z$date)
})

monthly_starts <- lapply(
  mats,
  function(z) min(as.Date(zoo::index(z)))
)

monthly_ends <- lapply(
  mats,
  function(z) max(as.Date(zoo::index(z)))
)

common_start <- do.call(max, monthly_starts)
common_end <- do.call(min, monthly_ends)

required_start <- seq.Date(
  common_start,
  by = "month",
  length.out = K + 2
)[K + 2]

end_exclusive <- seq.Date(
  common_end,
  by = "month",
  length.out = 2
)[2]

d <- d[
  d$date >= required_start &
  d$date < end_exclusive,
]

d <- d[order(d$date), ]

r <- xts(
  d$return,
  order.by = d$date
)

cat("\n--- PREPARATION ---\n")
cat("Country           :", cc, "\n")
cat("Variables         :", paste(vars, collapse = " + "), "\n")
cat("K                 :", K, "\n")
cat("common_start      :", common_start, "\n")
cat("common_end        :", common_end, "\n")
cat("required_start    :", required_start, "\n")
cat("r_start           :", as.Date(index(r)[1]), "\n")
cat("r_end             :", as.Date(index(r)[NROW(r)]), "\n")
cat("N daily           :", NROW(r), "\n")
cat("return mean       :", mean(as.numeric(r), na.rm = TRUE), "\n")
cat("return sd         :", sd(as.numeric(r), na.rm = TRUE), "\n")

mm <- lapply(
  mats,
  function(z) {
    rumidas::mv_into_mat(
      r,
      z,
      K = K,
      type = "monthly"
    )
  }
)

cat("\n--- MIDAS ---\n")
cat("GPRT dimensions   :", paste(dim(mm[[1]]), collapse = " x "), "\n")
cat("DEM dimensions    :", paste(dim(mm[[2]]), collapse = " x "), "\n")

fit <- readRDS(
  "results/models/garch_midas/PAK_GPRT_DEM.rds"
)

cat("\n--- RDS MODEL ---\n")
cat("Class             :", paste(class(fit), collapse = ", "), "\n")
cat("Model             :", fit$model, "\n")
cat("Observations      :", fit$obs, "\n")
cat("LogLik            :", fit$loglik, "\n")
cat("AIC               :", fit$inf_criteria[1], "\n")
cat("BIC               :", fit$inf_criteria[2], "\n")
cat("Period start      :", fit$period[1], "\n")
cat("Period end        :", fit$period[2], "\n")

cat("\n--- COEFFICIENTS ---\n")
print(fit$rob_coef_mat)

sigma <- as.numeric(fit$est_vol_in_s)

ret <- as.numeric(r)

idx_r <- as.Date(index(r))
idx_sigma <- as.Date(index(fit$est_vol_in_s))

cat("\n--- ALIGNMENT ---\n")
cat("Return length      :", length(ret), "\n")
cat("Sigma length       :", length(sigma), "\n")
cat("Same length        :", length(ret) == length(sigma), "\n")
cat("Same dates         :", identical(idx_r, idx_sigma), "\n")

if (length(ret) == length(sigma)) {

  z <- ret / sigma

  cat("\n--- STANDARDIZED RESIDUALS ---\n")
  cat("Mean               :", mean(z, na.rm = TRUE), "\n")
  cat("SD                 :", sd(z, na.rm = TRUE), "\n")
  cat("Min                :", min(z, na.rm = TRUE), "\n")
  cat("Max                :", max(z, na.rm = TRUE), "\n")

  cat("\n--- FIRST 10 OBSERVATIONS ---\n")

  print(
    data.frame(
      date = idx_r[1:10],
      return = ret[1:10],
      sigma = sigma[1:10],
      z = z[1:10]
    )
  )
}

cat("\n============================================================\n")
cat(" FIN\n")
cat("============================================================\n")
