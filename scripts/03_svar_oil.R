# ============================================================
# 03_svar_oil.R
# Kilian-style oil-market SVAR for RIMEH
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

# Expected monthly input columns:
# date, global_oil_production, global_activity, real_oil_price
# IMPORTANT: order follows Kilian: production growth, global activity,
# real oil price. Identification is recursive/triangular.

oil <- read_csv("data/oil/oil_svar_input.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

req <- c("date", "global_oil_production", "global_activity", "real_oil_price")
miss <- setdiff(req, names(oil))
if (length(miss) > 0) stop("Missing columns in oil_svar_input.csv: ", paste(miss, collapse = ", "))

# Keep the thesis sample but retain enough observations for lags.
oil <- oil %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31")) %>%
  dplyr::select(all_of(req)) %>%
  drop_na()

if (nrow(oil) < 180) stop("Too few monthly observations after cleaning: ", nrow(oil))

# Kilian variables:
# 1) percentage change in global crude-oil production
# 2) global real economic activity index (already stationary in levels)
# 3) log real oil price
# The input production and real oil price should be LEVELS.
# If your source already supplies transformed variables, set
# TRANSFORM_INPUT = TRUE below and adapt the block accordingly.
TRANSFORM_INPUT <- FALSE

if (!TRANSFORM_INPUT) {
  oil <- oil %>%
    mutate(
      dlog_oil_prod = 100 * (log(global_oil_production) - lag(log(global_oil_production))),
      rea = global_activity,
      log_real_oil_price = log(real_oil_price)
    )
}

z <- oil %>%
  transmute(
    date,
    dlog_oil_prod,
    rea,
    dlog_real_oil_price = 100 * (log_real_oil_price - lag(log_real_oil_price))
  ) %>%
  drop_na()

# 24 monthly lags follows the standard Kilian specification.
Z <- ts(z %>% dplyr::select(dlog_oil_prod, rea, dlog_real_oil_price),
        start = c(year(min(z$date)), month(min(z$date))), frequency = 12)

lag_max <- 24
sel <- VARselect(Z, lag.max = lag_max, type = "const")
write.csv(as.data.frame(sel$criteria), "results/tables/SVAR_lag_selection.csv")
print(sel$selection)

# For comparability with Kilian-style oil-market SVARs, use 24 lags.
p <- 24
var_fit <- VAR(Z, p = p, type = "const")
print(summary(var_fit))

# Recursive contemporaneous identification:
# A0 z_t = ... + e_t, with A0 lower triangular and diagonal = 1.
# Therefore:
# - oil production does not react contemporaneously to global activity/oil price;
# - global activity may react contemporaneously to oil production but not oil price;
# - real oil price can react contemporaneously to all three market variables.
# This maps shocks to:
# e1 = oil supply shock
# e2 = aggregate/global-demand shock
# e3 = oil-specific demand shock
A <- matrix(NA_real_, 3, 3)
diag(A) <- 1
A[1, 2] <- 0
A[1, 3] <- 0
A[2, 3] <- 0

svar_fit <- SVAR(var_fit, Amat = A, Bmat = NULL, estmethod = "direct")
print(summary(svar_fit))

saveRDS(var_fit, "results/models/VAR_oil_kilian_style.rds")
saveRDS(svar_fit, "results/models/SVAR_oil_kilian_style.rds")

# Structural shocks: A0 * reduced-form residuals (B = I under this setup).
U <- residuals(var_fit)
E <- t(svar_fit$A %*% t(U))
colnames(E) <- c("SUP", "DEM", "PRE")

shock_dates <- z$date[(length(z$date) - nrow(E) + 1):length(z$date)]
shocks <- tibble(
  date = shock_dates,
  SUP = as.numeric(E[, "SUP"]),
  DEM = as.numeric(E[, "DEM"]),
  PRE = as.numeric(E[, "PRE"])
)

write_csv(shocks, "data/oil/oil_structural_shocks.csv")

# Merge shocks with the SVAR input for auditability.
oil_audit <- oil %>%
  dplyr::select(date, global_oil_production, global_activity, real_oil_price) %>%
  left_join(shocks, by = "date")
write_csv(oil_audit, "data/oil/oil_svar_with_shocks.csv")

# IRFs and FEVD.
irf_fit <- irf(svar_fit, n.ahead = 24, boot = TRUE, runs = 1000, ci = 0.95)
saveRDS(irf_fit, "results/models/SVAR_IRF.rds")
png("results/figures/SVAR_IRF.png", width = 1600, height = 1100, res = 150)
plot(irf_fit)
dev.off()

fevd_fit <- fevd(svar_fit, n.ahead = 24)
saveRDS(fevd_fit, "results/models/SVAR_FEVD.rds")
png("results/figures/SVAR_FEVD.png", width = 1600, height = 1100, res = 150)
plot(fevd_fit)
dev.off()

# Basic diagnostics.
serial <- serial.test(var_fit, lags.pt = 24, type = "PT.asymptotic")
normal <- normality.test(var_fit, multivariate.only = TRUE)
writeLines(capture.output(serial), "results/tables/SVAR_serial_test.txt")
writeLines(capture.output(normal), "results/tables/SVAR_normality_test.txt")

message("SVAR completed. Structural shocks saved to data/oil/oil_structural_shocks.csv")
message("Interpretation: SUP is a positive oil-supply innovation. A supply disruption is a negative SUP shock.")
message("Do not change the sign convention after estimation; keep interpretation consistent throughout the thesis.")
