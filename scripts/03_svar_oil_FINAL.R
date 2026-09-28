# ============================================================
# RIMEH - PARTIE 2
# Kilian-style structural VAR: SUP / DEM / PRE
# ============================================================
# Structural system:
# z_t = (Delta oil production, global real activity,
#        Delta real oil price)'
#
# A0 z_t = c + sum(A_i z_{t-i}) + eps_t
# A0^{-1} is recursive/lower triangular.
# eps_1 = oil supply shock (SUP)
# eps_2 = aggregate/global demand shock (DEM)
# eps_3 = oil-specific demand shock (PRE)
#
# This is the identification used in Kilian (2009), AER 99(3), 1053-1069.
# IMPORTANT: SUP > 0 = positive supply innovation. A supply disruption
# is therefore SUP < 0.
# ============================================================

rm(list = ls())
source("scripts/00_packages.R")

for (d in c("results/models", "results/tables", "results/figures", "data/oil")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

D <- readr::read_csv("data/oil/oil_svar_input.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

needed <- c("date", "dlog_oil_prod", "rea", "dlog_real_oil_price")
miss <- setdiff(needed, names(D))
if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))

D <- D %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31")) %>%
  dplyr::select(all_of(needed)) %>%
  drop_na()

if (nrow(D) < 200) stop("Too few complete monthly observations: ", nrow(D))

Z <- ts(
  D %>% dplyr::select(dlog_oil_prod, rea, dlog_real_oil_price),
  start = c(lubridate::year(min(D$date)), lubridate::month(min(D$date))),
  frequency = 12
)

# ------------------------------------------------------------
# Lag selection
# ------------------------------------------------------------
sel <- VARselect(Z, lag.max = 24, type = "const")
write.csv(as.data.frame(sel$criteria), "results/tables/SVAR_lag_selection.csv")
writeLines(capture.output(sel$selection), "results/tables/SVAR_lag_choice.txt")

# Kilian (2009) uses 24 monthly lags. Keep this fixed for comparability.
p <- 24
var_fit <- VAR(Z, p = p, type = "const")

writeLines(capture.output(summary(var_fit)), "results/tables/SVAR_VAR_summary.txt")
saveRDS(var_fit, "results/models/VAR_oil_Kilian.rds")

# ------------------------------------------------------------
# Structural identification
# ------------------------------------------------------------
# A = [1  0  0
#      a21 1  0
#      a31 a32 1]
# This means the first variable is contemporaneously exogenous to
# the other two; the second can react contemporaneously to the first;
# the third can react contemporaneously to the first two.
A <- matrix(NA_real_, 3, 3)
diag(A) <- 1
A[1,2] <- 0
A[1,3] <- 0
A[2,3] <- 0

svar_fit <- SVAR(
  var_fit,
  Amat = A,
  Bmat = NULL,
  estmethod = "direct",
  hessian = TRUE
)

writeLines(capture.output(summary(svar_fit)), "results/tables/SVAR_structural_summary.txt")
saveRDS(svar_fit, "results/models/SVAR_oil_Kilian.rds")

# ------------------------------------------------------------
# Structural shocks
# ------------------------------------------------------------
# Under Amat identification with B = I, eps_t = A %*% u_t.
U <- residuals(var_fit)
E <- t(svar_fit$A %*% t(U))
colnames(E) <- c("SUP", "DEM", "PRE")

shock_dates <- D$date[(nrow(D) - nrow(E) + 1):nrow(D)]
shocks <- tibble(
  date = shock_dates,
  SUP = as.numeric(E[,1]),
  DEM = as.numeric(E[,2]),
  PRE = as.numeric(E[,3])
)

write_csv(shocks, "data/oil/oil_structural_shocks.csv")

# Merge for audit
Dfull <- readr::read_csv("data/oil/oil_svar_input.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  left_join(shocks, by = "date")
write_csv(Dfull, "data/oil/oil_svar_with_shocks.csv")

# ------------------------------------------------------------
# IRFs / FEVD
# ------------------------------------------------------------
set.seed(20260914)
irf_fit <- irf(
  svar_fit,
  n.ahead = 24,
  boot = TRUE,
  runs = 1000,
  ci = 0.95,
  ortho = TRUE
)
saveRDS(irf_fit, "results/models/SVAR_IRF_24m.rds")

png("results/figures/SVAR_IRF_24m.png", width = 1800, height = 1300, res = 150)
plot(irf_fit)
dev.off()

fevd_fit <- fevd(svar_fit, n.ahead = 24)
saveRDS(fevd_fit, "results/models/SVAR_FEVD_24m.rds")

png("results/figures/SVAR_FEVD_24m.png", width = 1800, height = 1300, res = 150)
plot(fevd_fit)
dev.off()

# ------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------
serial <- serial.test(var_fit, lags.pt = 24, type = "PT.asymptotic")
norm <- normality.test(var_fit, multivariate.only = TRUE)
stab <- stability(var_fit, type = "OLS-CUSUM")

writeLines(capture.output(serial), "results/tables/SVAR_serial_test.txt")
writeLines(capture.output(norm), "results/tables/SVAR_normality_test.txt")
writeLines(capture.output(summary(stab)), "results/tables/SVAR_stability.txt")

# Descriptive statistics of shocks
shock_stats <- shocks %>%
  summarise(
    across(
      c(SUP, DEM, PRE),
      list(mean = ~mean(.x), sd = ~sd(.x), min = ~min(.x), max = ~max(.x)),
      na.rm = TRUE
    )
  )
write_csv(shock_stats, "results/tables/SVAR_shock_descriptives.csv")

message("SVAR completed.")
message("Structural shocks: data/oil/oil_structural_shocks.csv")
message("SUP = positive oil-supply innovation; supply disruption = negative SUP.")
