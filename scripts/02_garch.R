# ============================================================
# 02_garch.R
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

daily <- read_csv("data/processed/daily_clean.csv", show_col_types=FALSE)
countries <- sort(unique(daily$country))

estimate_one_garch <- function(country_name, model_type="sGARCH") {
  x <- daily %>% filter(country==country_name, !is.na(return)) %>% arrange(date)

  spec <- ugarchspec(
    variance.model=list(model=model_type, garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
    distribution.model="std"
  )

  ugarchfit(spec=spec, data=x$return, solver="hybrid")
}

models <- list()
vols <- list()

for (cc in countries) {
  fit <- tryCatch(
    estimate_one_garch(cc, "sGARCH"),
    error=function(e) { message("GARCH failed for ",cc,": ",e$message); NULL }
  )
  models[[cc]] <- fit

  if (!is.null(fit)) {
    x <- daily %>% filter(country==cc, !is.na(return)) %>% arrange(date)
    x$garch_sigma <- as.numeric(sigma(fit))
    x$garch_variance <- x$garch_sigma^2
    vols[[cc]] <- x
  }
}

saveRDS(models, "results/models/garch_models.rds")
garch_daily <- bind_rows(vols)
write_csv(garch_daily, "data/processed/garch_daily.csv")

# Coefficient table
coef_table <- purrr::imap_dfr(models, function(fit, cc) {
  if (is.null(fit)) return(tibble(country=cc))
  tibble(country=cc, parameter=names(coef(fit)), estimate=as.numeric(coef(fit)))
})
write_csv(coef_table, "results/tables/garch_coefficients_long.csv")
