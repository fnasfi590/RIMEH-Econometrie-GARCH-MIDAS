# ============================================================
# RIMEH - PARTIE 3A
# Country-by-country GARCH(1,1), Student-t innovations.
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

if (!file.exists("data/processed/daily_clean.csv")) stop("Run Part 1 / preparation first.")
daily <- readr::read_csv("data/processed/daily_clean.csv", show_col_types=FALSE) %>%
  mutate(date=as.Date(date)) %>% arrange(country,date)

dir.create("results/models", recursive=TRUE, showWarnings=FALSE)
dir.create("results/tables", recursive=TRUE, showWarnings=FALSE)
dir.create("data/processed", recursive=TRUE, showWarnings=FALSE)

fit_one <- function(cc, model="sGARCH") {
  x <- daily %>% filter(country==cc, is.finite(return)) %>% arrange(date)
  if (nrow(x) < 500) stop("Too few daily observations: ", nrow(x))
  spec <- rugarch::ugarchspec(
    variance.model=list(model=model, garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
    distribution.model="std"
  )
  rugarch::ugarchfit(spec=spec, data=x$return, solver="hybrid")
}

countries <- sort(unique(daily$country))
models <- list(); vol_list <- list(); coef_list <- list()
for (cc in countries) {
  fit <- tryCatch(fit_one(cc), error=function(e){message("GARCH failed ",cc,": ",e$message); NULL})
  models[[cc]] <- fit
  if (!is.null(fit)) {
    x <- daily %>% filter(country==cc, is.finite(return)) %>% arrange(date)
    x$garch_sigma <- as.numeric(rugarch::sigma(fit))
    x$garch_variance <- x$garch_sigma^2
    vol_list[[cc]] <- x
    cf <- coef(fit)
    coef_list[[cc]] <- tibble::tibble(country=cc, parameter=names(cf), estimate=as.numeric(cf))
  }
}

saveRDS(models, "results/models/garch_all_countries.rds")
readr::write_csv(dplyr::bind_rows(vol_list), "data/processed/garch_daily_all_countries.csv")
readr::write_csv(dplyr::bind_rows(coef_list), "results/tables/garch_coefficients.csv")

# Persistence and half-life
persistence <- purrr::imap_dfr(models, function(fit, cc){
  if (is.null(fit)) return(tibble::tibble(country=cc))
  cf <- coef(fit); a <- unname(cf["alpha1"]); b <- unname(cf["beta1"])
  p <- a+b
  tibble::tibble(country=cc, alpha=a, beta=b, persistence=p,
                 stationary=(is.finite(p) && p<1), half_life=ifelse(p>0 && p<1, log(0.5)/log(p), NA_real_))
})
readr::write_csv(persistence, "results/tables/garch_persistence.csv")
message("GARCH completed.")
