# ============================================================
# RIMEH - PARTIE 3B
# GARCH-MIDAS using rumidas.
# Parsimonious models: one/two monthly MIDAS variables at a time.
# This avoids pretending rumidas directly estimates an arbitrary
# 10+ regressor long-run equation in one call.
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

if (!requireNamespace("rumidas", quietly=TRUE)) stop("Install package 'rumidas' first.")
if (!file.exists("data/processed/daily_clean.csv") || !file.exists("data/processed/monthly_model_data.csv")) stop("Run scripts 06 and 07 first.")

daily <- readr::read_csv("data/processed/daily_clean.csv", show_col_types=FALSE) %>% mutate(date=as.Date(date))
monthly <- readr::read_csv("data/processed/monthly_model_data.csv", show_col_types=FALSE) %>% mutate(date=as.Date(date))
dir.create("results/models/garch_midas", recursive=TRUE, showWarnings=FALSE)
dir.create("results/tables", recursive=TRUE, showWarnings=FALSE)

# Standardize a monthly series and build a monthly xts aligned to the daily sample.
fit_pair <- function(cc, vars, K=3) {
  d <- daily %>%
    dplyr::filter(country == cc, is.finite(return)) %>%
    dplyr::arrange(date)

  m <- monthly %>%
    dplyr::filter(country == cc) %>%
    dplyr::arrange(date)

  if (nrow(d) < 500 || nrow(m) < 24) {
    stop("Insufficient data")
  }

  mats <- lapply(vars, function(v) {
    z <- m %>%
      dplyr::select(date, value = dplyr::all_of(v)) %>%
      tidyr::drop_na()

    if (nrow(z) < 24) {
      stop("Insufficient non-missing monthly observations for ", v)
    }

    xts::xts(z$value, order.by = z$date)
  })

  K_use <- K

  monthly_dates <- lapply(
    mats,
    function(z) as.Date(zoo::index(z))
  )

  common_start <- max(sapply(monthly_dates, min))
  common_end <- min(sapply(monthly_dates, max))

  required_start <- seq.Date(
    common_start,
    by = "month",
    length.out = K_use + 2
  )[K_use + 2]

  end_exclusive <- seq.Date(
    common_end,
    by = "month",
    length.out = 2
  )[2]

  d <- d %>%
    dplyr::filter(
      date >= required_start,
      date < end_exclusive
    ) %>%
    dplyr::arrange(date)

  if (nrow(d) < 100) {
    stop(
      "Insufficient daily observations after common-sample restriction: ",
      nrow(d)
    )
  }

  r <- xts::xts(
    d$return,
    order.by = d$date
  )

  cat("  common_start =", format(common_start), "\n")
  cat("  common_end =", format(common_end), "\n")
  cat("  required_start =", format(required_start), "\n")
  cat("  r_start_final =", format(as.Date(zoo::index(r)[1])), "\n")
  cat("  r_end_final =", format(as.Date(zoo::index(r)[NROW(r)])), "\n")
  cat("  daily observations =", NROW(r), "\n")

  mm <- lapply(
    mats,
    function(z) {
      rumidas::mv_into_mat(
        r,
        z,
        K = K_use,
        type = "monthly"
      )
    }
  )

  if (length(mm) == 1) {
    args <- list(
      model = "GM",
      skew = "NO",
      distribution = "std",
      daily_ret = r,
      mv_m = mm[[1]],
      K = K,
      lag_fun = "Beta",
      R = 50
    )
  } else if (length(mm) == 2) {
    args <- list(
      model = "GM2M",
      skew = "NO",
      distribution = "std",
      daily_ret = r,
      mv_m = mm[[1]],
      mv_m_2 = mm[[2]],
      K = K,
      K_2 = K,
      lag_fun = "Beta",
      R = 50
    )
  } else {
    stop("Only one or two MIDAS variables are supported")
  }

  do.call(rumidas::ugmfit, args)
}
countries <- sort(unique(daily[["country"]]))
specs <- list(
  GPRT_SUP = c("GPRT", "SUP"),
  GPRA_SUP = c("GPRA", "SUP"),
  GPRT_DEM = c("GPRT", "DEM"),
  GPRT_PRE = c("GPRT", "PRE"),
  GPRT_VIX = c("GPRT", "VIX"),
  SUP_DEM = c("SUP", "DEM"),
  SUP_PRE = c("SUP", "PRE")
)
summary_rows <- list()
model_count <- 0
for (cc in countries) {
  for (nm in names(specs)) {
    vars <- specs[[nm]]
    fit <- tryCatch(fit_pair(cc, vars, K=3), error=function(e){message("MIDAS failed ",cc,"/",nm,": ",e$message); NULL})
    if (!is.null(fit)) {
      saveRDS(fit, file.path("results/models/garch_midas", paste0(cc,"_",nm,".rds")))
      txt <- capture.output(summary(fit))
      writeLines(txt, file.path("results/tables", paste0("GARCH_MIDAS_",cc,"_",nm,".txt")))
      # Store the printed summary as an auditable text result; package object is the authoritative estimate.
      summary_rows[[paste(cc,nm)]] <- tibble::tibble(country=cc, model=nm, var1=vars[1], var2=vars[2], status="OK")
      model_count <- model_count+1
    }
  }
}
readr::write_csv(dplyr::bind_rows(summary_rows), "results/tables/garch_midas_run_audit.csv")
message("GARCH-MIDAS models completed: ", model_count)

