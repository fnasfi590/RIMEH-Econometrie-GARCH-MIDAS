# RIMEH - ROBUSTESSE K=6 / K=12
# Exact same data preparation logic as 08_garch_midas_models.R
# Only K changes.
# ============================================================

rm(list = ls())
source("scripts/00_packages.R")

if (!requireNamespace("rumidas", quietly=TRUE))
  stop("Install package 'rumidas' first.")

daily <- readr::read_csv(
  "data/processed/daily_clean.csv",
  show_col_types=FALSE
) %>%
  dplyr::mutate(date=as.Date(date))

monthly <- readr::read_csv(
  "data/processed/monthly_model_data.csv",
  show_col_types=FALSE
) %>%
  dplyr::mutate(date=as.Date(date))

rds_dir <- "results/models/garch_midas"

out_dir <- "results/analysis/RIMEH_FINAL/15_FINALISATION/ROBUSTESSE"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

rds_files <- list.files(
  rds_dir,
  pattern="\\.rds$",
  full.names=TRUE
)

# ------------------------------------------------------------
# Specifications
# ------------------------------------------------------------

specs <- list(
  GPRA_SUP  = c("GPRA","SUP"),
  GPRT_DEM  = c("GPRT","DEM"),
  GPRT_PRE  = c("GPRT","PRE"),
  GPRT_SUP  = c("GPRT","SUP"),
  GPRT_VIX  = c("GPRT","VIX"),
  SUP_DEM   = c("SUP","DEM"),
  SUP_PRE   = c("SUP","PRE")
)

countries <- c(
  "DEU","GBR","IND","JPN","KOR",
  "MEX","NOR","PAK","SAU","TUR","USA"
)

Ks <- c(6,12)

results <- list()
errors <- list()

counter <- 0

cat("============================================================\n")
cat(" RIMEH - ROBUSTESSE K=6 / K=12\n")
cat("============================================================\n")
cat("RDS :", length(rds_files), "\n")
cat("============================================================\n")

# ------------------------------------------------------------
# Exact same preparation as main script
# ------------------------------------------------------------

fit_robust <- function(cc, vars, K) {

  d <- daily %>%
    dplyr::filter(
      country == cc,
      is.finite(return)
    ) %>%
    dplyr::arrange(date)

  m <- monthly %>%
    dplyr::filter(country == cc) %>%
    dplyr::arrange(date)

  if (nrow(d) < 500 || nrow(m) < 24)
    stop("Insufficient data")

  # EXACT main-script logic:
  # each MIDAS variable is independently cleaned.
  mats <- lapply(vars, function(v) {

    z <- m %>%
      dplyr::select(
        date,
        value = dplyr::all_of(v)
      ) %>%
      tidyr::drop_na()

    if (nrow(z) < 24)
      stop(
        "Insufficient non-missing monthly observations for ",
        v
      )

    xts::xts(
      z$value,
      order.by=z$date
    )
  })

  monthly_starts <- lapply(
    mats,
    function(z)
      min(as.Date(zoo::index(z)))
  )

  monthly_ends <- lapply(
    mats,
    function(z)
      max(as.Date(zoo::index(z)))
  )

  common_start <- do.call(
    max,
    monthly_starts
  )

  common_end <- do.call(
    min,
    monthly_ends
  )

  # EXACT main-script formula
  required_start <- seq.Date(
    common_start,
    by="month",
    length.out=K+2
  )[K+2]

  end_exclusive <- seq.Date(
    common_end,
    by="month",
    length.out=2
  )[2]

  d <- d[
    d[["date"]] >= required_start &
    d[["date"]] < end_exclusive,
    ,
    drop=FALSE
  ]

  d <- d[order(d[["date"]]), , drop=FALSE]

  if (nrow(d) < 100)
    stop(
      "Insufficient daily observations after common-sample restriction: ",
      nrow(d)
    )

  r <- xts::xts(
    d$return,
    order.by=d$date
  )

  cat(
    "      common_start =", format(common_start),
    "| required_start =", format(required_start),
    "| N =", NROW(r), "\n"
  )

  # EXACT rumidas construction
  mm <- lapply(
    mats,
    function(z) {
      rumidas::mv_into_mat(
        r,
        z,
        K=K,
        type="monthly"
      )
    }
  )

  if (length(mm) == 1) {

    fit <- rumidas::ugmfit(
      model="GM",
      skew="NO",
      distribution="std",
      daily_ret=r,
      mv_m=mm[[1]],
      K=K,
      lag_fun="Beta",
      R=50
    )

  } else {

    fit <- rumidas::ugmfit(
      model="GM2M",
      skew="NO",
      distribution="std",
      daily_ret=r,
      mv_m=mm[[1]],
      mv_m_2=mm[[2]],
      K=K,
      K_2=K,
      lag_fun="Beta",
      R=50
    )
  }

  fit
}

# ------------------------------------------------------------
# Loop
# ------------------------------------------------------------

for (cc in countries) {

  for (sp in names(specs)) {

    # only process specifications that exist in the 76-model set
    rds_match <- list.files(
      rds_dir,
      pattern=paste0("^",cc,"_",sp,"\\.rds$"),
      full.names=TRUE
    )

    if (length(rds_match) == 0)
      next

    for (K in Ks) {

      counter <- counter + 1

      cat(
        sprintf(
          "[%03d] %s_%s | K=%d ... ",
          counter, cc, sp, K
        )
      )

      ans <- tryCatch({

        fit <- fit_robust(
          cc,
          specs[[sp]],
          K
        )

        coef <- tryCatch(
          as.data.frame(fit$rob_coef_mat),
          error=function(e) NULL
        )

        ic <- tryCatch(
          as.numeric(fit$inf_criteria),
          error=function(e) c(NA,NA,NA)
        )

        data.frame(
          Country=cc,
          Specification=sp,
          K=K,
          N_obs=length(fit$obs),
          LogLik=as.numeric(fit$loglik),
          AIC=ic[1],
          BIC=ic[2],
          Status="OK",
          stringsAsFactors=FALSE
        )

      }, error=function(e) {

        cat("ERREUR\n")

        errors[[length(errors)+1]] <<- data.frame(
          Country=cc,
          Specification=sp,
          K=K,
          Error=conditionMessage(e),
          stringsAsFactors=FALSE
        )

        NULL
      })

      if (!is.null(ans)) {
        cat("OK\n")
        results[[length(results)+1]] <- ans
      }
    }
  }
}

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

if (length(results) > 0) {
  res_df <- dplyr::bind_rows(results)
} else {
  res_df <- data.frame()
}

if (length(errors) > 0) {
  err_df <- dplyr::bind_rows(errors)
} else {
  err_df <- data.frame()
}


