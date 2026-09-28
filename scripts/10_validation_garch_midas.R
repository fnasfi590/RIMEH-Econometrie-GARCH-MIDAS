if(!requireNamespace("FinTS", quietly=TRUE)) install.packages("FinTS", repos="https://cloud.r-project.org")
if(!requireNamespace("tseries", quietly=TRUE)) install.packages("tseries", repos="https://cloud.r-project.org")

rdir <- "results/models/garch_midas"

old <- c(
  "MEX_GPRT_DEM.rds",
  "NOR_GPRA_SUP.rds",
  "USA_GPRA_SUP.rds",
  "IND_GPRT_VIX.rds"
)

fs <- list.files(rdir, pattern="\\.rds$", full.names=TRUE)
fs <- fs[!basename(fs) %in% old]

daily <- read.csv(
  "data/processed/daily_clean.csv",
  stringsAsFactors=FALSE
)

daily[["date"]] <- as.Date(daily[["date"]])

rows <- list()

for(f in fs){

  x <- readRDS(f)

  nm <- tools::file_path_sans_ext(basename(f))
  sp <- strsplit(nm, "_", fixed=TRUE)[[1]]

  cc <- sp[[1]]
  mm <- paste(sp[-1], collapse="_")

  d <- daily[
    daily[["country"]] == cc &
    is.finite(daily[["return"]]),
    ,
    drop=FALSE
  ]

  lr_obj <- x[["est_lr_in_s"]]
  vv_obj <- x[["est_vol_in_s"]]

  lr <- as.numeric(lr_obj)
  vv <- as.numeric(vv_obj)

  idx_num <- attr(lr_obj, "index")

  dates_model <- as.Date(
    idx_num / 86400,
    origin="1970-01-01"
  )

  model_df <- data.frame(
    date=dates_model,
    mu=lr,
    vol=vv,
    stringsAsFactors=FALSE
  )

  d2 <- merge(
    d[,c("date","return")],
    model_df,
    by="date",
    all=FALSE
  )

  d2 <- d2[
    is.finite(d2[["return"]]) &
    is.finite(d2[["mu"]]) &
    is.finite(d2[["vol"]]) &
    d2[["vol"]] > 0,
    ,
    drop=FALSE
  ]

  z <- (
    d2[["return"]] - d2[["mu"]]
  ) / sqrt(d2[["vol"]])

  z <- z[is.finite(z)]

  if(length(z) > 30){

    lb1 <- Box.test(
      z,
      lag=12,
      type="Ljung-Box"
    )

    zsq <- z * z

    lb2 <- Box.test(
      zsq,
      lag=12,
      type="Ljung-Box"
    )

    arch <- FinTS::ArchTest(
      z,
      lags=12
    )

    jb <- tryCatch(
      tseries::jarque.bera.test(z),
      error=function(e) NULL
    )

    rows[[length(rows)+1]] <- data.frame(
      country=cc,
      model=mm,
      N=length(z),
      LB_resid=as.numeric(lb1$statistic),
      p_LB_resid=as.numeric(lb1$p.value),
      LB_sq=as.numeric(lb2$statistic),
      p_LB_sq=as.numeric(lb2$p.value),
      ARCH_LM=as.numeric(arch$statistic),
      p_ARCH=as.numeric(arch$p.value),
      JB=if(is.null(jb)) NA else as.numeric(jb$statistic),
      p_JB=if(is.null(jb)) NA else as.numeric(jb$p.value),
      stringsAsFactors=FALSE
    )
  }
}

tab <- do.call(rbind, rows)

write.csv(
  tab,
  "results/tables/garch_midas_diagnostics_FINAL_73.csv",
  row.names=FALSE
)

cat("\n============================================\n")
cat("VALIDATION GARCH-MIDAS - FINAL 73\n")
cat("============================================\n")

cat("RDS total:",length(list.files(rdir,pattern="\\.rds$")),"\n")
cat("RDS testes:",length(fs),"\n")
cat("Modeles testes:",nrow(tab),"\n\n")

cat("LB resid p < 0.05:",
    sum(tab[["p_LB_resid"]] < 0.05,na.rm=TRUE),
    "/",nrow(tab),"\n")

cat("LB resid p >= 0.05:",
    sum(tab[["p_LB_resid"]] >= 0.05,na.rm=TRUE),
    "/",nrow(tab),"\n")

cat("LB carres p < 0.05:",
    sum(tab[["p_LB_sq"]] < 0.05,na.rm=TRUE),
    "/",nrow(tab),"\n")

cat("LB carres p >= 0.05:",
    sum(tab[["p_LB_sq"]] >= 0.05,na.rm=TRUE),
    "/",nrow(tab),"\n")

cat("ARCH-LM p < 0.05:",
    sum(tab[["p_ARCH"]] < 0.05,na.rm=TRUE),
    "/",nrow(tab),"\n")

cat("ARCH-LM p >= 0.05:",
    sum(tab[["p_ARCH"]] >= 0.05,na.rm=TRUE),
    "/",nrow(tab),"\n")

cat("JB p < 0.05:",
    sum(tab[["p_JB"]] < 0.05,na.rm=TRUE),
    "/",nrow(tab),"\n")

cat("\nPremiers modeles:\n")
print(head(tab,10))
