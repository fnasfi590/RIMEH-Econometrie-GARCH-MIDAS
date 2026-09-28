d <- read.csv("data/processed/monthly_model_data.csv", check.names=FALSE)

vars <- c("GPRT","GPRA","SUP","DEM","PRE","VIX","inflation",
          "interest_rate","exchange_rate","GDP")

outdir <- "results/analysis/X4_PRELIMINARY_TESTS"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)

cat("\n============================================================\n")
cat(" RIMEH - DIAGNOSTIC COMPLET X.4 + VIF CORRIGE\n")
cat("============================================================\n")

# ============================================================
# 1. RESULTATS X4 EXISTANTS
# ============================================================

cat("\n\n================ RESULTATS X4 EXISTANTS ================\n")

files <- c(
  "X4_SUMMARY.csv",
  "X4_STATIONARITY_ADF_PP_KPSS.csv",
  "X4_ARCH_LM.csv",
  "X4_LJUNG_BOX.csv",
  "X4_VIF.csv"
)

for (f in files) {

  p <- file.path(outdir, f)

  cat("\n---------------- ", f, " ----------------\n", sep="")

  if (!file.exists(p)) {

    cat("[ABSENT] Le fichier n'existe pas.\n")

  } else {

    info <- file.info(p)

    if (is.na(info$size) || info$size == 0) {

      cat("[VIDE] Le fichier existe mais contient 0 octet.\n")
      cat("=> Ignore automatiquement.\n")

    } else {

      x <- tryCatch(
        read.csv(p, check.names=FALSE),
        error=function(e) NULL
      )

      if (is.null(x)) {
        cat("[ERREUR] Impossible de lire le fichier.\n")
      } else {
        print(x)
      }
    }
  }
}

# ============================================================
# 2. DIAGNOSTIC DES VARIABLES
# ============================================================

cat("\n\n============================================================\n")
cat(" DIAGNOSTIC DES VARIABLES MENSUELLES\n")
cat("============================================================\n\n")

diag <- data.frame(
  Variable=character(),
  N_total=integer(),
  N_NA=integer(),
  N_Inf=integer(),
  N_Finite=integer(),
  SD=numeric(),
  stringsAsFactors=FALSE
)

for (v in vars) {

  if (v %in% names(d)) {

    x <- suppressWarnings(as.numeric(d[[v]]))

    sdx <- if(sum(is.finite(x)) > 1) {
      sd(x, na.rm=TRUE)
    } else {
      NA_real_
    }

    diag <- rbind(
      diag,
      data.frame(
        Variable=v,
        N_total=length(x),
        N_NA=sum(is.na(x)),
        N_Inf=sum(is.infinite(x)),
        N_Finite=sum(is.finite(x)),
        SD=sdx
      )
    )

  } else {

    diag <- rbind(
      diag,
      data.frame(
        Variable=v,
        N_total=0,
        N_NA=NA_integer_,
        N_Inf=NA_integer_,
        N_Finite=0,
        SD=NA_real_
      )
    )
  }
}

print(diag, row.names=FALSE)

write.csv(
  diag,
  file.path(outdir,"X4_VARIABLES_DIAGNOSTIC.csv"),
  row.names=FALSE
)

# ============================================================
# 3. VARIABLES VALIDES POUR VIF
# ============================================================

cat("\n\n============================================================\n")
cat(" VARIABLES UTILISABLES POUR LE VIF\n")
cat("============================================================\n\n")

valid_vars <- c()

for (v in vars) {

  if (v %in% names(d)) {

    x <- suppressWarnings(as.numeric(d[[v]]))

    nfinite <- sum(is.finite(x))

    sdx <- if(nfinite > 1) {
      sd(x, na.rm=TRUE)
    } else {
      NA_real_
    }

    if(
      nfinite > 0 &&
      is.finite(sdx) &&
      sdx > 0
    ) {
      valid_vars <- c(valid_vars, v)
    }
  }
}

cat("Variables valides:\n")
print(valid_vars)

cat("\nNombre de variables valides:",length(valid_vars),"\n")

# ============================================================
# 4. MATRICE DE CORRELATION
# ============================================================

cat("\n\n============================================================\n")
cat(" MATRICE DE CORRELATION\n")
cat("============================================================\n\n")

if(length(valid_vars) >= 2) {

  X <- d[,valid_vars,drop=FALSE]

  X[] <- lapply(
    X,
    function(x) suppressWarnings(as.numeric(x))
  )

  X <- X[complete.cases(X),,drop=FALSE]

  cat("Observations completes:",nrow(X),"\n\n")

  if(nrow(X) > 10) {

    cor_matrix <- cor(X)

    print(round(cor_matrix,3))

    write.csv(
      cor_matrix,
      file.path(outdir,"X4_CORRELATION_MATRIX.csv")
    )

  } else {

    cat("[ATTENTION] Pas assez d'observations completes.\n")
  }

} else {

  cat("[ATTENTION] Moins de 2 variables valides.\n")
}

# ============================================================
# 5. VIF CORRIGE
# ============================================================

cat("\n\n============================================================\n")
cat(" CALCUL DU VIF CORRIGE\n")
cat("============================================================\n\n")

vif_success <- FALSE

if(length(valid_vars) >= 2) {

  X <- d[,valid_vars,drop=FALSE]

  X[] <- lapply(
    X,
    function(x) suppressWarnings(as.numeric(x))
  )

  X <- X[complete.cases(X),,drop=FALSE]

  cat("Observations completes pour VIF:",nrow(X),"\n")

  if(nrow(X) > 10) {

    if(!requireNamespace("car",quietly=TRUE)) {

      cat("[ERREUR] Package car non installe.\n")

    } else {

      library(car)

      target <- if("GPRT" %in% valid_vars) {
        "GPRT"
      } else {
        valid_vars[1]
      }

      predictors <- setdiff(valid_vars,target)

      if(length(predictors) >= 1) {

        formula_vif <- as.formula(
          paste(
            target,
            "~",
            paste(predictors,collapse=" + ")
          )
        )

        cat("\nVariable dependante:",target,"\n")
        cat("Predictors:",paste(predictors,collapse=", "),"\n")

        cat("\nFormule:\n")
        print(formula_vif)

        model <- tryCatch(
          lm(formula_vif,data=X),
          error=function(e) NULL
        )

        if(!is.null(model)) {

          vif_result <- tryCatch(
            vif(model),
            error=function(e) NULL
          )

          if(!is.null(vif_result)) {

            cat("\n================ VIF ================\n")
            print(vif_result)

            vif_table <- data.frame(
              Variable=names(vif_result),
              VIF=as.numeric(vif_result)
            )

            print(vif_table)

            write.csv(
              vif_table,
              file.path(outdir,"X4_VIF_CORRECTED.csv"),
              row.names=FALSE
            )

            vif_success <- TRUE

            cat("\n[OK] X4_VIF_CORRECTED.csv cree.\n")

          } else {

            cat("[ERREUR] Le calcul VIF a echoue.\n")
          }

        } else {

          cat("[ERREUR] lm() a echoue.\n")
        }

      } else {

        cat("[ERREUR] Aucun predictor disponible.\n")
      }
    }

  } else {

    cat("[ERREUR] Pas assez d'observations completes.\n")
  }

} else {

  cat("[ERREUR] Moins de 2 variables utilisables.\n")
}

# ============================================================
# 6. CONTROLE INFLATION
# ============================================================

cat("\n\n============================================================\n")
cat(" CONTROLE INFLATION\n")
cat("============================================================\n\n")

if("inflation" %in% names(d)) {

  inf <- suppressWarnings(as.numeric(d$inflation))

  cat("Total   :",length(inf),"\n")
  cat("Valides :",sum(is.finite(inf)),"\n")
  cat("NA      :",sum(is.na(inf)),"\n")
  cat("Inf     :",sum(is.infinite(inf)),"\n")

  if(sum(is.finite(inf)) > 0) {

    cat("Min     :",min(inf,na.rm=TRUE),"\n")
    cat("Mediane :",median(inf,na.rm=TRUE),"\n")
    cat("Moyenne :",mean(inf,na.rm=TRUE),"\n")
    cat("Max     :",max(inf,na.rm=TRUE),"\n")
    cat("SD      :",sd(inf,na.rm=TRUE),"\n")

  } else {

    cat("\n[PROBLEME] Inflation contient 0 observation valide.\n")
  }

} else {

  cat("[PROBLEME] Variable inflation absente.\n")
}

# ============================================================
# 7. DIMENSIONS
# ============================================================

cat("\n\n============================================================\n")
cat(" DIMENSIONS DES DONNEES\n")
cat("============================================================\n\n")

cat(
  "monthly_model_data.csv :",
  nrow(d),
  "lignes x",
  ncol(d),
  "colonnes\n"
)

daily_path <- "data/processed/daily_clean.csv"

if(file.exists(daily_path)) {

  daily <- read.csv(daily_path)

  cat(
    "daily_clean.csv        :",
    nrow(daily),
    "lignes x",
    ncol(daily),
    "colonnes\n"
  )
}

# ============================================================
# 8. RESUME
# ============================================================

cat("\n\n============================================================\n")
cat(" RESUME X.4\n")
cat("============================================================\n\n")

cat("Variables testees :",length(vars),"\n")
cat("Variables valides :",length(valid_vars),"\n")
cat("VIF calcule       :",ifelse(vif_success,"OUI","NON"),"\n")

if("inflation" %in% names(d)) {

  cat(
    "Inflation valide  :",
    sum(is.finite(suppressWarnings(as.numeric(d$inflation)))),
    "\n"
  )
}

cat("\nFichiers produits:\n")
cat(" - X4_VARIABLES_DIAGNOSTIC.csv\n")
cat(" - X4_CORRELATION_MATRIX.csv\n")

if(vif_success) {
  cat(" - X4_VIF_CORRECTED.csv\n")
}

cat("\n============================================================\n")
cat(" DIAGNOSTIC X.4 TERMINE\n")
cat("============================================================\n")
