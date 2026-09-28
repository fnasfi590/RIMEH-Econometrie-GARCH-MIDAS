base <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

out <- file.path(base, "results/validation/H1_H9_EXACT_AUDIT")
dir.create(out, recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 1. COEFFICIENTS H1-H3
# ============================================================

f <- file.path(
  base,
  "results/analysis/FINAL_HYPOTHESES_DETAIL_BY_COUNTRY.csv"
)

d <- read.csv(
  f,
  stringsAsFactors=FALSE,
  check.names=FALSE
)

theta <- d[
  d$parameter %in% c("theta","theta_1","theta_2"),
]

# H1 = GPRT_VIX
h1 <- theta[theta$model=="GPRT_VIX", ]
h1$Hypothese <- "H1"

# H2 = DEMAND
h2 <- theta[
  theta$model %in% c("GPRT_DEM","SUP_DEM"),
]
h2$Hypothese <- "H2"

# H3 = SUPPLY
h3 <- theta[
  theta$model %in% c("GPRA_SUP","GPRT_SUP"),
]
h3$Hypothese <- "H3"

make_individual <- function(x) {

  if(nrow(x)==0)
    return(data.frame())

  data.frame(
    Hypothese=x$Hypothese,
    Modele=x$model,
    Pays=x$country,
    Parametre=x$parameter,
    H0=paste0(x$parameter," = 0"),
    H1=paste0(x$parameter," != 0"),
    Test="Test individuel du coefficient (t/z)",
    Estimation=x$Estimate,
    SE=x$Std_Error,
    Statistique=x$t_value,
    P_value=x$p_value,
    Decision_5pct=ifelse(
      x$p_value < 0.05,
      "Rejet de H0",
      "Non-rejet de H0"
    ),
    stringsAsFactors=FALSE
  )
}

detail_h1 <- make_individual(h1)
detail_h2 <- make_individual(h2)
detail_h3 <- make_individual(h3)

detail <- rbind(
  detail_h1,
  detail_h2,
  detail_h3
)

# ============================================================
# 2. H5-H6
# ============================================================

f56 <- file.path(
  base,
  "results/analysis/FORMAL_H1_H9/H5_H6_PAIRED_THETA_DETAIL.csv"
)

if(file.exists(f56)) {

  x56 <- read.csv(
    f56,
    stringsAsFactors=FALSE,
    check.names=FALSE
  )

  # H5
  h5 <- data.frame(
    Hypothese="H5",
    Modele=x56$model,
    Pays=x56$country,
    Parametre="theta_1 - theta_2",
    H0="theta_1 - theta_2 = 0",
    H1="theta_1 - theta_2 != 0",
    Test="Test de difference sous hypothese d'independance",
    Estimation=x56$diff,
    SE=x56$se_ind,
    Statistique=x56$z_ind,
    P_value=x56$p_ind,
    Decision_5pct=ifelse(
      x56$p_ind < 0.05,
      "Rejet de H0",
      "Non-rejet de H0"
    ),
    stringsAsFactors=FALSE
  )

  # H6
  h6 <- data.frame(
    Hypothese="H6",
    Modele=x56$model,
    Pays=x56$country,
    Parametre="theta_1 - theta_2",
    H0="theta_1 - theta_2 = 0",
    H1="theta_1 - theta_2 != 0",
    Test="Test de difference sous hypothese d'independance",
    Estimation=x56$diff,
    SE=x56$se_ind,
    Statistique=x56$z_ind,
    P_value=x56$p_ind,
    Decision_5pct=ifelse(
      x56$p_ind < 0.05,
      "Rejet de H0",
      "Non-rejet de H0"
    ),
    stringsAsFactors=FALSE
  )

} else {

  h5 <- data.frame()
  h6 <- data.frame()

}

# ============================================================
# 3. H9
# ============================================================

f9 <- file.path(
  base,
  "results/analysis/FORMAL_H1_H9/H9_GPRT_VS_GPRA_DETAIL.csv"
)

if(file.exists(f9)) {

  x9 <- read.csv(
    f9,
    stringsAsFactors=FALSE,
    check.names=FALSE
  )

  h9 <- data.frame(
    Hypothese="H9",
    Modele="GPRT_SUP vs GPRA_SUP",
    Pays=x9$country,
    Parametre=x9$parameter,
    H0="beta_GPRT = beta_GPRA",
    H1="beta_GPRT != beta_GPRA",
    Test="Test de difference sous hypothese d'independance",
    Estimation=x9$diff,
    SE=x9$se_ind,
    Statistique=x9$z_ind,
    P_value=x9$p_ind,
    Decision_5pct=ifelse(
      x9$p_ind < 0.05,
      "Rejet de H0",
      "Non-rejet de H0"
    ),
    stringsAsFactors=FALSE
  )

} else {

  h9 <- data.frame()

}

# ============================================================
# 4. H4 H7 H8
# ============================================================

non_testable <- data.frame(
  Hypothese=c("H4","H7","H8"),
  Modele=c(
    "Classification exportateur/importateur",
    "Comparaison GPRT/GPRA - PRICE",
    "Comparaison GPRT/GPRA - DEMAND"
  ),
  Pays=NA,
  Parametre=NA,
  H0=c(
    "Parametre = 0",
    "Relation nulle / absence de difference",
    "Relation nulle / absence de difference"
  ),
  H1=c(
    "Parametre != 0",
    "Relation conforme a H7",
    "Relation conforme a H8"
  ),
  Test="Test formel non disponible",
  Estimation=NA,
  SE=NA,
  Statistique=NA,
  P_value=NA,
  Decision_5pct="NON TESTABLE FORMELLEMENT",
  stringsAsFactors=FALSE
)

# ============================================================
# 5. TABLEAU DETAIL FINAL
# ============================================================

final_detail <- rbind(
  detail,
  h5,
  h6,
  h9,
  non_testable
)

write.csv(
  final_detail,
  file.path(out,"H1_H9_TESTS_DETAIL.csv"),
  row.names=FALSE,
  na=""
)

# ============================================================
# 6. SYNTHESE
# ============================================================

summary_rows <- list()

# H1-H3
for(H in c("H1","H2","H3")) {

  z <- final_detail[
    final_detail$Hypothese==H &
    !is.na(final_detail$P_value),
  ]

  if(nrow(z)>0) {

    summary_rows[[length(summary_rows)+1]] <- data.frame(
      Hypothese=H,
      N_tests=nrow(z),
      N_rejet_H0=sum(z$P_value < 0.05),
      N_non_rejet_H0=sum(z$P_value >= 0.05),
      P_min=min(z$P_value,na.rm=TRUE),
      P_mediane=median(z$P_value,na.rm=TRUE),
      Conclusion="Tests individuels disponibles; aucune p-value globale calculee",
      stringsAsFactors=FALSE
    )
  }
}

# H5-H6-H9
for(H in c("H5","H6","H9")) {

  z <- final_detail[
    final_detail$Hypothese==H &
    !is.na(final_detail$P_value),
  ]

  if(nrow(z)>0) {

    summary_rows[[length(summary_rows)+1]] <- data.frame(
      Hypothese=H,
      N_tests=nrow(z),
      N_rejet_H0=sum(z$P_value < 0.05),
      N_non_rejet_H0=sum(z$P_value >= 0.05),
      P_min=min(z$P_value,na.rm=TRUE),
      P_mediane=median(z$P_value,na.rm=TRUE),
      Conclusion="Test de difference sous hypothese d'independance",
      stringsAsFactors=FALSE
    )
  }
}

# H4 H7 H8
summary_rows[[length(summary_rows)+1]] <- data.frame(
  Hypothese="H4",
  N_tests=0,
  N_rejet_H0=NA,
  N_non_rejet_H0=NA,
  P_min=NA,
  P_mediane=NA,
  Conclusion="NON TESTABLE FORMELLEMENT",
  stringsAsFactors=FALSE
)

summary_rows[[length(summary_rows)+1]] <- data.frame(
  Hypothese="H7",
  N_tests=0,
  N_rejet_H0=NA,
  N_non_rejet_H0=NA,
  P_min=NA,
  P_mediane=NA,
  Conclusion="NON TESTABLE FORMELLEMENT",
  stringsAsFactors=FALSE
)

summary_rows[[length(summary_rows)+1]] <- data.frame(
  Hypothese="H8",
  N_tests=0,
  N_rejet_H0=NA,
  N_non_rejet_H0=NA,
  P_min=NA,
  P_mediane=NA,
  Conclusion="NON TESTABLE FORMELLEMENT",
  stringsAsFactors=FALSE
)

summary <- do.call(rbind,summary_rows)

summary <- summary[
  match(
    c("H1","H2","H3","H4","H5","H6","H7","H8","H9"),
    summary$Hypothese
  ),
]

write.csv(
  summary,
  file.path(out,"H1_H9_SYNTHESE.csv"),
  row.names=FALSE,
  na=""
)

# ============================================================
# 7. TXT
# ============================================================

capture.output(
  {
    cat("============================================================\n")
    cat("TABLEAU FINAL DES TESTS H1-H9\n")
    cat("============================================================\n\n")
    print(summary,row.names=FALSE)

    cat("\n\n============================================================\n")
    cat("DETAIL DES TESTS\n")
    cat("============================================================\n\n")
    print(final_detail,row.names=FALSE)
  },
  file=file.path(out,"H1_H9_TESTS_FINAL.txt")
)

cat("\n============================================================\n")
cat("TABLEAUX H1-H9 CREES\n")
cat("============================================================\n")
cat(file.path(out,"H1_H9_TESTS_DETAIL.csv"),"\n")
cat(file.path(out,"H1_H9_SYNTHESE.csv"),"\n")
cat(file.path(out,"H1_H9_TESTS_FINAL.txt"),"\n")
cat("\nTERMINE.\n")
