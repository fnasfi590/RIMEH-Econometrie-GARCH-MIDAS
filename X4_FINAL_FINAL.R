options(stringsAsFactors=FALSE)

cat("\n============================================================\n")
cat(" RIMEH - X.4 FINAL\n")
cat("============================================================\n")

base_dir <- getwd()
outdir <- file.path(base_dir,"results","analysis","X4_PRELIMINARY_TESTS")
dir.create(outdir,recursive=TRUE,showWarnings=FALSE)

# ============================================================
# PACKAGES
# ============================================================

if(!requireNamespace("openxlsx",quietly=TRUE))
    install.packages("openxlsx",repos="https://cloud.r-project.org")

if(!requireNamespace("officer",quietly=TRUE))
    install.packages("officer",repos="https://cloud.r-project.org")

suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(officer))

cat("Packages OK\n")

# ============================================================
# DONNEES
# ============================================================

monthly_file <- file.path(
    base_dir,
    "data",
    "processed",
    "monthly_model_data.csv"
)

daily_file <- file.path(
    base_dir,
    "data",
    "processed",
    "daily_clean.csv"
)

monthly <- read.csv(
    monthly_file,
    check.names=FALSE,
    stringsAsFactors=FALSE
)

daily <- read.csv(
    daily_file,
    check.names=FALSE,
    stringsAsFactors=FALSE
)

cat(
    "monthly_model_data : ",
    nrow(monthly),
    " x ",
    ncol(monthly),
    "\n",
    sep=""
)

cat(
    "daily_clean : ",
    nrow(daily),
    " x ",
    ncol(daily),
    "\n",
    sep=""
)

# ============================================================
# RESULTATS X4
# ============================================================

read_x4 <- function(name){

    f <- file.path(outdir,name)

    if(!file.exists(f)){
        cat("Absent :",name,"\n")
        return(data.frame())
    }

    if(file.info(f)$size==0){
        cat("Vide :",name,"\n")
        return(data.frame())
    }

    x <- read.csv(
        f,
        check.names=FALSE,
        stringsAsFactors=FALSE
    )

    as.data.frame(x)
}

stationarity <- read_x4(
    "X4_STATIONARITY_ADF_PP_KPSS.csv"
)

arch <- read_x4(
    "X4_ARCH_LM.csv"
)

ljung <- read_x4(
    "X4_LJUNG_BOX.csv"
)

vif <- read_x4(
    "X4_VIF_CORRECTED.csv"
)

cat("Stationnarite :",nrow(stationarity),"\n")
cat("ARCH-LM :",nrow(arch),"\n")
cat("Ljung-Box :",nrow(ljung),"\n")
cat("VIF :",nrow(vif),"\n")

# ============================================================
# DIAGNOSTIC VARIABLES
# ============================================================

vars <- c(
    "GPRT",
    "GPRA",
    "SUP",
    "DEM",
    "PRE",
    "VIX",
    "inflation",
    "interest_rate",
    "exchange_rate",
    "GDP"
)

diag <- data.frame(
    Variable=character(),
    N_total=integer(),
    N_NA=integer(),
    N_Inf=integer(),
    N_Finite=integer(),
    Mean=numeric(),
    SD=numeric(),
    Min=numeric(),
    Median=numeric(),
    Max=numeric(),
    stringsAsFactors=FALSE
)

for(v in vars){

    if(v %in% names(monthly)){

        x <- suppressWarnings(
            as.numeric(monthly[[v]])
        )

        finite <- is.finite(x)

        diag <- rbind(
            diag,
            data.frame(
                Variable=v,
                N_total=length(x),
                N_NA=sum(is.na(x)),
                N_Inf=sum(is.infinite(x)),
                N_Finite=sum(finite),
                Mean=if(sum(finite)>0) mean(x[finite]) else NA_real_,
                SD=if(sum(finite)>1) sd(x[finite]) else NA_real_,
                Min=if(sum(finite)>0) min(x[finite]) else NA_real_,
                Median=if(sum(finite)>0) median(x[finite]) else NA_real_,
                Max=if(sum(finite)>0) max(x[finite]) else NA_real_
            )
        )
    }
}

# ============================================================
# CORRELATION
# ============================================================

cat("Construction matrice de correlation...\n")

corr_vars <- c(
    "GPRT",
    "GPRA",
    "SUP",
    "DEM",
    "PRE",
    "VIX",
    "inflation"
)

corr_vars <- corr_vars[
    corr_vars %in% names(monthly)
]

correlation <- data.frame()

if(length(corr_vars)>=2){

    X <- monthly[,corr_vars,drop=FALSE]

    X <- as.data.frame(
        lapply(
            X,
            function(z)
                suppressWarnings(as.numeric(z))
        ),
        stringsAsFactors=FALSE
    )

    names(X) <- corr_vars

    X <- X[
        complete.cases(X),
        ,
        drop=FALSE
    ]

    if(nrow(X)>10){

        C <- round(
            cor(X),
            4
        )

        correlation <- as.data.frame(
            C,
            stringsAsFactors=FALSE
        )

        correlation <- cbind(
            Variable=rownames(C),
            correlation
        )

        rownames(correlation) <- NULL
    }
}

cat(
    "Correlation : ",
    nrow(correlation),
    " x ",
    ncol(correlation),
    "\n",
    sep=""
)

# ============================================================
# RESUMES
# ============================================================

stationarity_summary <- data.frame()

if(nrow(stationarity)>0){

    stationarity_summary <- data.frame(
        Test=c(
            "ADF",
            "Phillips-Perron",
            "KPSS"
        ),
        N_tests=rep(nrow(stationarity),3),
        Stationary_5pct=c(
            sum(
                stationarity$ADF_stationary_5pct %in% TRUE,
                na.rm=TRUE
            ),
            sum(
                stationarity$PP_stationary_5pct %in% TRUE,
                na.rm=TRUE
            ),
            sum(
                stationarity$KPSS_stationary_5pct %in% TRUE,
                na.rm=TRUE
            )
        )
    )

    stationarity_summary$Non_stationary_5pct <-
        stationarity_summary$N_tests -
        stationarity_summary$Stationary_5pct
}

arch_summary <- data.frame()

if(nrow(arch)>0){

    arch_summary <- data.frame(
        N_pays=nrow(arch),
        ARCH_significatif_5pct=sum(
            arch$ARCH_effect_5pct %in% TRUE,
            na.rm=TRUE
        ),
        ARCH_non_significatif_5pct=sum(
            arch$ARCH_effect_5pct %in% FALSE,
            na.rm=TRUE
        )
    )
}

ljung_summary <- data.frame()

if(nrow(ljung)>0){

    ljung_summary <- data.frame(
        N_pays=nrow(ljung),
        Returns_significatifs_5pct=sum(
            ljung$autocorrelation_returns_5pct %in% TRUE,
            na.rm=TRUE
        ),
        Returns_carres_significatifs_5pct=sum(
            ljung$dependence_squared_returns_5pct %in% TRUE,
            na.rm=TRUE
        )
    )
}

vif_summary <- data.frame()

if(nrow(vif)>0){

    vif_summary <- vif

    if("VIF" %in% names(vif_summary)){

        vif_summary$VIF <- suppressWarnings(
            as.numeric(vif_summary$VIF)
        )

        vif_summary$Interpretation <- ifelse(
            vif_summary$VIF < 5,
            "Pas de multicollinearite importante",
            ifelse(
                vif_summary$VIF < 10,
                "Multicollinearite moderee",
                "Multicollinearite forte"
            )
        )
    }
}

count_valid <- function(v){

    if(!(v %in% names(monthly)))
        return(0)

    x <- suppressWarnings(
        as.numeric(monthly[[v]])
    )

    sum(is.finite(x))
}

global_summary <- data.frame(
    Element=c(
        "Observations mensuelles",
        "Observations quotidiennes",
        "Tests stationnarite",
        "Pays ARCH-LM",
        "Pays Ljung-Box",
        "Variables VIF",
        "Inflation valide",
        "Interest rate valide",
        "Exchange rate valide",
        "GDP valide"
    ),
    Valeur=c(
        nrow(monthly),
        nrow(daily),
        nrow(stationarity),
        nrow(arch),
        nrow(ljung),
        nrow(vif),
        count_valid("inflation"),
        count_valid("interest_rate"),
        count_valid("exchange_rate"),
        count_valid("GDP")
    )
)

# ============================================================
# EXCEL
# ============================================================

cat("\nGeneration Excel...\n")

xlsx_file <- file.path(
    outdir,
    "X4_FINAL_TESTS_ECONOMETRIQUES.xlsx"
)

wb <- openxlsx::createWorkbook()

write_sheet <- function(name,x){

    x <- as.data.frame(x)

    openxlsx::addWorksheet(
        wb,
        name
    )

    if(nrow(x)==0){

        openxlsx::writeData(
            wb,
            name,
            "Aucune donnee disponible"
        )

    } else {

        openxlsx::writeData(
            wb,
            name,
            x,
            withFilter=TRUE
        )

        openxlsx::freezePane(
            wb,
            name,
            firstRow=TRUE
        )

        openxlsx::setColWidths(
            wb,
            name,
            cols=1:ncol(x),
            widths="auto"
        )
    }
}

write_sheet("Resume_global",global_summary)
write_sheet("Stationnarite",stationarity)
write_sheet("Resume_stationnarite",stationarity_summary)
write_sheet("ARCH_LM",arch)
write_sheet("Resume_ARCH",arch_summary)
write_sheet("Ljung_Box",ljung)
write_sheet("Resume_Ljung",ljung_summary)
write_sheet("VIF",vif_summary)
write_sheet("Diagnostic_variables",diag)
write_sheet("Correlation",correlation)

openxlsx::saveWorkbook(
    wb,
    xlsx_file,
    overwrite=TRUE
)

cat("EXCEL OK\n")

# ============================================================
# WORD
# ============================================================

cat("\nGeneration Word...\n")

docx_file <- file.path(
    outdir,
    "X4_FINAL_RAPPORT_ECONOMETRIQUE.docx"
)

doc <- officer::read_docx()

doc <- officer::body_add_par(
    doc,
    "RIMEH - TESTS ECONOMETRIQUES PRELIMINAIRES X.4",
    style="heading 1"
)

doc <- officer::body_add_par(
    doc,
    paste0(
        "Rapport final - ",
        format(Sys.time(),"%d/%m/%Y %H:%M")
    )
)

doc <- officer::body_add_par(
    doc,
    "1. Donnees",
    style="heading 2"
)

doc <- officer::body_add_par(
    doc,
    paste0(
        "La base mensuelle contient ",
        nrow(monthly),
        " observations et ",
        ncol(monthly),
        " variables. La base quotidienne contient ",
        nrow(daily),
        " observations."
    )
)

add_table_text <- function(doc,x){

    x <- as.data.frame(x)

    if(nrow(x)==0){

        return(
            officer::body_add_par(
                doc,
                "Aucune donnee disponible."
            )
        )
    }

    doc <- officer::body_add_par(
        doc,
        paste(names(x),collapse=" | ")
    )

    for(i in seq_len(nrow(x))){

        doc <- officer::body_add_par(
            doc,
            paste(
                as.character(x[i,]),
                collapse=" | "
            )
        )
    }

    doc
}

doc <- officer::body_add_par(
    doc,
    "2. Tests de stationnarite",
    style="heading 2"
)

doc <- add_table_text(
    doc,
    stationarity_summary
)

doc <- officer::body_add_par(
    doc,
    "Les tests ADF, Phillips-Perron et KPSS sont interpretes conjointement. Lorsque leurs conclusions different, les resultats sont presentes comme des evidences mixtes."
)

doc <- officer::body_add_par(
    doc,
    "3. Test ARCH-LM",
    style="heading 2"
)

doc <- add_table_text(
    doc,
    arch_summary
)

if(nrow(arch)>0){

    n_sig <- sum(
        arch$ARCH_effect_5pct %in% TRUE,
        na.rm=TRUE
    )

    doc <- officer::body_add_par(
        doc,
        paste0(
            n_sig,
            " pays sur ",
            nrow(arch),
            " presentent un effet ARCH significatif au seuil de 5 %."
        )
    )
}

doc <- officer::body_add_par(
    doc,
    "4. Tests de Ljung-Box",
    style="heading 2"
)

doc <- add_table_text(
    doc,
    ljung_summary
)

doc <- officer::body_add_par(
    doc,
    "5. Multicollinearite - VIF",
    style="heading 2"
)

doc <- add_table_text(
    doc,
    vif_summary
)

doc <- officer::body_add_par(
    doc,
    "6. Diagnostic des variables",
    style="heading 2"
)

doc <- add_table_text(
    doc,
    diag
)

doc <- officer::body_add_par(
    doc,
    "7. Conclusion X.4",
    style="heading 2"
)

doc <- officer::body_add_par(
    doc,
    paste0(
        "Les tests preliminaires X.4 documentent les proprietes statistiques des donnees avant l'estimation GARCH-MIDAS. Les tests ARCH-LM et Ljung-Box des rendements au carre documentent la dynamique de variance conditionnelle. Le VIF permet d'examiner la multicollinearite des variables disponibles."
    )
)

doc <- officer::body_add_par(
    doc,
    paste0(
        "La variable inflation dispose de ",
        count_valid("inflation"),
        " observations valides sur ",
        nrow(monthly),
        ". Les variables interest_rate, exchange_rate et GDP ne disposent actuellement d'aucune observation valide dans monthly_model_data.csv."
    )
)

print(
    doc,
    target=docx_file
)

cat("WORD OK\n")

# ============================================================
# CSV
# ============================================================

write.csv(
    diag,
    file.path(
        outdir,
        "X4_VARIABLES_DIAGNOSTIC_FINAL.csv"
    ),
    row.names=FALSE
)

write.csv(
    correlation,
    file.path(
        outdir,
        "X4_CORRELATION_MATRIX_FINAL.csv"
    ),
    row.names=FALSE
)

write.csv(
    vif_summary,
    file.path(
        outdir,
        "X4_VIF_FINAL.csv"
    ),
    row.names=FALSE
)

# ============================================================
# TXT
# ============================================================

txt_file <- file.path(
    outdir,
    "X4_FINAL_RAPPORT_ECONOMETRIQUE.txt"
)

sink(txt_file)

cat("RIMEH - X.4 FINAL\n")
cat("=================\n\n")
cat("Date :",format(Sys.time()),"\n\n")

cat("RESUME GLOBAL\n")
print(global_summary)

cat("\nSTATIONNARITE\n")
print(stationarity_summary)

cat("\nARCH-LM\n")
print(arch_summary)

cat("\nLJUNG-BOX\n")
print(ljung_summary)

cat("\nVIF\n")
print(vif_summary)

cat("\nDIAGNOSTIC VARIABLES\n")
print(diag)

cat("\nCORRELATION\n")
print(correlation)

sink()

cat("\n============================================================\n")
cat(" X.4 FINAL TERMINE AVEC SUCCES\n")
cat("============================================================\n\n")

cat("Excel :",xlsx_file,"\n")
cat("Word  :",docx_file,"\n")
cat("TXT   :",txt_file,"\n")
cat("CSV VIF :",file.path(outdir,"X4_VIF_FINAL.csv"),"\n")
cat("CSV diagnostic :",file.path(outdir,"X4_VARIABLES_DIAGNOSTIC_FINAL.csv"),"\n")
cat("CSV correlation :",file.path(outdir,"X4_CORRELATION_MATRIX_FINAL.csv"),"\n")

cat("\n============================================================\n")