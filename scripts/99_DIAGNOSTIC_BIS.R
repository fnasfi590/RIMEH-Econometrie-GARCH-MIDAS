root <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

cat("\n============================================================\n")
cat(" DIAGNOSTIC STRUCTURE FICHIERS BIS\n")
cat("============================================================\n\n")

files <- list.files(
  file.path(root,"data/raw"),
  pattern="\\.zip$|\\.csv$",
  full.names=TRUE,
  recursive=TRUE,
  ignore.case=TRUE
)

cat("FICHIERS TROUVES :\n\n")
print(files)

cat("\n============================================================\n")
cat(" FICHIERS CSV BIS\n")
cat("============================================================\n")

csvs <- files[
  grepl(
    "BIS|WS_CBPOL|CPI|LONG",
    basename(files),
    ignore.case=TRUE
  ) &
  grepl("\\.csv$",files,ignore.case=TRUE)
]

if(length(csvs)==0){
  cat("\nAucun CSV BIS trouve directement dans data/raw.\n")
}

for(f in csvs){

  cat("\n------------------------------------------------------------\n")
  cat("FICHIER :",f,"\n")
  cat("------------------------------------------------------------\n")

  x <- tryCatch(
    read.csv(
      f,
      nrows=10,
      check.names=FALSE,
      stringsAsFactors=FALSE
    ),
    error=function(e){
      cat("ERREUR :",conditionMessage(e),"\n")
      NULL
    }
  )

  if(!is.null(x)){
    cat("\nCOLONNES :\n")
    print(names(x))

    cat("\nPREMIERES LIGNES :\n")
    print(x)
  }
}

cat("\n============================================================\n")
cat(" CONTENU DES ZIP BIS\n")
cat("============================================================\n")

zips <- files[
  grepl("\\.zip$",files,ignore.case=TRUE)
]

for(z in zips){

  cat("\n------------------------------------------------------------\n")
  cat("ZIP :",basename(z),"\n")
  cat("------------------------------------------------------------\n")

  zz <- tryCatch(
    unzip(z,list=TRUE),
    error=function(e){
      cat("ERREUR :",conditionMessage(e),"\n")
      NULL
    }
  )

  if(!is.null(zz)){
    print(
      head(
        zz,
        30
      )
    )
  }
}

cat("\n============================================================\n")
cat(" FIN DIAGNOSTIC\n")
cat("============================================================\n")
