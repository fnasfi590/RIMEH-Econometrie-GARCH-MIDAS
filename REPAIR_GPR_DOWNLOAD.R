options(stringsAsFactors=FALSE)

library(readxl)
library(data.table)

cat("\n============================================\n")
cat(" RIMEH - RECUPERATION GPR OFFICIEL\n")
cat("============================================\n\n")

url <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_export_202609.xls"

dest <- "data/raw/data_gpr_export_202609.xls"

dir.create("data/raw",recursive=TRUE,showWarnings=FALSE)

cat("Téléchargement :\n")
cat(url,"\n\n")

ok <- tryCatch({

  download.file(
    url,
    dest,
    mode="wb",
    quiet=FALSE
  )

  TRUE

}, error=function(e){

  cat("\nERREUR téléchargement :\n")
  cat(conditionMessage(e),"\n")

  FALSE
})

if(!ok || !file.exists(dest)){
  stop("Le fichier GPR n'a pas pu être téléchargé.")
}

cat("\nFichier téléchargé :\n")
print(file.info(dest)[,c("size","mtime")])

cat("\n============================================\n")
cat(" FEUILLES EXCEL\n")
cat("============================================\n")

sheets <- excel_sheets(dest)

print(sheets)

for(s in sheets){

  cat("\n--------------------------------------------\n")
  cat("SHEET :",s,"\n")
  cat("--------------------------------------------\n")

  x <- read_excel(
    dest,
    sheet=s,
    .name_repair="minimal"
  )

  cat("Rows :",nrow(x),"\n")
  cat("Cols :",ncol(x),"\n")

  cat("\nNoms des colonnes :\n")
  print(names(x))

  cat("\nColonnes GPR potentielles :\n")
  print(
    names(x)[
      grepl(
        "GPR|GEO|THREAT|ACT",
        names(x),
        ignore.case=TRUE
      )
    ]
  )
}

cat("\n============================================\n")
cat(" FIN INSPECTION GPR\n")
cat("============================================\n")