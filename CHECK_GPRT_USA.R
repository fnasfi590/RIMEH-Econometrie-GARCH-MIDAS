options(stringsAsFactors=FALSE)

library(data.table)

cat("\n============================================\n")
cat(" RIMEH - CHECK GPRT USA\n")
cat("============================================\n\n")

files <- c(
  "data/processed/monthly_model_data_FINAL.csv",
  "data/processed/monthly_clean.csv",
  "data/processed/monthly_market_gpr_data.csv",
  "data/processed/gpr_monthly_2000_2025.csv"
)

for(f in files){

  cat("\n--------------------------------------------\n")
  cat("FILE :",f,"\n")
  cat("--------------------------------------------\n")

  if(!file.exists(f)){
    cat("ABSENT\n")
    next
  }

  x <- fread(f)

  cat("Rows :",nrow(x),"\n")
  cat("Cols :",ncol(x),"\n")

  cat("\nColonnes contenant GPR :\n")
  print(names(x)[grepl("GPR|gpr",names(x))])

  if("country" %in% names(x)){

    u <- unique(x$country)

    cat("\nCountries contenant USA :\n")
    print(u[grepl("USA",u,ignore.case=TRUE)])

    usa <- x[toupper(country)=="USA"]

    cat("\nUSA rows :",nrow(usa),"\n")

    if("GPRT" %in% names(usa)){

      cat("\nGPRT summary USA:\n")
      print(summary(usa$GPRT))

      cat("\nGPRT unique USA:\n")
      print(head(unique(usa$GPRT),30))

    }

    if("GPRA" %in% names(usa)){

      cat("\nGPRA summary USA:\n")
      print(summary(usa$GPRA))

      cat("\nGPRA unique USA:\n")
      print(head(unique(usa$GPRA),30))

    }
  }

  if(!"country" %in% names(x)){

    cat("\nPas de colonne country.\n")

    if("GPRT" %in% names(x)){
      cat("\nGPRT summary:\n")
      print(summary(x$GPRT))

      cat("\nGPRT unique:\n")
      print(head(unique(x$GPRT),30))
    }

    if("GPRA" %in% names(x)){
      cat("\nGPRA summary:\n")
      print(summary(x$GPRA))

      cat("\nGPRA unique:\n")
      print(head(unique(x$GPRA),30))
    }
  }
}

cat("\n============================================\n")
cat(" FIN\n")
cat("============================================\n")