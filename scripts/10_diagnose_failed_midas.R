lines <- readLines("scripts/08_garch_midas_models.R", warn = FALSE)
marker <- 'countries <- sort(unique(daily[["country"]]))'
cut <- which(trimws(lines) == marker)[1]

eval(parse(text = paste(lines[1:(cut - 1)], collapse = "\n")),
     envir = .GlobalEnv)

tests <- list(
  c("GBR","SUP_PRE"),
  c("MEX","GPRT_SUP"),
  c("NOR","SUP_PRE"),
  c("USA","GPRA_SUP")
)

for (x in tests) {
  cc <- x[1]
  nm <- x[2]
  vars <- switch(
    nm,
    SUP_PRE = c("SUP","PRE"),
    GPRT_SUP = c("GPRT","SUP"),
    GPRA_SUP = c("GPRA","SUP")
  )

  cat("\n========================================\n")
  cat(cc, "/", nm, "\n")
  cat("========================================\n")

  m <- monthly %>%
    dplyr::filter(country == cc) %>%
    dplyr::arrange(date)

  for (v in vars) {
    z <- m %>%
      dplyr::select(date, value = dplyr::all_of(v)) %>%
      tidyr::drop_na()

    cat(
      v,
      ": N =", nrow(z),
      "| min =", min(z$value),
      "| max =", max(z$value),
      "| sd =", sd(z$value),
      "\n"
    )
  }

  z1 <- m %>%
    dplyr::select(date, v1 = dplyr::all_of(vars[1])) %>%
    tidyr::drop_na()

  z2 <- m %>%
    dplyr::select(date, v2 = dplyr::all_of(vars[2])) %>%
    tidyr::drop_na()

  common <- dplyr::inner_join(z1, z2, by = "date")

  cat(
    "Common monthly N =", nrow(common),
    "| correlation =", cor(common$v1, common$v2),
    "\n"
  )
}
