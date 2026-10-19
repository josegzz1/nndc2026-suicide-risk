# =============================================================================
#  run_all.R - runs the full analysis from the repository root.
#
#  Requires the private registry export `base_actualizada` loaded in the R
#  session and the private exclusion list ../datos_privados/exclusiones.csv
#  (neither is part of this repository). Usage:
#
#      source("path/to/nndc2026-bri-suicide-risk/run_all.R")
#
#  Without the data, preview the figures with simulated data instead:
#      DEMO <- TRUE; source("code/04_models_figures_tables.R")
# =============================================================================

local({
  # Folder of this file; only known when the file is run with source() (or the Source button)
  raiz <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
  if (!file.exists(file.path(raiz, "code", "01_data_preparation.R"))) {
    stop("Cannot find the code/ folder. Run this whole file with ",
         "source(\"path/to/run_all.R\") or with the Source button in RStudio, ",
         "not line by line.", call. = FALSE)
  }
  anterior <- setwd(raiz)
  on.exit(setwd(anterior), add = TRUE)
  assign("DEMO", FALSE, envir = globalenv())
  for (s in c("01_data_preparation.R", "02_table_s1.R", "03_model_assumptions.R",
              "04_models_figures_tables.R", "05_supplementary.R")) {
    message("\n>>> ", s)
    source(file.path("code", s), encoding = "UTF-8")
  }
  message("\nDone: figures in ", file.path(raiz, "figures"), ", results in ", file.path(raiz, "results"),
          " and SUPPLEMENTARY.md updated.")
})
