# =============================================================================
#  01_data_preparation.R
#
#  NNDC 2026 poster - Body Roundness Index, conventional anthropometrics and
#  diet quality (REAP-S) for identifying suicide risk at first consultation in
#  major depressive disorder (MDD).
#
#  Builds the analytic sample (n = 652) from the clinic registry export.
#
#  Input  : `base_actualizada`, the patient-level registry export. It is NOT part
#           of this repository and must already be loaded in the R session.
#           ../datos_privados/exclusiones.csv: record numbers excluded after
#           manual review. Private file kept OUTSIDE the repository.
#  Output : `tble_final` and `tble_final_ordinal` in the R session, and
#           results/participant_flow.csv (counts only).
#
#  Run from the repository root (see run_all.R).
# =============================================================================

# ---- Settings ------------------------------------------------------------------
# Private file with columns registro (record number), edad (age) and motivo (reason).
# An empty edad excludes every visit of that record. Never uploaded to the repository.
archivo_exclusiones <- file.path("..", "datos_privados", "exclusiones.csv")
carpeta_res <- "results"

if (!exists("base_actualizada")) {
  stop("Cannot find `base_actualizada` (the registry export; not included in this repository). ",
       "Load it in the R session before running this script.")
}
if (!file.exists(archivo_exclusiones)) {
  stop("Cannot find the private exclusion file: ",
       normalizePath(archivo_exclusiones, mustWork = FALSE))
}

# ---- Registry columns -----------------------------------------------------------
col_dx     <- "Discapacidad intelectual (Trastorno del desarrollo intelectual)"  # primary diagnosis
col_reg    <- "No. REGISTRO"                      # record number (used only for the exclusions)
col_riesgo <- "Riesgo de Suicidio"                # 0 = no risk, 1 = risk
col_cat    <- "Categor\u00edas de Riesgo Suicida"     # 0 = none, 1 = low, 2-4 = higher
dx_tdm     <- "Trastorno de depresi\u00f3n mayor"
columnas <- c("PESO (kg)", "ESTATURA (m)", "EDAD", "CINTURA (cm)", "CADERA (cm)",
              "Total...37",                       # REAP-S total score
              col_dx, col_reg, col_riesgo, col_cat,
              "SEXO")                             # 1 = female, 2 = male
# Complete data on everything except age and record number (as in the original analysis)
vars_requeridas <- setdiff(columnas, c("EDAD", col_reg))

flujo <- data.frame(step = "Registry records", n = nrow(base_actualizada))
anotar <- function(paso, datos) flujo[nrow(flujo) + 1, ] <<- list(paso, nrow(datos))

# ---- 1. Selection, complete data and MDD -------------------------------------------
tble_final <- base_actualizada |>
  dplyr::select(dplyr::all_of(columnas)) |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(vars_requeridas), ~ !is.na(.x)))
anotar("Complete data (anthropometrics, REAP-S, diagnosis, suicide risk, sex)", tble_final)

tble_final <- dplyr::filter(tble_final, .data[[col_dx]] == dx_tdm)
anotar("Major depressive disorder", tble_final)

# ---- 2. Exclusions after manual review (private file) ------------------------------
# Reasons: one duplicated patient (second visit removed) and one record with
# inconsistent risk coding (risk = 1 but risk category = 0).
excl <- utils::read.csv(archivo_exclusiones, colClasses = "character",
                        na.strings = c("", "NA"), encoding = "UTF-8")
stopifnot(all(c("registro", "edad") %in% names(excl)))
for (i in seq_len(nrow(excl))) {
  if (is.na(excl$edad[i])) {
    tble_final <- dplyr::filter(tble_final, .data[[col_reg]] != excl$registro[i])
  } else {
    tble_final <- dplyr::filter(tble_final, !(.data[[col_reg]] == excl$registro[i] &
                                                EDAD == as.numeric(excl$edad[i])))
  }
}
anotar("After exclusions on manual review (duplicate visit; inconsistent risk coding)", tble_final)

# ---- 3. Names, anthropometric indices and risk categories -------------------------
tble_final <- tble_final |>
  dplyr::rename(REAPS = "Total...37",
                Suicide_Risk = dplyr::all_of(col_riesgo),
                Risk_Categories = dplyr::all_of(col_cat)) |>
  dplyr::mutate(
    ESTATURA_cm = `ESTATURA (m)` * 100,
    BMI  = `PESO (kg)` / `ESTATURA (m)`^2,         # kg/m2
    WHR  = `CINTURA (cm)` / `CADERA (cm)`,          # waist / hip
    WHtR = `CINTURA (cm)` / ESTATURA_cm,            # waist / height
    # BRI (Thomas et al., Obesity 2013), with waist and height in meters
    BRI  = 364.2 - 365.5 * sqrt(1 - ((`CINTURA (cm)` / 100) / (2 * pi))^2 / (0.5 * `ESTATURA (m)`)^2),
    # Severity: 0 = none, 1 = low, 2 = categories 2 to 4 combined
    Risk_Categories = replace(Risk_Categories, Risk_Categories %in% c(2, 3, 4), 2)
  ) |>
  dplyr::rename(Cintura_cm = "CINTURA (cm)", Estatura_m = "ESTATURA (m)",
                Peso_kg = "PESO (kg)", Cadera_cm = "CADERA (cm)")

# Version with the ordinal outcome as a factor (for polr and the Brant test)
tble_final_ordinal <- tble_final |>
  dplyr::mutate(Risk_Categories_f = factor(Risk_Categories, levels = c(0, 1, 2),
                                           labels = c("None", "Low", "High"), ordered = TRUE))

# ---- 4. Checks and participant flow ---------------------------------------------------
if (!all(tble_final$Suicide_Risk == as.integer(tble_final$Risk_Categories > 0))) {
  warning("Some records have a binary risk different from (category > 0).")
}
if (nrow(tble_final) != 652) warning("n = ", nrow(tble_final), "; the poster used n = 652.")
dir.create(carpeta_res, showWarnings = FALSE)
utils::write.csv(flujo, file.path(carpeta_res, "participant_flow.csv"), row.names = FALSE)
message("Analytic sample: n = ", nrow(tble_final),
        " (with suicide risk: ", sum(tble_final$Suicide_Risk == 1), ")")
