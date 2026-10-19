# =============================================================================
#  02_table_s1.R
#
#  Supplementary Table S1: characteristics of the sample, overall, by suicide
#  risk and by sex, built with gtsummary (as in the original analysis).
#
#  Input  : `tble_final_ordinal` (from 01_data_preparation.R).
#  Output : results/table_s1_characteristics.md (aggregated values only). The
#           formatted table is also shown in the RStudio Viewer.
#  Tests  : Welch t-test (means), Wilcoxon rank-sum test (age), Pearson
#           chi-square test without continuity correction (categorical).
# =============================================================================

suppressPackageStartupMessages(library(gtsummary))
if (!exists("tble_final_ordinal")) stop("Run code/01_data_preparation.R first")
carpeta_res <- "results"
dir.create(carpeta_res, showWarnings = FALSE)

datos_t1 <- tble_final_ordinal |>
  dplyr::mutate(
    Riesgo     = factor(Suicide_Risk, levels = c(0, 1),
                        labels = c("No suicide risk", "Suicide risk")),
    Sexo       = factor(SEXO, levels = c(1, 2), labels = c("Women", "Men")),
    Riesgo_cat = factor(Risk_Categories, levels = c(0, 1, 2),
                        labels = c("None", "Low", "High"))
  ) |>
  dplyr::select(Riesgo, Sexo, EDAD, Peso_kg, Estatura_m, BMI, Cintura_cm,
                Cadera_cm, WHR, WHtR, BRI, REAPS, Riesgo_cat)

# English labels stored as a column attribute (used by the three tables)
etiquetas_t1 <- c(
  Sexo = "Sex", EDAD = "Age (years)", Peso_kg = "Weight (kg)", Estatura_m = "Height (m)",
  BMI = "BMI (kg/m\u00b2)", Cintura_cm = "Waist circumference (cm)",
  Cadera_cm = "Hip circumference (cm)", WHR = "Waist-to-hip ratio",
  WHtR = "Waist-to-height ratio", BRI = "Body Roundness Index",
  REAPS = "REAP-S score", Riesgo_cat = "Suicide risk category"
)
for (v in names(etiquetas_t1)) attr(datos_t1[[v]], "label") <- etiquetas_t1[[v]]

estadisticos_t1 <- list(all_continuous() ~ "{mean} ({sd})",
                        EDAD ~ "{median} ({p25}, {p75})",
                        all_categorical() ~ "{n} ({p}%)")

decimales_t1 <- list(all_continuous() ~ 1,
                     EDAD ~ 0,
                     c(Estatura_m, WHR, WHtR) ~ 2,
                     all_categorical() ~ c(0, 1))

pruebas_t1 <- list(all_continuous() ~ "t.test",                 # Welch: unequal variances
                   EDAD ~ "wilcox.test",                         # age as median (Q1, Q3)
                   all_categorical() ~ "chisq.test.no.correct")  # Pearson chi-square

# (a) Overall column
t_total <- datos_t1 |>
  dplyr::select(-Riesgo) |>
  tbl_summary(statistic = estadisticos_t1, digits = decimales_t1) |>
  add_stat_label() |>
  modify_header(all_stat_cols() ~ "**Overall** (N = {N})")

# (b) No suicide risk vs suicide risk
t_riesgo <- datos_t1 |>
  dplyr::select(-Riesgo_cat) |>
  tbl_summary(by = Riesgo, statistic = estadisticos_t1, digits = decimales_t1) |>
  add_p(test = pruebas_t1) |>
  add_stat_label() |>
  bold_p() |>
  modify_header(all_stat_cols() ~ "**{level}** (n = {n})", p.value ~ "**p (risk)**")

# (c) Women vs men
t_sexo <- datos_t1 |>
  dplyr::select(-Riesgo) |>
  tbl_summary(by = Sexo, statistic = estadisticos_t1, digits = decimales_t1) |>
  add_p(test = pruebas_t1) |>
  add_stat_label() |>
  bold_p() |>
  modify_header(all_stat_cols() ~ "**{level}** (n = {n})", p.value ~ "**p (sex)**")

tabla_1 <- tbl_merge(
  tbls = list(t_total, t_riesgo, t_sexo),
  tab_spanner = c("**Total**", "**Suicide risk at first consultation**", "**Sex**")
) |>
  modify_header(label = "**Characteristic**") |>
  bold_labels()

print(tabla_1)   # formatted table in the RStudio Viewer

# Markdown version for GitHub (results/ and SUPPLEMENTARY.md)
md <- as.character(gtsummary::as_kable(tabla_1, format = "pipe"))
nota <- paste0("Mean (SD) unless stated; age: median (Q1, Q3). p: Welch t-test (means), ",
               "Wilcoxon rank-sum test (age), Pearson chi-square test without continuity ",
               "correction (categorical). Suicide risk category: none; low; high ",
               "(categories 2-4 combined).")
writeLines(enc2utf8(c(md, "", nota)), file.path(carpeta_res, "table_s1_characteristics.md"),
           useBytes = TRUE)
message("Table S1 saved to ", file.path(carpeta_res, "table_s1_characteristics.md"))
