# =============================================================================
#  03_model_assumptions.R
#
#  Assumption checks for the logistic models, unadjusted and adjusted for age
#  and sex:
#   1. Brant test (proportional odds) for the ordinal models.
#   2. Box-Tidwell test (linearity of the logit) for each index and for age,
#      binomial and ordinal models; p-values from likelihood-ratio tests.
#   3. Cook's distance (threshold 4/n) in the binomial models: odds ratios per
#      1 SD with and without influential observations.
#
#  Input  : `tble_final_ordinal` (from 01_data_preparation.R).
#  Output : results/assumptions_brant.csv, results/assumptions_box_tidwell.csv
#           and results/assumptions_cook.csv (aggregated statistics only).
# =============================================================================

suppressPackageStartupMessages({
  library(MASS)    # polr()
  library(brant)   # brant()
})
if (!exists("tble_final_ordinal")) stop("Run code/01_data_preparation.R first")
carpeta_res <- "results"
dir.create(carpeta_res, showWarnings = FALSE)

# Predictors: name in the poster = column in the data (same order as the poster)
predictores <- c("BMI" = "BMI", "Waist circumference" = "Cintura_cm", "Waist-to-hip ratio" = "WHR",
                 "Waist-to-height ratio" = "WHtR", "Body Roundness Index" = "BRI", "REAP-S" = "REAPS")

# Data for the adjusted models: sex as a factor and standardized indices (z)
datos_aj <- dplyr::mutate(tble_final_ordinal, SEXO = factor(SEXO))
for (v in predictores) datos_aj[[paste0("z_", v)]] <- as.numeric(scale(datos_aj[[v]]))

# Likelihood-ratio p between two nested models (differing by one term)
p_lrt <- function(completo, reducido) {
  stats::pchisq(as.numeric(2 * (stats::logLik(completo) - stats::logLik(reducido))),
                df = 1, lower.tail = FALSE)
}

# ---- 1. Brant test (proportional odds) ----------------------------------------------
# brant() refits the model from its call, so each polr is built with the formula
# written out and a data frame from the global environment (as when typing polr(...)).
polr_global <- function(formula_txt, base) {
  eval(parse(text = paste0("polr(", formula_txt, ", data = ", base, ", Hess = TRUE)")),
       envir = globalenv())
}
termino <- function(x, v) {
  ifelse(x == "Omnibus", "Omnibus",
         ifelse(x %in% c(v, paste0("z_", v)), "Index",
                ifelse(x == "EDAD", "Age", ifelse(x == "SEXO2", "Male sex", x))))
}
tabla_brant <- function(modelo, nombre, v, ajuste) {
  b <- NULL
  invisible(utils::capture.output(b <- brant::brant(modelo)))   # brant() prints its table; keep the result
  data.frame(predictor = nombre, model = ajuste, term = termino(rownames(b), v),
             X2 = b[, "X2"], df = b[, "df"], p = b[, "probability"], row.names = NULL)
}
res_brant <- do.call(rbind, lapply(names(predictores), function(nom) {
  v <- predictores[[nom]]
  rbind(tabla_brant(polr_global(paste0("Risk_Categories_f ~ ", v), "tble_final_ordinal"),
                    nom, v, "Unadjusted"),
        tabla_brant(polr_global(paste0("Risk_Categories_f ~ z_", v, " + EDAD + SEXO"), "datos_aj"),
                    nom, v, "Adjusted"))
}))

# ---- 2. Box-Tidwell (linearity of the logit) --------------------------------------------
# x*log(x) is added to the model, and the models with and without that term are
# compared by likelihood ratio (in polr the Wald p of that term can be falsely
# small). The index is used on its original scale because x*log(x) needs x > 0;
# if the logit is linear in x it is also linear in z. In the adjusted models
# both the index and age are tested.
box_tidwell <- function(nom) {
  d <- datos_aj |>
    dplyr::transmute(Suicide_Risk, Risk_Categories_f, EDAD, SEXO, x = .data[[predictores[[nom]]]]) |>
    tidyr::drop_na() |>
    dplyr::mutate(x_logx = x * log(x), edad_logedad = EDAD * log(EDAD))
  stopifnot(all(d$x > 0), all(d$EDAD > 0))
  bin <- function(f) stats::glm(f, data = d, family = binomial)
  ord <- function(f) MASS::polr(f, data = d, Hess = TRUE)
  b_aj <- bin(Suicide_Risk ~ x + x_logx + EDAD + edad_logedad + SEXO)
  o_aj <- ord(Risk_Categories_f ~ x + x_logx + EDAD + edad_logedad + SEXO)
  data.frame(
    predictor = nom,
    outcome = rep(c("Binomial", "Ordinal"), each = 3),
    model   = rep(c("Unadjusted", "Adjusted", "Adjusted"), 2),
    term    = rep(c("Index", "Index", "Age"), 2),
    p = c(p_lrt(bin(Suicide_Risk ~ x + x_logx), bin(Suicide_Risk ~ x)),
          p_lrt(b_aj, bin(Suicide_Risk ~ x + EDAD + edad_logedad + SEXO)),
          p_lrt(b_aj, bin(Suicide_Risk ~ x + x_logx + EDAD + SEXO)),
          p_lrt(ord(Risk_Categories_f ~ x + x_logx), ord(Risk_Categories_f ~ x)),
          p_lrt(o_aj, ord(Risk_Categories_f ~ x + EDAD + edad_logedad + SEXO)),
          p_lrt(o_aj, ord(Risk_Categories_f ~ x + x_logx + EDAD + SEXO))))
}
res_bt <- do.call(rbind, lapply(names(predictores), box_tidwell))

# ---- 3. Cook's distance (binomial models) -----------------------------------------------
# Influential observations: Cook's distance > 4/n. Each model is refitted without
# them and the OR per 1 SD is compared (SD of the full sample, kept fixed). Cook's
# distances do not change when the index is rescaled.
umbral_cook <- 4 / nrow(datos_aj)
res_cook <- do.call(rbind, lapply(names(predictores), function(nom) {
  z <- paste0("z_", predictores[[nom]])
  do.call(rbind, lapply(c("Unadjusted", "Adjusted"), function(ajuste) {
    f <- stats::reformulate(if (ajuste == "Adjusted") c(z, "EDAD", "SEXO") else z,
                            response = "Suicide_Risk")
    m     <- stats::glm(f, data = datos_aj, family = binomial)
    idx   <- which(stats::cooks.distance(m) > umbral_cook)
    m_sin <- stats::glm(f, data = if (length(idx)) datos_aj[-idx, ] else datos_aj, family = binomial)
    or_1 <- exp(stats::coef(m)[[z]]); or_2 <- exp(stats::coef(m_sin)[[z]])
    data.frame(predictor = nom, model = ajuste, n_influential = length(idx),
               or_all = or_1, p_all = stats::coef(summary(m))[z, 4],
               or_without = or_2, p_without = stats::coef(summary(m_sin))[z, 4],
               change_pct = 100 * (or_2 - or_1) / or_1)
  }))
}))

# ---- Save and summarize --------------------------------------------------------------------------
utils::write.csv(res_brant, file.path(carpeta_res, "assumptions_brant.csv"), row.names = FALSE)
utils::write.csv(res_bt,    file.path(carpeta_res, "assumptions_box_tidwell.csv"), row.names = FALSE)
utils::write.csv(res_cook,  file.path(carpeta_res, "assumptions_cook.csv"), row.names = FALSE)

cat("\n=== Brant (p < 0.05 = proportional odds not met) ===\n")
print(res_brant[res_brant$term %in% c("Omnibus", "Index"), ], digits = 3, row.names = FALSE)
cat("\n=== Box-Tidwell (p < 0.05 = evidence of non-linearity) ===\n")
print(res_bt, digits = 3, row.names = FALSE)
cat("\n=== Cook (threshold 4/n =", signif(umbral_cook, 3), ") ===\n")
print(res_cook, digits = 3, row.names = FALSE)
