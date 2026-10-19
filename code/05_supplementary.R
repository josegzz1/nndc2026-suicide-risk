# =============================================================================
#  05_supplementary.R
#
#  Writes SUPPLEMENTARY.md (repository root) from the aggregated files in
#  results/, so the supplementary tables always match the code output.
#
#  Input  : results/*.csv and results/table_s1_characteristics.md (scripts 01-04).
#  Output : SUPPLEMENTARY.md
# =============================================================================

carpeta_res <- "results"
leer <- function(archivo) utils::read.csv(file.path(carpeta_res, archivo), check.names = FALSE,
                                          stringsAsFactors = FALSE, encoding = "UTF-8")
flujo  <- leer("participant_flow.csv")
res    <- leer("model_results.csv")
cortes <- leer("or_by_cutpoint.csv")
cov    <- leer("or_age_sex.csv")
auc_r  <- leer("auc_age_sex.csv")
dl_ref <- leer("delong_vs_age_sex.csv")
dl_par <- leer("delong_pairwise.csv")
brant  <- leer("assumptions_brant.csv")
bt     <- leer("assumptions_box_tidwell.csv")
cook   <- leer("assumptions_cook.csv")
t_s1   <- readLines(file.path(carpeta_res, "table_s1_characteristics.md"), encoding = "UTF-8")

# ---- Formatting ---------------------------------------------------------------------
f2 <- function(v, k = 2) formatC(v, format = "f", digits = k)
f_p <- function(p) ifelse(p < 0.001, "<0.001", ifelse(p > 0.99, ">0.99", f2(p, 3)))
or_ic <- function(or, lo, hi) paste0(f2(or), " (", f2(lo), "\u2013", f2(hi), ")")
negrita <- function(txt, si) ifelse(si, paste0("**", txt, "**"), txt)
tabla_md <- function(df, alinear) {
  df[] <- lapply(df, as.character)          # no space padding in numbers
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(ifelse(alinear == "l", ":---", "---:"), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
orden <- unique(res$predictor)
unidades <- c("BMI" = " kg/m\u00b2", "Waist circumference" = " cm", "REAP-S" = " points")
u <- res[res$model == "Unadjusted", ]
sd_txt <- paste0(u$predictor, " ", signif(u$sd, 2),
                 ifelse(is.na(unidades[u$predictor]), "", unidades[u$predictor]), collapse = "; ")

# ---- S1. Flow ---------------------------------------------------------------------------
s1 <- c("## S1. Participant flow", "",
        tabla_md(data.frame(Step = flujo$step, n = flujo$n, check.names = FALSE), c("l", "r")), "",
        "Two records were excluded after manual review: the second visit of a patient seen twice, and one record ",
        "with inconsistent suicide-risk coding (risk present but risk category none). Their record numbers are kept ",
        "in a private file outside this repository.", "")

# ---- S2. Table S1 -----------------------------------------------------------------------
s2 <- c("## S2. Characteristics of the sample (Table S1)", "", t_s1, "")

# ---- S3. OR per 1 SD ---------------------------------------------------------------------
fila_s3 <- function(r) {
  c(Predictor = r$predictor, Model = r$model,
    `Any risk: OR (95% CI)` = negrita(or_ic(r$or_bin, r$lo_bin, r$hi_bin), r$p_holm_bin < 0.05),
    `p` = f_p(r$p_bin), `Holm p` = f_p(r$p_holm_bin),
    `Severity: OR (95% CI)` = negrita(or_ic(r$or_ord, r$lo_ord, r$hi_ord), r$p_holm_ord < 0.05),
    `p ` = f_p(r$p_ord), `Holm p ` = f_p(r$p_holm_ord))
}
t3 <- do.call(rbind, lapply(seq_len(nrow(res)), function(i) fila_s3(res[i, ])))
t3[duplicated(t3[, "Predictor"]), "Predictor"] <- ""
cov_ancho <- merge(cov[cov$outcome == "Binomial", ], cov[cov$outcome == "Ordinal", ],
                   by = c("predictor", "model"), suffixes = c("_b", "_o"))
cov_ancho <- cov_ancho[order(cov_ancho$predictor, cov_ancho$model == "Adjusted"), ]
t3c <- data.frame(Covariate = ifelse(cov_ancho$predictor == "Age", "Age, per 10 years", "Male sex (vs female)"),
                  Model = ifelse(cov_ancho$model == "Adjusted", "Mutually adjusted", "Unadjusted"),
                  `Any risk: OR (95% CI)` = or_ic(cov_ancho$or_b, cov_ancho$lo_b, cov_ancho$hi_b),
                  p = f_p(cov_ancho$p_b),
                  `Severity: OR (95% CI)` = or_ic(cov_ancho$or_o, cov_ancho$lo_o, cov_ancho$hi_o),
                  `p ` = f_p(cov_ancho$p_o), check.names = FALSE)
t3c$Covariate[duplicated(t3c$Covariate)] <- ""
s3 <- c("## S3. Odds ratios per 1 SD, unadjusted and adjusted for age and sex", "",
        "Any risk: binomial logistic regression (suicide risk present vs absent). Severity: ordinal (proportional-odds) ",
        "logistic regression (none < low < high). Adjusted models include age and sex. 95% CI by profile likelihood; ",
        "Holm correction across the six indices within each model. Bold: Holm p < 0.05.", "",
        tabla_md(as.data.frame(t3, check.names = FALSE), c("l", "l", rep("r", 6))), "",
        paste0("1 SD: ", sd_txt, "."),
        "REAP-S did not meet the proportional-odds assumption (S6); see S4 for its severity odds ratios by cut-point.", "",
        "Age and sex, alone and mutually adjusted:", "",
        tabla_md(t3c, c("l", "l", rep("r", 4))), "")

# ---- S4. OR by cut-point ----------------------------------------------------------------
c_u <- cortes[cortes$model == "Unadjusted", ]; c_a <- cortes[cortes$model == "Adjusted", ]
c_w <- merge(c_u, c_a, by = c("predictor", "cutpoint"), suffixes = c("_u", "_a"))
c_w <- c_w[order(match(c_w$predictor, orden), c_w$cutpoint), ]
t4 <- data.frame(Predictor = c_w$predictor,
                 `Cut-point` = ifelse(c_w$cutpoint == 1, "Any risk vs none", "High vs none/low"),
                 `Unadjusted OR (95% CI)` = negrita(or_ic(c_w$or_u, c_w$lo_u, c_w$hi_u), c_w$hi_u < 1 | c_w$lo_u > 1),
                 p = f_p(c_w$p_u),
                 `Adjusted OR (95% CI)` = negrita(or_ic(c_w$or_a, c_w$lo_a, c_w$hi_a), c_w$hi_a < 1 | c_w$lo_a > 1),
                 `p ` = f_p(c_w$p_a), check.names = FALSE)
t4$Predictor[duplicated(t4$Predictor)] <- ""
s4 <- c("## S4. Odds ratios by cut-point of suicide-risk severity", "",
        "Separate binomial logistic regressions for each cut-point of the ordinal outcome; OR per 1 SD; adjusted ",
        "models include age and sex. The first cut-point is the binomial model of S3. Bold: 95% CI excludes 1.", "",
        tabla_md(t4, c("l", "l", rep("r", 4))), "")

# ---- S5. Discrimination -------------------------------------------------------------------
a_u <- res[res$model == "Unadjusted", ]; a_a <- res[res$model == "Adjusted", ]
a_a <- a_a[match(a_u$predictor, a_a$predictor), ]; d_r <- dl_ref[match(a_u$predictor, dl_ref$predictor), ]
t5 <- data.frame(Predictor = a_u$predictor,
                 `AUC alone (95% CI)` = or_ic(a_u$auc, a_u$auc_lo, a_u$auc_hi),
                 `AUC with age + sex (95% CI)` = or_ic(a_a$auc, a_a$auc_lo, a_a$auc_hi),
                 dAUC = sprintf("%+.3f", d_r$delta_auc),
                 `DeLong p` = f_p(d_r$p_vs_ref), check.names = FALSE)
names(t5)[names(t5) == "dAUC"] <- "\u0394AUC vs age + sex"
ref <- auc_r[auc_r$predictor == "Age + sex", ]
s5 <- c("## S5. Discriminative capacity", "",
        "AUC of the fitted probabilities with DeLong 95% CI. \u0394AUC: change when the index is added to the age + sex ",
        "model, tested with paired DeLong tests.", "",
        tabla_md(t5, c("l", rep("r", 4))), "",
        paste0("Reference models: age + sex ", or_ic(ref$auc, ref$auc_lo, ref$auc_hi),
               "; age alone ", f2(auc_r$auc[auc_r$predictor == "Age"]),
               "; sex alone ", f2(auc_r$auc[auc_r$predictor == "Male sex"]), "."),
        paste0("Pairwise DeLong tests between the six indices alone (", nrow(dl_par), " pairs): smallest p = ",
               f_p(min(dl_par$p)), "; all Holm-adjusted p ", ifelse(min(dl_par$p_holm) >= 0.05,
               paste0("\u2265 ", f2(min(dl_par$p_holm))), "include values < 0.05"), "."), "",
        "![Figure S1. ROC curves](figures/figS1_roc_curves.png)", "",
        "**Figure S1.** ROC curves of the six indices alone and of the age + sex model; gray curve: theoretical ",
        "benchmark (binormal, AUC 0.80).", "")

# ---- S6. Assumptions --------------------------------------------------------------------------
b_u <- brant[brant$model == "Unadjusted" & brant$term == "Index", ]
b_ao <- brant[brant$model == "Adjusted" & brant$term == "Omnibus", ]
b_ai <- brant[brant$model == "Adjusted" & brant$term == "Index", ]
x2 <- function(b) paste0(f2(b$X2), " (", b$df, "), ", f_p(b$p))
t6b <- data.frame(Predictor = orden,
                  u = x2(b_u[match(orden, b_u$predictor), ]),
                  ao = x2(b_ao[match(orden, b_ao$predictor), ]),
                  ai = x2(b_ai[match(orden, b_ai$predictor), ]))
names(t6b) <- c("Predictor", "Unadjusted: \u03c7\u00b2 (df), p", "Adjusted, omnibus: \u03c7\u00b2 (df), p",
                "Adjusted, index: \u03c7\u00b2 (df), p")
p_bt <- function(o, m, t) { s <- bt[bt$outcome == o & bt$model == m & bt$term == t, ]; f_p(s$p[match(orden, s$predictor)]) }
t6t <- data.frame(Predictor = orden,
                  `Any risk, unadjusted` = p_bt("Binomial", "Unadjusted", "Index"),
                  `Any risk, adjusted: index` = p_bt("Binomial", "Adjusted", "Index"),
                  `Any risk, adjusted: age` = p_bt("Binomial", "Adjusted", "Age"),
                  `Severity, unadjusted` = p_bt("Ordinal", "Unadjusted", "Index"),
                  `Severity, adjusted: index` = p_bt("Ordinal", "Adjusted", "Index"),
                  `Severity, adjusted: age` = p_bt("Ordinal", "Adjusted", "Age"), check.names = FALSE)
cook <- cook[order(match(cook$predictor, orden), cook$model == "Adjusted"), ]
t6c <- data.frame(Predictor = cook$predictor, Model = cook$model, `Influential (n)` = cook$n_influential,
                  `OR, all (p)` = paste0(f2(cook$or_all), " (", f_p(cook$p_all), ")"),
                  `OR, without influential (p)` = paste0(f2(cook$or_without), " (", f_p(cook$p_without), ")"),
                  `Change in OR` = paste0(sprintf("%+.1f", cook$change_pct), "%"), check.names = FALSE)
t6c$Predictor[duplicated(t6c$Predictor)] <- ""
s6 <- c("## S6. Model assumptions", "",
        "**Proportional odds (Brant test)** in the ordinal models; p < 0.05 indicates that the assumption is not met.", "",
        tabla_md(t6b, c("l", rep("r", 3))), "",
        "**Linearity of the logit (Box-Tidwell test).** Likelihood-ratio p for the x\u00b7log(x) term of each index (and of ",
        "age in the adjusted models); p < 0.05 suggests non-linearity.", "",
        tabla_md(t6t, c("l", rep("r", 6))), "",
        paste0("**Influential observations (Cook's distance > 4/n)** in the binomial models; OR per 1 SD with all ",
               "patients and after excluding influential observations (sensitivity analysis)."), "",
        tabla_md(t6c, c("l", "l", rep("r", 4))), "")

# ---- Write ----------------------------------------------------------------------------------
encabezado <- c("# Supplementary material", "",
                paste0("*A Preliminary Analysis of the Discriminative Capacity of the Body Roundness Index, ",
                       "Conventional Anthropometrics, and Diet Quality for Identifying Suicide Risk at First ",
                       "Consultation in Major Depression.* NNDC 2026 Annual Conference."), "",
                paste0("Generated by `code/05_supplementary.R` from the aggregated files in [`results/`](results/). ",
                       "[Back to README](README.md)"), "",
                "S1 Participant flow \u00b7 S2 Characteristics (Table S1) \u00b7 S3 Odds ratios \u00b7 S4 Odds ratios by cut-point \u00b7 ",
                "S5 Discriminative capacity \u00b7 S6 Model assumptions", "")
writeLines(enc2utf8(c(encabezado, s1, s2, s3, s4, s5, s6)), "SUPPLEMENTARY.md", useBytes = TRUE)
message("SUPPLEMENTARY.md updated")
