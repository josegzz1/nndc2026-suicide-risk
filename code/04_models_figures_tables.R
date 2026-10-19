# =============================================================================
#  04_models_figures_tables.R
#
#  Main analysis, figures and tables of the poster:
#   - Binomial (any suicide risk) and ordinal (none / low / high) logistic
#     regression for each index: OR per 1 SD, unadjusted and adjusted for age
#     (per 10 years) and sex; profile-likelihood 95% CI; Holm correction.
#   - Cut-point logistic regressions for predictors that do not meet
#     proportional odds (REAP-S).
#   - Discrimination: AUC (DeLong 95% CI) of each index alone and with age + sex;
#     DeLong tests against the age + sex model and between indices (Holm).
#   - Exports Figure 1 and Tables 1-2 as placed in the poster, Figure S1 (ROC
#     curves) as PNG at 300 dpi and final physical size, and aggregated results
#     as CSV.
#
#  Input  : `tble_final` (from 01_data_preparation.R). Without the data, set
#           DEMO <- TRUE to run on simulated data: outputs then go to
#           demo_output/ and are watermarked "SIMULATED DATA".
#  Output : figures/*.png and results/*.csv
#  Requires R >= 4.2 and the Open Sans font.
# =============================================================================

if (!exists("DEMO")) DEMO <- FALSE   # TRUE = preview with simulated data

# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("ggplot2", "patchwork", "pROC", "MASS", "ragg", "systemfonts")
faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) install.packages(faltan)
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

# Poster font (Open Sans). Install it from fonts.google.com/specimen/Open+Sans
fuente <- "Open Sans"
if (!fuente %in% systemfonts::system_fonts()$family) {
  warning("Font 'Open Sans' not found: figures will use the default ",
          "font. Install it and restart R.")
}

# ---- 1. Data (from code/01_data_preparation.R) ---------------------------------
if (!DEMO) {
  if (!exists("tble_final")) {
    stop("Cannot find `tble_final`. Run code/01_data_preparation.R first (or set DEMO <- TRUE).")
  }
  datos <- tble_final
}

# Ordinal outcome: Risk_Categories (0 = none, 1 = low, 2 = categories 2-4 combined)
var_riesgo_ord   <- "Risk_Categories"
niveles_ord      <- c(0, 1, 2)
etiquetas_riesgo <- c("None", "Low", "High")   # labels, same order

# Binary outcome (1 = risk, 0 = no risk)
var_riesgo_bin <- "Suicide_Risk"

# Adjustment covariates
var_edad     <- "EDAD"   # years
var_sexo     <- "SEXO"   # 1 = female, 2 = male
valor_hombre <- 2        # value of SEXO for men

# Predictors: "name in the poster" = "column in tble_final"
predictores <- c(
  "BMI"                   = "BMI",
  "Waist circumference"   = "Cintura_cm",
  "Waist-to-hip ratio"    = "WHR",
  "Waist-to-height ratio" = "WHtR",
  "Body Roundness Index"  = "BRI",
  "REAP-S"                = "REAPS"
)
dominio  <- c(rep("Anthropometric", 5), "Diet quality")
unidades <- c("kg/m\u00b2", "cm", "", "", "", "points")   # for "1 SD = ..."
# Table 1: label (with units) and decimals of each predictor, same order
etiquetas_t1 <- c("BMI, kg/m\u00b2", "Waist circumference, cm", "Waist-to-hip ratio",
                  "Waist-to-height ratio", "Body Roundness Index", "REAP-S score")
decimales_t1 <- c(1, 1, 2, 2, 1, 1)

# Predictors that did not meet proportional odds (Brant test in
# code/03_model_assumptions.R; see results/assumptions_brant.csv).
# Panel B shows them by cut-point instead of a single ordinal OR.
violan_po <- c("REAP-S")
nota_po   <- "Brant test p = 0.02"   # printed in the footnote of panel B

# Output folders (with simulated data everything goes to demo_output/)
carpeta_fig <- if (DEMO) file.path("demo_output", "figures") else "figures"
carpeta_res <- if (DEMO) file.path("demo_output", "results") else "results"

# ---- Style (matches the poster) ---------------------------------------------
tinta      <- "#0b0b0b"   # main text
tinta2     <- "#52514e"   # secondary text
tenue      <- "#898781"   # reference lines
rejilla    <- "#e1e0d9"   # grid
gris_crudo <- "#a3a19b"   # UNADJUSTED models
azul       <- "#2a78d6"   # anthropometric
naranja    <- "#eb6834"   # diet quality
azul_marca <- "#104281"   # poster dark blue
fondo      <- "#F4F8FD"   # poster panel background: same look in the poster, readable in GitHub dark mode
col_dominio <- c("Anthropometric" = azul, "Diet quality" = naranja, "Covariate" = tinta2)
pt_tam <- function(x) x / ggplot2::.pt
fmt    <- function(v, d = 2) formatC(v, format = "f", digits = d)
fmt_sd <- function(v) trimws(formatC(signif(v, 2), digits = 2, format = "fg"))

theme_poster <- function(base = 24) {
  theme_minimal(base_size = base, base_family = fuente) %+replace%
    theme(
      text             = element_text(colour = tinta, family = fuente),
      axis.text        = element_text(colour = tinta2, size = base),
      axis.title       = element_text(colour = tinta, size = base),
      axis.title.x     = element_text(margin = margin(t = 12), colour = tinta, size = base),
      axis.title.y     = element_text(margin = margin(r = 12), angle = 90, colour = tinta, size = base),
      panel.grid.major = element_line(colour = rejilla, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = base + 4, hjust = 0,
                                      margin = margin(b = 6), colour = tinta),
      plot.subtitle    = element_text(size = base, hjust = 0, colour = tinta2,
                                      margin = margin(b = 14)),
      plot.caption     = element_text(size = base - 3, hjust = 0, colour = tinta2,
                                      margin = margin(t = 12), lineheight = 0.95),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position  = "top",
      legend.justification = "left",
      legend.title     = element_blank(),
      legend.text      = element_text(size = base - 2, colour = tinta2,
                                      margin = margin(r = 24, l = 6)),
      legend.key.width = unit(0.5, "in"),
      strip.text       = element_text(face = "bold", size = base + 2, hjust = 0,
                                      colour = tinta, margin = margin(b = 10)),
      plot.background  = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.margin      = margin(12, 16, 12, 12)
    )
}

# ---- 2. Simulated data (DEMO only) -----------------------------------------
if (DEMO) {
  set.seed(2026)
  n_sim  <- 652
  SEXO   <- sample(c(1, 2), n_sim, replace = TRUE, prob = c(0.68, 0.32))
  hombre <- as.integer(SEXO == 2)
  EDAD   <- pmin(65, round(15 + rgamma(n_sim, shape = 2.2, scale = 6.5)))
  talla  <- rnorm(n_sim, ifelse(hombre == 1, 1.71, 1.60), 0.065)
  BMI    <- pmax(16, 24 + 0.12 * (EDAD - 25) + rnorm(n_sim, 0, 5.8))
  Cintura_cm <- 36 + 1.9 * BMI + 4.5 * hombre + rnorm(n_sim, 0, 6)
  cadera <- 54 + 1.6 * BMI - 2.5 * hombre + rnorm(n_sim, 0, 5)
  REAPS  <- pmin(39, pmax(13, round(rnorm(n_sim, 25.3, 4.7) - 0.6 * hombre)))
  BRI    <- 364.2 - 365.5 * sqrt(pmax(0, 1 - ((Cintura_cm / 100) / (2 * pi))^2 / (0.5 * talla)^2))
  latente <- -0.35 * scale(EDAD)[, 1] - 0.45 * hombre - 0.25 * scale(REAPS)[, 1] + rlogis(n_sim)
  Risk_Categories <- as.integer(cut(latente, quantile(latente, c(0, .584, .825, 1)),
                                    include.lowest = TRUE)) - 1L
  datos <- data.frame(EDAD, SEXO, BMI, Cintura_cm, WHR = Cintura_cm / cadera,
                      WHtR = Cintura_cm / (talla * 100), BRI, REAPS, Risk_Categories,
                      Suicide_Risk = as.integer(Risk_Categories > 0))
}

# ---- 3. Preparation ----------------------------------------------------------
for (k in c(carpeta_fig, carpeta_res)) dir.create(k, recursive = TRUE, showWarnings = FALSE)
v_ord <- datos[[var_riesgo_ord]]
sobran <- setdiff(unique(as.character(v_ord[!is.na(v_ord)])), as.character(niveles_ord))
if (length(sobran)) stop("Values of '", var_riesgo_ord, "' outside niveles_ord: ", paste(sobran, collapse = ", "))
y_ord <- factor(as.character(v_ord), levels = as.character(niveles_ord), ordered = TRUE)
stopifnot(length(etiquetas_riesgo) == nlevels(y_ord))
levels(y_ord) <- etiquetas_riesgo
y_bin <- as.integer(datos[[var_riesgo_bin]])
stopifnot(all(y_bin %in% c(0, 1, NA)))
if (any(y_bin != as.integer(y_ord != levels(y_ord)[1]), na.rm = TRUE)) {
  warning("Some patients have a binary risk different from (ordinal level > '", levels(y_ord)[1], "').")
}
edad   <- as.numeric(datos[[var_edad]])
hombre <- as.integer(datos[[var_sexo]] == valor_hombre)
if (length(unique(stats::na.omit(datos[[var_sexo]]))) != 2) stop("'", var_sexo, "' must have exactly two values.")
K <- nlevels(y_ord)
# Labels of the ordinal cut-points: "Low/High vs None", "High vs None/Low"
etq_corte <- vapply(seq_len(K - 1), function(j) {
  paste(paste(levels(y_ord)[(j + 1):K], collapse = "/"), "vs", paste(levels(y_ord)[1:j], collapse = "/"))
}, character(1))
etq_corte[1] <- paste0("any risk vs ", tolower(levels(y_ord)[1]))

ci_perfil <- function(fit, term) {
  ci <- tryCatch(suppressMessages(confint(fit)), error = function(e) NULL)
  if (is.null(ci)) ci <- confint.default(fit)          # fallback: Wald
  if (is.matrix(ci)) ci[term, ] else ci
}
p_polr <- function(fit, term) 2 * pnorm(-abs(coef(summary(fit))[term, "t value"]))
fila_or <- function(fit, term, tipo) {
  ci <- ci_perfil(fit, term)
  p  <- if (inherits(fit, "polr")) p_polr(fit, term) else coef(summary(fit))[term, "Pr(>|z|)"]
  data.frame(or = exp(coef(fit)[[term]]), lo = exp(ci[[1]]), hi = exp(ci[[2]]), p = p, tipo = tipo)
}
auc_ci <- function(r) { v <- as.numeric(pROC::ci.auc(r, method = "delong")); c(auc = v[2], auc_lo = v[1], auc_hi = v[3]) }
roc_p  <- function(y, p) pROC::roc(y, p, levels = c(0, 1), direction = "<", quiet = TRUE)

# ---- 4. Models: unadjusted, adjusted, by cut-point, AUC ---------------------------
or_bin <- list(); or_ord <- list(); or_cortes <- list(); aucs <- list(); rocs <- list(); dl_ref <- list()

for (i in seq_along(predictores)) {
  nom <- names(predictores)[i]
  x   <- datos[[predictores[i]]]
  ok  <- stats::complete.cases(x, y_bin, y_ord, edad, hombre)
  d   <- data.frame(x = x[ok], yb = y_bin[ok], yo = droplevels(y_ord[ok]),
                    edad10 = edad[ok] / 10, hombre = factor(hombre[ok], levels = c(0, 1)))
  d$z <- (d$x - mean(d$x)) / sd(d$x)                    # OR per 1 SD

  m_b  <- glm(yb ~ z, data = d, family = binomial)
  m_ba <- glm(yb ~ z + edad10 + hombre, data = d, family = binomial)
  m_o  <- MASS::polr(yo ~ z, data = d, Hess = TRUE)
  m_oa <- MASS::polr(yo ~ z + edad10 + hombre, data = d, Hess = TRUE)

  or_bin[[nom]] <- cbind(predictor = nom, rbind(fila_or(m_b, "z", "Unadjusted"), fila_or(m_ba, "z", "Adjusted")))
  or_ord[[nom]] <- cbind(predictor = nom, rbind(fila_or(m_o, "z", "Unadjusted"), fila_or(m_oa, "z", "Adjusted")))

  # Cut-point regressions (plotted only if the predictor violates proportional odds)
  or_cortes[[nom]] <- do.call(rbind, lapply(seq_len(K - 1), function(j) {
    d$yc <- as.integer(as.integer(d$yo) > j)
    cbind(predictor = nom, corte = j,
          rbind(fila_or(glm(yc ~ z, data = d, family = binomial), "z", "Unadjusted"),
                fila_or(glm(yc ~ z + edad10 + hombre, data = d, family = binomial), "z", "Adjusted")))
  }))

  # AUC: index alone and index + age + sex model, from the fitted probabilities
  # (same ROC curve as the raw index, but all with direction "<",
  # as the paired DeLong test requires).
  r_solo <- roc_p(d$yb, fitted(m_b))
  r_adj  <- roc_p(d$yb, fitted(m_ba))
  r_ref_i <- roc_p(d$yb, fitted(glm(yb ~ edad10 + hombre, data = d, family = binomial)))
  dl_ref[[nom]] <- data.frame(predictor = nom,
                              delta_auc = as.numeric(pROC::auc(r_adj)) - as.numeric(pROC::auc(r_ref_i)),
                              p_vs_ref = pROC::roc.test(r_adj, r_ref_i, method = "delong", paired = TRUE)$p.value)
  rocs[[nom]] <- list(roc = r_solo, x = x)
  aucs[[nom]] <- cbind(predictor = nom, n = sum(ok), sd = sd(d$x),
                       rbind(data.frame(t(auc_ci(r_solo)), tipo = "Unadjusted"),
                             data.frame(t(auc_ci(r_adj)),  tipo = "Adjusted")))
}
or_bin <- do.call(rbind, or_bin); or_ord <- do.call(rbind, or_ord)
or_cortes <- do.call(rbind, or_cortes); aucs <- do.call(rbind, aucs); dl_ref <- do.call(rbind, dl_ref)
rownames(or_bin) <- rownames(or_ord) <- rownames(or_cortes) <- rownames(aucs) <- NULL

# Reference block: age (per 10 years) and sex (male vs female), alone and mutually adjusted
ok_ref <- stats::complete.cases(y_bin, y_ord, edad, hombre)
d_ref  <- data.frame(yb = y_bin[ok_ref], yo = droplevels(y_ord[ok_ref]), edad10 = edad[ok_ref] / 10,
                     hombre = factor(hombre[ok_ref], levels = c(0, 1)))
m_ref_b <- glm(yb ~ edad10 + hombre, data = d_ref, family = binomial)
m_ref_o <- MASS::polr(yo ~ edad10 + hombre, data = d_ref, Hess = TRUE)
or_ref_bin <- rbind(
  cbind(predictor = "Age", rbind(fila_or(glm(yb ~ edad10, data = d_ref, family = binomial), "edad10", "Unadjusted"),
                                 fila_or(m_ref_b, "edad10", "Adjusted"))),
  cbind(predictor = "Male sex", rbind(fila_or(glm(yb ~ hombre, data = d_ref, family = binomial), "hombre1", "Unadjusted"),
                                      fila_or(m_ref_b, "hombre1", "Adjusted"))))
or_ref_ord <- rbind(
  cbind(predictor = "Age", rbind(fila_or(MASS::polr(yo ~ edad10, data = d_ref, Hess = TRUE), "edad10", "Unadjusted"),
                                 fila_or(m_ref_o, "edad10", "Adjusted"))),
  cbind(predictor = "Male sex", rbind(fila_or(MASS::polr(yo ~ hombre, data = d_ref, Hess = TRUE), "hombre1", "Unadjusted"),
                                      fila_or(m_ref_o, "hombre1", "Adjusted"))))
r_edad <- roc_p(d_ref$yb, fitted(glm(yb ~ edad10, data = d_ref, family = binomial)))
r_sexo <- roc_p(d_ref$yb, fitted(glm(yb ~ hombre, data = d_ref, family = binomial)))
r_ref  <- roc_p(d_ref$yb, fitted(m_ref_b))
auc_ref <- auc_ci(r_ref)
aucs_ref <- rbind(data.frame(predictor = "Age", t(auc_ci(r_edad)), tipo = "Unadjusted"),
                  data.frame(predictor = "Male sex", t(auc_ci(r_sexo)), tipo = "Unadjusted"))

# Holm across the 6 indices, within each model
holm <- function(df) { df$p_holm <- ave(df$p, df$tipo, FUN = function(p) p.adjust(p, "holm")); df }
or_bin <- holm(or_bin); or_ord <- holm(or_ord)

# Paired DeLong tests between the 6 indices alone (15 pairs)
pares <- combn(names(predictores), 2, simplify = FALSE)
delong <- do.call(rbind, lapply(pares, function(pp) {
  xa <- rocs[[pp[1]]]$x; xb <- rocs[[pp[2]]]$x
  ok <- stats::complete.cases(xa, xb, y_bin)
  ra <- roc_p(y_bin[ok], fitted(glm(y_bin[ok] ~ xa[ok], family = binomial)))
  rb <- roc_p(y_bin[ok], fitted(glm(y_bin[ok] ~ xb[ok], family = binomial)))
  data.frame(par = paste(pp, collapse = " vs "),
             p = pROC::roc.test(ra, rb, method = "delong", paired = TRUE)$p.value)
}))
delong$p_holm <- p.adjust(delong$p, "holm")

# Results table for the poster text
resultados <- merge(
  merge(or_bin, or_ord, by = c("predictor", "tipo"), suffixes = c("_bin", "_ord")),
  aucs, by = c("predictor", "tipo"))
resultados <- resultados[order(match(resultados$predictor, names(predictores)), resultados$tipo == "Adjusted"), ]
a_csv <- function(df, archivo) {   # English column names: tipo -> model, corte -> cutpoint, par -> pair
  names(df)[names(df) == "tipo"]  <- "model"
  names(df)[names(df) == "corte"] <- "cutpoint"
  names(df)[names(df) == "par"]   <- "pair"
  utils::write.csv(df, file.path(carpeta_res, archivo), row.names = FALSE)
}
a_csv(resultados, "model_results.csv")
a_csv(or_cortes, "or_by_cutpoint.csv")
a_csv(delong, "delong_pairwise.csv")
a_csv(dl_ref, "delong_vs_age_sex.csv")
a_csv(rbind(cbind(outcome = "Binomial", or_ref_bin), cbind(outcome = "Ordinal", or_ref_ord)), "or_age_sex.csv")
a_csv(rbind(aucs_ref, data.frame(predictor = "Age + sex", t(auc_ref), tipo = "Combined")), "auc_age_sex.csv")

# ---- 5. Rows of Figure 1 ----------------------------------------------------
# Order (top -> bottom): ANTHROPOMETRIC, 5 indices; DIET QUALITY, REAP-S (+ its
# cut-points 2..K-1 if it violates proportional odds); COVARIATES, age and sex.
alto <- c(header = 0.7, main = 1, sub = 0.72, gap = 0.3)
items <- list()
agregar <- function(tipo, id = "", etiqueta = "", dominio = "") {
  items[[length(items) + 1]] <<- data.frame(tipo, id, etiqueta, dominio, alto = alto[[tipo]])
}
for (g in c("Anthropometric", "Diet quality")) {
  agregar("header", etiqueta = toupper(g), dominio = g)
  for (nom in names(predictores)[dominio == g]) {
    agregar("main", nom, nom, g)
    if (nom %in% violan_po && K > 2) {
      for (j in 2:(K - 1)) agregar("sub", paste0(nom, "|corte", j), paste0(etq_corte[j], "\u2020"), g)
    }
  }
  agregar("gap")
}
agregar("header", etiqueta = "COVARIATES", dominio = "Covariate")
agregar("main", "Age", "Age, per 10 years", "Covariate")
agregar("main", "Male sex", "Male sex (vs female)", "Covariate")
filas <- do.call(rbind, items)
# Size of 1 SD of each index (for the figure footnote)
sd_idx <- aucs$sd[match(names(predictores), aucs$predictor)]
abrev  <- c("BMI", "WC", "WHR", "WHtR", "BRI", "REAP-S")   # same order as `predictores`
nota_sd <- paste0("1 SD: ", paste0(abrev, " ", fmt_sd(sd_idx),
                                   ifelse(unidades == "", "", paste0(" ", unidades)), collapse = " \u00b7 "), ".")
filas$y <- sum(filas$alto) - (cumsum(filas$alto) - filas$alto / 2) + 0.55   # 0.55 = bottom space (arrow)
lim_y <- c(0, sum(filas$alto) + 0.55)
eje_y <- scale_y_continuous(limits = lim_y, expand = c(0, 0))
y_de  <- function(id) filas$y[match(id, filas$id)]
y_hdr_top <- filas$y[1]

# ---- 6. Figure 1 -------------------------------------------------------------
# Label column
p_lab <- ggplot() +
  geom_point(data = subset(filas, tipo == "header"), aes(x = 0.02, y = y, colour = dominio), size = 5) +
  geom_text(data = subset(filas, tipo == "header"), aes(x = 0.07, y = y, label = etiqueta), hjust = 0,
            family = fuente, fontface = "bold", size = pt_tam(19), colour = tinta2) +
  geom_text(data = subset(filas, tipo == "main"), aes(x = 0.07, y = y, label = etiqueta), hjust = 0,
            family = fuente, size = pt_tam(25), colour = tinta) +
  geom_text(data = subset(filas, tipo == "sub"), aes(x = 0.13, y = y, label = etiqueta), hjust = 0,
            family = fuente, size = pt_tam(20), colour = tinta2) +
  scale_colour_manual(values = col_dominio, guide = "none") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + eje_y +
  labs(title = " ", subtitle = " ") +
  theme_void(base_family = fuente) +
  theme(plot.title = element_text(size = 28, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 24, margin = margin(b = 14)),
        plot.margin = margin(12, 0, 12, 0))

# Forest plot data: gray = unadjusted (upper), color = adjusted (lower)
preparar_forest <- function(or_idx, or_ref, cortes = NULL) {
  f <- rbind(or_idx[, c("predictor", "tipo", "or", "lo", "hi", "p")],
             or_ref[, c("predictor", "tipo", "or", "lo", "hi", "p")])
  f$id <- f$predictor
  if (!is.null(cortes)) {
    # cut-point 1 (any risk vs none) goes on the main row; the others on sub-rows
    f <- f[!f$predictor %in% violan_po, ]
    cc <- cortes[cortes$predictor %in% violan_po, ]
    cc$id <- ifelse(cc$corte == 1, cc$predictor, paste0(cc$predictor, "|corte", cc$corte))
    f <- rbind(f, cc[, c("predictor", "tipo", "or", "lo", "hi", "p", "id")])
  }
  f$dominio <- ifelse(f$predictor %in% names(predictores),
                      dominio[match(f$predictor, names(predictores))], "Covariate")
  f$tipo <- factor(f$tipo, levels = c("Unadjusted", "Adjusted"))
  f$y   <- y_de(f$id) + ifelse(f$tipo == "Unadjusted", 0.19, -0.19)
  f$col <- ifelse(f$tipo == "Unadjusted", gris_crudo, col_dominio[f$dominio])
  f$relleno <- ifelse(f$tipo == "Unadjusted", "white", col_dominio[f$dominio])
  f
}
fA <- preparar_forest(or_bin, or_ref_bin)
fB <- preparar_forest(or_ord, or_ref_ord, or_cortes)
lim_or <- range(c(fA$lo, fA$hi, fB$lo, fB$hi)); lim_or <- c(lim_or[1] / 1.06, max(1.06, lim_or[2] * 1.06))
cortes_or <- c(0.3, 0.4, 0.5, 0.6, 0.8, 1, 1.2, 1.5, 2)
cortes_or <- cortes_or[cortes_or >= lim_or[1] & cortes_or <= lim_or[2]]
leyenda_modelo <- c("Unadjusted", "Adjusted for age and sex")

forest <- function(f, titulo, subtitulo) {
  ggplot(f, aes(y = y)) +
    geom_vline(xintercept = 1, colour = tinta, linewidth = 0.7) +
    geom_segment(aes(x = lo, xend = hi, yend = y, colour = col), linewidth = 1.3, lineend = "round") +
    geom_point(aes(x = or, colour = col, fill = relleno, shape = tipo), size = 5.2, stroke = 1.5) +
    annotate("segment", x = 0.985, xend = 0.985 / 1.14, y = 0.3, yend = 0.3, colour = tinta2,
             linewidth = 0.8, arrow = arrow(length = unit(0.13, "in"), type = "closed")) +
    annotate("text", x = 0.985 / 1.17, y = 0.3, label = "lower odds of suicide risk", hjust = 1,
             family = fuente, size = pt_tam(18), colour = tinta2) +
    scale_shape_manual(values = c(21, 21), labels = leyenda_modelo, drop = FALSE) +
    scale_colour_identity() + scale_fill_identity() +
    scale_x_log10(limits = lim_or, breaks = cortes_or, labels = fmt(cortes_or, 1)) + eje_y +
    guides(shape = guide_legend(override.aes = list(colour = c(gris_crudo, tinta2),
                                                    fill = c("white", tinta2), size = 5.2, stroke = 1.5))) +
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = NULL) +
    theme_poster() +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
}
n_sig <- function(f) { a <- f[f$predictor %in% names(predictores), ]
  tapply(a$p_holm < 0.05, a$tipo, sum) }
sigA <- n_sig(or_bin |> transform(tipo = factor(tipo, levels = c("Unadjusted", "Adjusted"))))
sigO <- n_sig(or_ord |> transform(tipo = factor(tipo, levels = c("Unadjusted", "Adjusted"))))

p_A <- forest(fA, "A   Any suicide risk", "Binomial OR per 1 SD (95% CI)")
p_B <- forest(fB, "B   Suicide risk severity", "Ordinal OR per 1 SD (95% CI)") +
  theme(legend.position = "none")
# Indices with Holm p < 0.05 (for the footnote)
sig_adj_A <- or_bin$predictor[or_bin$tipo == "Adjusted" & or_bin$p_holm < 0.05]
antropo   <- names(predictores)[dominio == "Anthropometric"]
sigB_ant  <- tapply(or_ord$p_holm[or_ord$predictor %in% antropo] < 0.05,
                    factor(or_ord$tipo[or_ord$predictor %in% antropo], levels = c("Unadjusted", "Adjusted")), sum)

# Panel C: AUC alone (gray) vs + age and sex (color); dashed line = age + sex
fC <- rbind(aucs[, c("predictor", "tipo", "auc", "auc_lo", "auc_hi")], aucs_ref)
fC$dominio <- ifelse(fC$predictor %in% names(predictores),
                     dominio[match(fC$predictor, names(predictores))], "Covariate")
fC$tipo <- factor(fC$tipo, levels = c("Unadjusted", "Adjusted"))
fC$y    <- y_de(fC$predictor) + ifelse(fC$tipo == "Unadjusted", 0.19, -0.19)
fC$col  <- ifelse(fC$tipo == "Unadjusted", gris_crudo, col_dominio[fC$dominio])
fC$relleno <- ifelse(fC$tipo == "Unadjusted", "white", col_dominio[fC$dominio])
delta_max <- max(abs(dl_ref$delta_auc)); p_ref_min <- min(dl_ref$p_vs_ref)
lim_auc <- c(0.45, max(0.85, 0.05 + ceiling(max(c(fC$auc_hi, auc_ref[["auc_hi"]])) * 10) / 10))

p_C <- ggplot(fC, aes(y = y)) +
  annotate("rect", xmin = 0.7, xmax = lim_auc[2], ymin = lim_y[1], ymax = y_hdr_top - 0.3,
           fill = "#cde2fb", alpha = 0.6) +
  annotate("text", x = 0.708, y = y_hdr_top - 0.38, label = "Acceptable\n(AUC \u2265 0.70)", hjust = 0,
           vjust = 1, lineheight = 0.9, family = fuente, size = pt_tam(17), colour = tinta2) +
  geom_vline(xintercept = 0.5, colour = tinta, linewidth = 0.7) +
  annotate("text", x = 0.507, y = 0.3, label = "chance", hjust = 0, family = fuente,
           size = pt_tam(18), colour = tinta2) +
  geom_vline(xintercept = auc_ref[["auc"]], colour = tinta2, linewidth = 0.9, linetype = "22") +
  annotate("text", x = auc_ref[["auc"]] + 0.006, y = 0.3, hjust = 0,
           label = paste0("age + sex: ", fmt(auc_ref[["auc"]])),
           family = fuente, size = pt_tam(18), colour = tinta2) +
  geom_segment(aes(x = auc_lo, xend = auc_hi, yend = y, colour = col), linewidth = 1.3, lineend = "round") +
  geom_point(aes(x = auc, colour = col, fill = relleno, shape = tipo), size = 5.2, stroke = 1.5) +
  scale_shape_manual(values = c(21, 21), labels = c("Index alone", "Index + age + sex"), drop = FALSE) +
  scale_colour_identity() + scale_fill_identity() +
  scale_x_continuous(limits = lim_auc, breaks = seq(0.5, 1, 0.1), labels = fmt(seq(0.5, 1, 0.1), 1),
                     expand = c(0, 0)) + eje_y +
  guides(shape = guide_legend(override.aes = list(colour = c(gris_crudo, tinta2),
                                                  fill = c("white", tinta2), size = 5.2, stroke = 1.5))) +
  labs(title = "C   Discriminative capacity", subtitle = "Area under the ROC curve (95% CI)", x = NULL, y = NULL) +
  theme_poster() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())

# AUC value column: gray = alone, color = + age and sex
vC <- fC; vC$x <- ifelse(vC$tipo == "Unadjusted", 0.03, 0.52); vC$y <- y_de(vC$predictor)
vC$txt <- ifelse(vC$tipo == "Unadjusted", gris_crudo, ifelse(vC$dominio == "Covariate", tinta, col_dominio[vC$dominio]))
p_val <- ggplot(vC) +
  geom_text(aes(x = x, y = y, label = fmt(auc), colour = txt), hjust = 0, family = fuente,
            fontface = "bold", size = pt_tam(23)) +
  # "AUC" header over the "Alone" and "Adjusted" columns (drawn above the panel: clip = "off")
  annotate("text", x = 0.5, y = y_hdr_top + 0.72, label = "AUC", hjust = 0.5,
           family = fuente, fontface = "bold", size = pt_tam(20), colour = tinta) +
  annotate("segment", x = 0.03, xend = 0.97, y = y_hdr_top + 0.33, yend = y_hdr_top + 0.33,
           colour = tinta2, linewidth = 0.6) +
  annotate("text", x = c(0.03, 0.52), y = y_hdr_top, label = c("Alone", "Adjusted"), hjust = 0,
           family = fuente, fontface = "bold", size = pt_tam(17), colour = tinta2) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_cartesian(ylim = lim_y, expand = FALSE, clip = "off") +
  labs(title = " ", subtitle = " ") +
  theme_void(base_family = fuente) +
  theme(plot.title = element_text(size = 28, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 24, margin = margin(b = 14)),
        plot.margin = margin(12, 0, 12, 6))

# Shared footnote (4 short lines to fit 22.5 in)
nota_fig1 <- paste0(
  "Holm-adjusted p < 0.05 \u2014 A: ", sigA[["Unadjusted"]], "/6 indices unadjusted, ",
  sigA[["Adjusted"]], "/6 adjusted", if (length(sig_adj_A)) paste0(" (", paste(sig_adj_A, collapse = ", "), ")"),
  "; B: ", sigB_ant[["Unadjusted"]], "/", length(antropo), " anthropometric indices unadjusted, ",
  sigB_ant[["Adjusted"]], "/", length(antropo), " adjusted. Age and sex are mutually adjusted.\n",
  "\u2020", paste(violan_po, collapse = ", "), ": proportional odds not met (", nota_po,
  "), so ORs are shown per cut-point; the first cut (", etq_corte[1], ") is on the main row.\n",
  "C: adding an index to age + sex changed the AUC by \u2264 ", fmt(delta_max), " (DeLong vs. age + sex, all p ",
  if (p_ref_min >= 0.05) "> 0.05" else paste0("\u2265 ", fmt(p_ref_min, 3)), ").\n",
  nota_sd)
fig1 <- p_lab + p_A + p_B + p_C + p_val + plot_layout(widths = c(4.3, 5.4, 5.4, 5.1, 2.3)) +
  plot_annotation(caption = nota_fig1,
                  theme = theme(plot.caption = element_text(size = 18, hjust = 0, colour = tinta2,
                                                            family = fuente, lineheight = 1.05,
                                                            margin = margin(t = 2, l = 6)),
                                plot.background = element_rect(fill = "transparent", colour = NA)))

# ---- 7. Figure S1 - ROC curves (+ age + sex model) ----------------------------
roc_df <- do.call(rbind, lapply(names(rocs), function(nom) {
  r <- rocs[[nom]]$roc
  data.frame(fpr = 1 - r$specificities, tpr = r$sensitivities, predictor = nom,
             grupo = dominio[match(nom, names(predictores))])
}))
roc_df <- rbind(roc_df, data.frame(fpr = 1 - r_ref$specificities, tpr = r_ref$sensitivities,
                                   predictor = "Age + sex", grupo = "Age + sex"))
roc_df$grupo <- factor(roc_df$grupo, levels = c("Anthropometric", "Diet quality", "Age + sex"))
roc_df$predictor <- factor(roc_df$predictor, levels = c(names(predictores), "Age + sex"))
rango_auc <- paste0(fmt(min(aucs$auc[aucs$tipo == "Unadjusted"])), "\u2013",
                    fmt(max(aucs$auc[aucs$tipo == "Unadjusted"])))
d_ref80 <- sqrt(2) * qnorm(0.80)
ref80 <- data.frame(fpr = seq(0, 1, length.out = 300)); ref80$tpr <- pnorm(qnorm(ref80$fpr) + d_ref80)
col_roc <- c("Anthropometric" = azul, "Diet quality" = naranja, "Age + sex" = tinta)
lab_roc <- c("Anthropometric" = "Anthropometric index (5)", "Diet quality" = "REAP-S",
             "Age + sex" = paste0("Age + sex model (AUC ", fmt(auc_ref[["auc"]]), ")"))
leyenda_dentro <- function(x, y) {
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    theme(legend.position = "inside", legend.position.inside = c(x, y), legend.justification = c(1, 0))
  } else theme(legend.position = c(x, y), legend.justification = c(1, 0))
}
fig2 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, colour = tenue, linewidth = 0.8, linetype = "22") +
  geom_line(data = ref80, aes(fpr, tpr), colour = "#cfcdc6", linewidth = 2.2) +
  geom_path(data = roc_df, aes(fpr, tpr, group = predictor, colour = grupo, linewidth = grupo,
                               linetype = grupo), alpha = 0.9, lineend = "round") +
  annotate("text", x = 0.665, y = 0.575, label = "Chance (AUC 0.50)", angle = 45,
           family = fuente, size = pt_tam(20), colour = tinta2) +
  annotate("text", x = 0.03, y = 0.955, label = "Benchmark: AUC 0.80", hjust = 0,
           family = fuente, size = pt_tam(20), colour = tinta2) +
  scale_colour_manual(values = col_roc, labels = lab_roc) +
  scale_linewidth_manual(values = c("Anthropometric" = 1.0, "Diet quality" = 1.6, "Age + sex" = 1.6), guide = "none") +
  scale_linetype_manual(values = c("Anthropometric" = "solid", "Diet quality" = "solid", "Age + sex" = "42"), guide = "none") +
  scale_x_continuous(breaks = seq(0, 1, 0.25), labels = c("0", "0.25", "0.50", "0.75", "1"), expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0", "0.25", "0.50", "0.75", "1"), expand = c(0.01, 0.01)) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(linewidth = 2.2, linetype = c("solid", "solid", "42")))) +
  labs(x = "1 \u2212 Specificity", y = "Sensitivity",
       caption = paste0("Single indices: AUC ", rango_auc, ". Age + sex alone: ", fmt(auc_ref[["auc"]]),
                        ".\nGray: theoretical benchmark (binormal, AUC 0.80).")) +
  theme_poster() +
  leyenda_dentro(0.99, 0.02) +
  theme(legend.direction = "vertical", legend.key.height = unit(0.36, "in"),
        legend.background = element_rect(fill = "transparent", colour = NA))

# ---- 8. Poster Tables 1 and 2 --------------------------------------------------
# Each table is exported at 10.75 x 9.25 in, the size of its slot in the poster.
TW <- 10.75; TH <- 9.25
regla <- "#c9d6e6"                       # thin lines between rows
fmt_p <- function(p) {
  ifelse(p < 0.001, "<0.001",
         ifelse(p < 0.1, formatC(p, format = "f", digits = 3),
                ifelse(p < 0.2, formatC(p, format = "f", digits = 2),
                       ifelse(p > 0.9, ">0.9", formatC(p, format = "f", digits = 1)))))
}
lienzo_tabla <- function(p) {
  p + scale_x_continuous(limits = c(0, TW), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, TH), expand = c(0, 0)) +
    scale_colour_identity() +
    theme_void(base_family = fuente) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = margin(0, 0, 0, 0))
}

# --- Table 1: characteristics, no risk vs suicide risk ---------------
ok1   <- !is.na(y_bin)
grupo <- y_bin[ok1]
n_tot <- length(grupo); n_no <- sum(grupo == 0); n_si <- sum(grupo == 1)
media_de <- function(x, d) paste0(fmt(mean(x, na.rm = TRUE), d), " (", fmt(sd(x, na.rm = TRUE), d), ")")
mediana_ric <- function(x) {
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), type = 2, na.rm = TRUE)   # type 2 = as in gtsummary
  paste0(fmt(q[[2]], 0), " (", fmt(q[[1]], 0), "\u2013", fmt(q[[3]], 0), ")")
}
fila_t1 <- function(etiqueta, x, tipo, d = 1, dom = "Covariate") {
  x <- x[ok1]
  if (tipo == "mediana") {
    celdas <- c(mediana_ric(x), mediana_ric(x[grupo == 0]), mediana_ric(x[grupo == 1]))
    p <- suppressWarnings(stats::wilcox.test(x ~ grupo)$p.value)
  } else if (tipo == "n_pct") {
    n_pct <- function(v) paste0(sum(v, na.rm = TRUE), " (", fmt(100 * mean(v, na.rm = TRUE), 1), ")")
    celdas <- c(n_pct(x), n_pct(x[grupo == 0]), n_pct(x[grupo == 1]))
    p <- suppressWarnings(stats::chisq.test(table(x, grupo), correct = FALSE)$p.value)
  } else {
    celdas <- c(media_de(x, d), media_de(x[grupo == 0], d), media_de(x[grupo == 1], d))
    p <- stats::t.test(x ~ grupo)$p.value                     # Welch
  }
  data.frame(lab = etiqueta, dom = dom, all = celdas[1], no = celdas[2], si = celdas[3],
             p = fmt_p(p), sig = p < 0.05)
}
t1 <- rbind(
  fila_t1("Age, years\u2021", edad, "mediana"),
  fila_t1("Female sex, n (%)", as.integer(hombre == 0), "n_pct"),
  do.call(rbind, lapply(seq_along(predictores), function(i) {
    fila_t1(etiquetas_t1[i], datos[[predictores[i]]], "media", decimales_t1[i], dominio[i])
  }))
)
n1 <- nrow(t1)
top1 <- TH - 0.1; hdr1 <- 1.1; rh1 <- 0.855   # same top/bottom as Table 2 (aligned in the poster)
t1$y <- top1 - hdr1 - rh1 * (seq_len(n1) - 0.5)
t1$col_dot <- col_dominio[t1$dom]
xc1 <- c(all = 5.12, no = 6.87, si = 8.62); xp1 <- 10.72
base1 <- top1 - hdr1 - rh1 * n1

tab1 <- ggplot() +
  annotate("segment", x = 0, xend = TW, y = c(top1, base1), yend = c(top1, base1), colour = azul_marca, linewidth = 1.6) +
  annotate("segment", x = 0, xend = TW, y = top1 - hdr1, yend = top1 - hdr1, colour = azul_marca, linewidth = 0.9) +
  annotate("segment", x = 0, xend = TW, y = top1 - hdr1 - rh1 * seq_len(n1 - 1),
           yend = top1 - hdr1 - rh1 * seq_len(n1 - 1), colour = regla, linewidth = 0.5) +
  annotate("text", x = 0.05, y = top1 - hdr1 / 2, label = "Characteristic", hjust = 0, family = fuente,
           fontface = "bold", size = pt_tam(23), colour = azul_marca) +
  annotate("text", x = xc1, y = top1 - 0.36, label = c("Overall", "No risk", "Suicide risk"), family = fuente,
           fontface = "bold", size = pt_tam(23), colour = azul_marca) +
  annotate("text", x = xc1, y = top1 - 0.78, label = paste("n =", c(n_tot, n_no, n_si)), family = fuente,
           size = pt_tam(20), colour = tinta2) +
  annotate("text", x = xp1, y = top1 - hdr1 / 2, label = "p", hjust = 1, family = fuente,
           fontface = "bold.italic", size = pt_tam(23), colour = azul_marca) +
  geom_point(data = t1, aes(x = 0.14, y = y, colour = col_dot), size = 3.2) +
  geom_text(data = t1, aes(x = 0.36, y = y, label = lab), hjust = 0, family = fuente, size = pt_tam(23), colour = tinta) +
  geom_text(data = t1, aes(x = xc1[["all"]], y = y, label = all), family = fuente, size = pt_tam(23), colour = tinta) +
  geom_text(data = t1, aes(x = xc1[["no"]], y = y, label = no), family = fuente, size = pt_tam(23), colour = tinta) +
  geom_text(data = t1, aes(x = xc1[["si"]], y = y, label = si), family = fuente, size = pt_tam(23), colour = tinta) +
  geom_text(data = t1, aes(x = xp1, y = y, label = p, fontface = ifelse(sig, "bold", "plain")), hjust = 1,
            family = fuente, size = pt_tam(23), colour = tinta) +
  annotate("text", x = 0, y = base1 - 0.2, vjust = 1, hjust = 0, lineheight = 1.05, family = fuente,
           size = pt_tam(18), colour = tinta2,
           label = paste0("Mean (SD) unless stated; \u2021median (IQR). Bold: p < 0.05. Welch t-test;\n",
                          "Wilcoxon rank-sum test for age; Pearson \u03c7\u00b2 test for sex."))
tab1 <- lienzo_tabla(tab1)

# --- Table 2: OR (95% CI) unadjusted vs adjusted for age and sex -------------
celda_or <- function(f) list(or = fmt(f$or), ci = paste0("(", fmt(f$lo), "\u2013", fmt(f$hi), ")"),
                             sig = f$hi < 1 | f$lo > 1)
toma <- function(df, nom, tp, corte = NULL) {
  s <- df[df$predictor == nom & df$tipo == tp, ]
  if (!is.null(corte)) s <- s[s$corte == corte, ]
  s[1, ]
}
filas2 <- list()
for (i in seq_along(predictores)) {
  nom <- names(predictores)[i]
  ord_u <- if (nom %in% violan_po) toma(or_cortes, nom, "Unadjusted", 1) else toma(or_ord, nom, "Unadjusted")
  ord_a <- if (nom %in% violan_po) toma(or_cortes, nom, "Adjusted", 1) else toma(or_ord, nom, "Adjusted")
  filas2[[length(filas2) + 1]] <- list(lab = nom, tipo = "main", dom = dominio[i],
    bu = celda_or(toma(or_bin, nom, "Unadjusted")), ba = celda_or(toma(or_bin, nom, "Adjusted")),
    ou = celda_or(ord_u), oa = celda_or(ord_a))
  if (nom %in% violan_po && K > 2) {
    for (j in 2:(K - 1)) {
      filas2[[length(filas2) + 1]] <- list(lab = paste0(etq_corte[j], "\u2020"), tipo = "sub", dom = dominio[i],
        bu = NULL, ba = NULL, ou = celda_or(toma(or_cortes, nom, "Unadjusted", j)),
        oa = celda_or(toma(or_cortes, nom, "Adjusted", j)))
    }
  }
}
for (cv in c("Age", "Male sex")) {
  filas2[[length(filas2) + 1]] <- list(lab = if (cv == "Age") "Age, per 10 years" else "Male sex (vs female)",
    tipo = "cov", dom = "Covariate",
    bu = celda_or(toma(or_ref_bin, cv, "Unadjusted")), ba = celda_or(toma(or_ref_bin, cv, "Adjusted")),
    ou = celda_or(toma(or_ref_ord, cv, "Unadjusted")), oa = celda_or(toma(or_ref_ord, cv, "Adjusted")))
}
alto2 <- vapply(filas2, function(f) c(main = 0.76, sub = 0.62, cov = 0.76)[[f$tipo]], numeric(1))
hueco_cov <- 0.14                                         # space before the age and sex block
es_cov <- vapply(filas2, function(f) f$tipo == "cov", logical(1))
top2 <- TH - 0.1; hdr2 <- 1.1
arriba2 <- top2 - hdr2 - c(0, cumsum(alto2)[-length(alto2)]) - ifelse(es_cov, hueco_cov, 0)
medio2  <- arriba2 - alto2 / 2
base2   <- arriba2[length(arriba2)] - alto2[length(alto2)]
xs2 <- c(bu = 4.8, ba = 6.5, ou = 8.25, oa = 9.95)

etq2 <- data.frame(lab = vapply(filas2, `[[`, "", "lab"), tipo = vapply(filas2, `[[`, "", "tipo"),
                   y = medio2, col_dot = col_dominio[vapply(filas2, `[[`, "", "dom")])
celdas2 <- do.call(rbind, lapply(seq_along(filas2), function(r) {
  do.call(rbind, lapply(names(xs2), function(k) {
    cc <- filas2[[r]][[k]]
    if (is.null(cc)) return(NULL)
    data.frame(x = xs2[[k]], y = medio2[r], or = cc$or, ci = cc$ci, sig = cc$sig)
  }))
}))
tipos2  <- etq2$tipo
# thin separators: before each index and between age and sex (not before a sub-row or the covariate block)
sep2    <- which(seq_along(tipos2) > 1 & (tipos2 == "main" | (tipos2 == "cov" & c("", head(tipos2, -1)) == "cov")))
lineas2 <- arriba2[sep2]
x_punto <- xs2 - c(0.72, 0.62, 0.72, 0.62)

tab2 <- ggplot() +
  annotate("segment", x = 0, xend = TW, y = c(top2, base2), yend = c(top2, base2), colour = azul_marca, linewidth = 1.6) +
  annotate("segment", x = c(3.95, 7.45), xend = c(7.3, TW), y = top2 - 0.52, yend = top2 - 0.52,
           colour = azul_marca, linewidth = 0.7) +
  annotate("segment", x = 0, xend = TW, y = top2 - hdr2, yend = top2 - hdr2, colour = azul_marca, linewidth = 0.9) +
  annotate("segment", x = 0, xend = TW, y = lineas2, yend = lineas2, colour = regla, linewidth = 0.5) +
  annotate("segment", x = 0, xend = TW, y = arriba2[which(es_cov)[1]] + hueco_cov / 2,
           yend = arriba2[which(es_cov)[1]] + hueco_cov / 2, colour = azul_marca, linewidth = 0.7) +
  annotate("text", x = c(mean(xs2[1:2]), mean(xs2[3:4])), y = top2 - 0.27,
           label = c("Any suicide risk", "Risk severity"), family = fuente, fontface = "bold",
           size = pt_tam(22), colour = azul_marca) +
  annotate("point", x = x_punto, y = top2 - 0.83, shape = 21, size = 3.2, stroke = 1.2,
           colour = c(gris_crudo, tinta2, gris_crudo, tinta2), fill = c("white", tinta2, "white", tinta2)) +
  annotate("text", x = x_punto + 0.14, y = top2 - 0.83, hjust = 0,
           label = c("Unadjusted", "Adjusted*", "Unadjusted", "Adjusted*"), family = fuente,
           size = pt_tam(18), colour = tinta2) +
  annotate("text", x = 0.05, y = top2 - 0.83, hjust = 0, label = "OR (95% CI)", family = fuente,
           fontface = "bold", size = pt_tam(19), colour = azul_marca) +
  geom_point(data = subset(etq2, tipo != "sub"), aes(x = 0.14, y = y, colour = col_dot), size = 3.2) +
  geom_text(data = subset(etq2, tipo != "sub"), aes(x = 0.36, y = y, label = lab), hjust = 0, family = fuente,
            size = pt_tam(22), colour = tinta) +
  geom_text(data = subset(etq2, tipo == "sub"), aes(x = 0.62, y = y, label = lab), hjust = 0, family = fuente,
            size = pt_tam(18), colour = tinta2) +
  geom_text(data = celdas2, aes(x = x, y = y + 0.14, label = or, fontface = ifelse(sig, "bold", "plain")),
            family = fuente, size = pt_tam(23), colour = tinta) +
  geom_text(data = celdas2, aes(x = x, y = y - 0.19, label = ci), family = fuente, size = pt_tam(17), colour = tinta2) +
  annotate("text", x = 0, y = base2 - 0.18, vjust = 1, hjust = 0, lineheight = 1.05, family = fuente,
           size = pt_tam(17), colour = tinta2,
           label = paste0("Indices: OR per 1-SD increase; age: per 10 years; sex: men vs women.\n",
                          "*Indices adjusted for age and sex; age and sex mutually adjusted. Bold: 95% CI excludes 1.",
                          if (length(violan_po)) paste0("\n\u2020", paste(violan_po, collapse = ", "),
                            " did not meet proportional odds (", nota_po, "): severity ORs by cut-point.") else ""))
tab2 <- lienzo_tabla(tab2)

# ---- 9. Export ----------------------------------------------------------------
guardar <- function(p, archivo, ancho, alto) {
  ragg::agg_png(file.path(carpeta_fig, archivo), width = ancho, height = alto,
                units = "in", res = 300, background = fondo)
  print(p)
  if (DEMO) grid::grid.text("SIMULATED DATA - DO NOT USE", rot = 18,
                            gp = grid::gpar(col = "#d03b3b", alpha = 0.3, fontsize = 80,
                                            fontface = "bold", fontfamily = fuente))
  invisible(grDevices::dev.off())
}
guardar(fig1, "fig1_association_vs_discrimination.png", 22.5, 9.5)
guardar(tab1, "table1_characteristics.png", TW, TH)
guardar(tab2, "table2_unadjusted_vs_adjusted_or.png", TW, TH)
guardar(fig2, "figS1_roc_curves.png", 10.75, 9.25)

# ---- 10. Numbers for the poster text ---------------------------------------
cat("\n=== Numbers for the poster ===\n")
cat("AUC of single indices:", rango_auc, "| age + sex:", fmt(auc_ref[["auc"]]),
    " (", fmt(auc_ref[["auc_lo"]]), "\u2013", fmt(auc_ref[["auc_hi"]]), ")\n")
cat("Largest AUC change when adding an index to age + sex:", fmt(delta_max, 3), "\n")
imprimir_or <- function(f, titulo) {
  cat(titulo, "\n")
  for (tp in c("Unadjusted", "Adjusted")) {
    s <- f[f$tipo == tp, ]
    cat("  ", tp, ": ", paste0(s$predictor, " ", fmt(s$or), " (", fmt(s$lo), "\u2013", fmt(s$hi), ")", collapse = "; "), "\n", sep = "")
  }
}
cols_or <- c("predictor", "tipo", "or", "lo", "hi")
imprimir_or(rbind(or_bin[, cols_or], or_ref_bin[, cols_or]), "Binomial OR (per 1 SD; age per 10 years):")
imprimir_or(rbind(or_ord[, cols_or], or_ref_ord[, cols_or]), "Ordinal OR:")
cat("OR by cut-point (", paste(violan_po, collapse = ", "), "):\n", sep = "")
print(or_cortes[or_cortes$predictor %in% violan_po, c("predictor", "corte", "tipo", "or", "lo", "hi", "p")], digits = 3, row.names = FALSE)
cat("DeLong between indices: minimum Holm p =", fmt(min(delong$p_holm), 3), "\n")
cat("Suicide risk prevalence:", fmt(100 * mean(y_bin, na.rm = TRUE), 1), "%\n")
n_nivel <- table(y_ord)
cat("Levels:", paste0(names(n_nivel), " ", n_nivel, collapse = " | "), "\n")
cat("Figures in:", normalizePath(carpeta_fig), "| results in:", normalizePath(carpeta_res), "\n")
