# =========================
#  PAF(t) para Cox (manual)
# =========================
library(survival)

# util: operador OR nulo
`%||%` <- function(a, b) if (is.null(a)) b else a

# --------- helpers de riesgo/base ---------
.risk_mean <- function(H0, eta_vec) {
  if (anyNA(eta_vec)) stop("eta_vec contiene NA (revisa niveles de factores).")
  mean(1 - exp(- H0 * exp(eta_vec)))
}

.get_H0_vec <- function(fit, t_vec) {
  bh <- basehaz(fit, centered = FALSE)  # data.frame con columnas: time, hazard
  sapply(t_vec, function(tt) {
    idx <- max(which(bh$time <= tt))
    if (!is.finite(idx) || idx < 1) 0 else bh$hazard[idx]
  })
}

# --------- helpers de datos/niveles ---------
# Alinea niveles de factores del data.frame según xlevels del modelo.
# Soporta nombres como "factor(year_week1)" (mapear a columna "year_week1").
.align_levels <- function(fit, df) {
  xl <- fit$xlevels
  if (is.null(xl)) return(df)
  for (tn in names(xl)) {
    var <- tn
    if (grepl("^factor\\(.+\\)$", tn)) {
      var <- sub("^factor\\((.+)\\)$", "\\1", tn)
    }
    if (var %in% names(df)) {
      df[[var]] <- factor(df[[var]], levels = xl[[tn]])
    }
  }
  df
}

# Resolver nombre de exposición de forma tolerante a pequeñas diferencias
.resolve_expose_name <- function(expose, data){
  nm <- names(data)
  nm_tidy <- trimws(nm)
  nm_syn  <- make.names(nm_tidy)
  if (expose %in% nm)      return(expose)
  if (expose %in% nm_tidy) return(nm[match(expose, nm_tidy)])
  if (expose %in% nm_syn)  return(nm[match(expose, nm_syn)])
  cand <- unique(c(
    nm[agrep(expose, nm,      max.distance = 0.1)],
    nm[agrep(expose, nm_tidy, max.distance = 0.1)],
    nm[agrep(expose, nm_syn,  max.distance = 0.1)]
  ))
  if (length(cand) == 1) return(cand)
  stop(sprintf("No se encontró la columna de exposición '%s' en data.", expose))
}

# --------- construcción de predictores lineales (observado y CF) ---------
.make_eta_cf <- function(fit, data, expose, cf_fun) {
  data  <- .align_levels(fit, data)
  eta_obs <- drop(predict(fit, newdata = data, type = "lp"))
  if (anyNA(eta_obs)) stop("eta_obs tiene NA tras alinear niveles. Revisa data.")

  data_cf <- cf_fun(data)
  if (!(.resolve_expose_name(expose, data_cf) %in% names(data_cf))) {
    stop("La función cf_fun devolvió un data.frame sin la exposición esperada.")
  }
  data_cf <- .align_levels(fit, data_cf)
  eta_cf  <- drop(predict(fit, newdata = data_cf, type = "lp"))
  if (anyNA(eta_cf)) stop("eta_cf tiene NA (probable desalineación de niveles tras cf_fun).")

  list(eta_obs = eta_obs, eta_cf = eta_cf)
}

# --------- FUNCIÓN PRINCIPAL (punto) ----------
# fit: coxph ya ajustado
# data: data.frame (mismas columnas/levels con las que se ajustó fit)
# expose: nombre de la exposición continua (columna en data/presente en el modelo)
# t_vec: vector de tiempos (p. ej., c(32,34,37))
# cf_fun: función que aplica la intervención (debe devolver mismo df con solo la exposición modificada)
paf_cox_manual <- function(fit, data, expose, t_vec, cf_fun) {
  stopifnot(inherits(fit, "coxph"))
  stopifnot(is.data.frame(data))
  stopifnot(length(t_vec) >= 1)

  expose_res <- .resolve_expose_name(expose, data)

  etas <- .make_eta_cf(fit, data, expose_res, cf_fun)
  H0   <- .get_H0_vec(fit, t_vec)

  Risk_obs <- mapply(.risk_mean, H0, MoreArgs = list(eta_vec = etas$eta_obs))
  Risk_cf  <- mapply(.risk_mean, H0, MoreArgs = list(eta_vec = etas$eta_cf))
  PAF      <- (Risk_obs - Risk_cf) / Risk_obs

  data.frame(t = t_vec, H0 = H0,
             Risk_obs = as.numeric(Risk_obs),
             Risk_cf  = as.numeric(Risk_cf),
             PAF      = as.numeric(PAF))
}

# --------- BOOTSTRAP con opción de cluster ----------
# cluster_var: nombre de columna para remuestreo por conglomerado (p.ej., "com"); si NULL, bootstrap individual
# B: réplicas
paf_cox_boot <- function(fit, data, expose, t_vec, cf_fun, B = 300, cluster_var = NULL, seed = 123) {
  set.seed(seed)
  expose_res <- .resolve_expose_name(expose, data)

  # guardar niveles originales para restaurarlos en submuestras
  factor_levels <- lapply(data, function(x) if (is.factor(x)) levels(x) else NULL)

  boot_once <- function(dfb) {
  for (nm in names(dfb)) {
    if (!is.null(factor_levels[[nm]]) && !is.factor(dfb[[nm]])) {
      dfb[[nm]] <- factor(dfb[[nm]], levels = factor_levels[[nm]])
    }
  }
  dfb <- .align_levels(fit, dfb)

  fit_b <- try(survival::coxph(
    stats::formula(fit),
    data  = dfb,
    ties  = fit$method %||% "breslow",
    x     = TRUE, y = TRUE, model = TRUE,
    control = survival::coxph.control(iter.max = 50)
  ), silent = TRUE)

  if (inherits(fit_b, "try-error")) return(rep(NA_real_, length(t_vec)))
  paf_cox_manual(fit_b, dfb, expose_res, t_vec, cf_fun)$PAF
}

  if (is.null(cluster_var)) {
    idx_list <- replicate(B, sample.int(nrow(data), replace = TRUE), simplify = FALSE)
    mat <- sapply(idx_list, function(idx) boot_once(data[idx, , drop = FALSE]))
  } else {
    stopifnot(cluster_var %in% names(data))
    clusters <- unique(data[[cluster_var]])
    mat <- sapply(seq_len(B), function(b) {
      sel_cl <- sample(clusters, replace = TRUE, size = length(clusters))
      dfb <- data[data[[cluster_var]] %in% sel_cl, , drop = FALSE]
      boot_once(dfb)
    })
  }

  PAF_hat <- paf_cox_manual(fit, data, expose_res, t_vec, cf_fun)$PAF
  LCL <- apply(mat, 1, quantile, probs = 0.025, na.rm = TRUE)
  UCL <- apply(mat, 1, quantile, probs = 0.975, na.rm = TRUE)

  out <- data.frame(t = t_vec, PAF = PAF_hat, LCL = LCL, UCL = UCL)
  rownames(out) <- NULL
  out
}