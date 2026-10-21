# ---------------------------------------------------------------------------
# FMKL generalised lambda distribution: fitting (gld and GLDEX engines),
# goodness-of-fit, fitted moments, support regions, batch estimation.
# ---------------------------------------------------------------------------

# Estimation methods. 'short' is the label used in tables/plots; the colour slot
# is fixed per method so that a method keeps its colour whatever else is shown.
GLD_METHODS <- data.frame(
  code   = c("gldex_mm", "gldex_ml", "gldex_qs", "gldex_ss",
             "gld_Mom", "gld_ML", "gld_MSP", "gld_TM", "gld_SM", "gld_Lmom", "gld_TL", "gld_DLA"),
  engine = c(rep("gldex", 4), rep("gld", 8)),
  short  = c("MM", "MLE", "QM", "Starship", "MM", "MLE", "MPS", "TM", "Starship", "L-mom", "TL-mom", "DLA"),
  label  = c("MM - moment matching (fun.RMFMKL.mm, as in the papers)",
             "MLE - maximum likelihood (fun.RMFMKL.ml, as in the papers)",
             "QM - quantile matching (fun.RMFMKL.qs)",
             "Starship (GLDEX::starship)",
             "MM - method of moments (fit.fkml 'Mom')",
             "MLE - numerical maximum likelihood (fit.fkml 'ML')",
             "MPS - maximum product of spacings (fit.fkml 'MSP')",
             "TM - Titterington's method (fit.fkml 'TM')",
             "Starship (fit.fkml 'SM')",
             "L-moments (fit.fkml 'Lmom')",
             "TL-moments, trimmed (fit.fkml 'TL')",
             "DLA - distributional least absolutes (fit.fkml 'DLA')"),
  stringsAsFactors = FALSE)

method_choices <- function(engine) {
  m <- GLD_METHODS[GLD_METHODS$engine == engine, ]
  stats::setNames(m$code, m$label)
}
method_short <- function(code) GLD_METHODS$short[match(code, GLD_METHODS$code)]

# Fit one block of returns with one method. Returns list(lambda, secs, error).
fit_gld_one <- function(x, code, seed = 1000, t1 = 0, t2 = 0) {
  set.seed(seed)
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch({
    lam <- switch(code,
      gldex_mm = GLDEX::fun.RMFMKL.mm(x),
      gldex_ml = GLDEX::fun.RMFMKL.ml(x),
      gldex_qs = GLDEX::fun.RMFMKL.qs(x),
      gldex_ss = GLDEX::starship(x, param = "FMKL")$lambda,
      gld_Mom  = gld::fit.fkml(x, method = "Mom")$lambda,
      gld_ML   = gld::fit.fkml(x, method = "ML")$lambda,
      gld_MSP  = gld::fit.fkml(x, method = "MSP")$lambda,
      gld_TM   = gld::fit.fkml(x, method = "TM")$lambda,
      gld_SM   = gld::fit.fkml(x, method = "SM")$lambda,
      gld_Lmom = gld::fit.fkml(x, method = "Lmom")$lambda,
      gld_TL   = gld::fit.fkml(x, method = "TL", t1 = t1, t2 = t2)$lambda,
      gld_DLA  = gld::fit.fkml(x, method = "DLA")$lambda,
      stop("unknown method ", code))
    lam <- as.numeric(lam)[1:4]
    if (any(!is.finite(lam)) || lam[2] <= 0) stop("the optimiser did not return a valid FMKL parameter vector")
    list(lambda = lam, error = NA_character_)
  }, error = function(e) list(lambda = rep(NA_real_, 4), error = conditionMessage(e)))
  res$secs <- proc.time()[["elapsed"]] - t0
  res
}

# ---- distribution helpers (FMKL, gld package) ------------------------------
gld_cdf <- function(q, lambda) gld::pgl(q, lambda1 = lambda, param = "fkml")
gld_quantile <- function(p, lambda) gld::qgl(p, lambda1 = lambda, param = "fkml")
gld_density_curve <- function(lambda, n = 512) {
  u <- stats::ppoints(n, a = 0)
  u <- u[u > 0 & u < 1]
  data.frame(x = gld::qgl(u, lambda1 = lambda, param = "fkml"), y = gld::dqgl(u, lambda1 = lambda, param = "fkml"))
}

# Support of the FMKL GLD: finite on a side only when the corresponding shape parameter is positive
gld_support <- function(lambda) {
  lo <- if (lambda[3] > 0) lambda[1] - 1 / (lambda[2] * lambda[3]) else -Inf
  hi <- if (lambda[4] > 0) lambda[1] + 1 / (lambda[2] * lambda[4]) else Inf
  c(lower = lo, upper = hi)
}

# Regions of the lambda3-lambda4 space (Chalabi, Scott & Wuertz 2010, 2012; Figure 1 of the paper)
REGION_LABELS <- c("Region 1: lower infinite support" = 1, "Region 2: upper infinite support" = 2,
                   "Region 3: finite support" = 3, "Region 4: infinite support" = 4)
gld_region <- function(l3, l4) {
  r <- ifelse(l3 <= 0 & l4 > 0, 1L, ifelse(l3 > 0 & l4 <= 0, 2L, ifelse(l3 > 0 & l4 > 0, 3L, 4L)))
  r[is.na(l3) | is.na(l4)] <- NA_integer_
  r
}
region_name <- function(r) ifelse(is.na(r), NA_character_, paste("Region", r))

# Shape classes of Freimer et al. (1988)
gld_class <- function(l3, l4) {
  cls <- ifelse(l3 < 1 & l4 < 1, "I: unimodal",
         ifelse(l3 > 2 & l4 > 2, "V: unimodal, truncated tails",
         ifelse(l3 > 1 & l3 < 2 & l4 > 1 & l4 < 2, "III: U-shaped",
         ifelse(l3 > 1 & l4 < 1, "II: monotone (left tail truncated)",
         ifelse(l3 < 1 & l4 > 1, "II': monotone (right tail truncated)",
         ifelse(l3 < 2 & l4 > 1 & l4 < 2, "IV: S-shaped", "other"))))))
  cls[is.na(l3) | is.na(l4)] <- NA_character_
  cls
}

# Theoretical first four moments of the fitted GLD (excess kurtosis, as fun.theo.mv.gld(normalise = "Y"))
gld_fitted_moments <- function(lambda) {
  if (any(!is.finite(lambda))) return(c(mean = NA, variance = NA, skewness = NA, kurtosis = NA))
  if (lambda[3] <= -0.25 || lambda[4] <= -0.25) return(c(mean = NA, variance = NA, skewness = NA, kurtosis = NA))
  m <- tryCatch(suppressWarnings(GLDEX::fun.theo.mv.gld(lambda[1], lambda[2], lambda[3], lambda[4], param = "fmkl", normalise = "Y")),
                error = function(e) rep(NA_real_, 4))
  m <- as.numeric(m)
  c(mean = m[1], variance = m[2], skewness = m[3], kurtosis = m[4])
}

# ---- goodness-of-fit ---------------------------------------------------------
# KS-D: one-sample Kolmogorov-Smirnov distance test against the fitted GLD
ks_distance <- function(x, lambda) {
  out <- tryCatch(suppressWarnings(stats::ks.test(x, function(q) gld_cdf(q, lambda))),
                  error = function(e) NULL)
  if (is.null(out)) return(c(D = NA_real_, p = NA_real_))
  c(D = unname(out$statistic), p = unname(out$p.value))
}
# KS-R: re-sampling KS test of Su (2007), GLDEX::fun.diag.ks.g. Returns % of re-samples not rejecting.
ks_resample <- function(x, lambda, no.test = 1000, prop = 0.9, alpha = 0.05, seed = 1000) {
  set.seed(seed)
  k <- tryCatch(suppressWarnings(GLDEX::fun.diag.ks.g(lambda, x, no.test = no.test, len = max(10, floor(prop * length(x))),
                                                      param = "fmkl", alpha = alpha)),
                error = function(e) NA_real_)
  100 * as.numeric(k) / no.test
}

# Re-home a worker closure in a small environment (parent = globalenv) holding only the objects it needs,
# so that exporting it to a cluster does not serialise the calling frame (which inside Shiny holds the session).
lean_worker <- function(fun, objects) {
  env <- list2env(objects, parent = globalenv())
  environment(fun) <- env
  env$one <- fun          # so that clusterExport(cl, "one", envir = environment(one)) finds it
  fun
}

# ---- batch estimation --------------------------------------------------------
# rets: data.frame(time, ret, period) from build_returns(); methods: codes from GLD_METHODS
# Returns a long data.frame with one row per (period, method).
fit_gld_blocks <- function(rets, block, methods, seed = 1000, ks_tests = 1000, ks_prop = 0.9, alpha = 0.05,
                           reject_rule = "ksd", ksr_threshold = 70, sym_tol = 1e-4, t1 = 0, t2 = 0,
                           workers = 1, progress = NULL) {
  periods <- unique(rets$period)
  grid <- expand.grid(period = periods, method = methods, stringsAsFactors = FALSE)
  one <- function(i) {
    p <- grid$period[i]; m <- grid$method[i]
    x <- rets$ret[rets$period == p]
    f <- fit_gld_one(x, m, seed = seed, t1 = t1, t2 = t2)
    lam <- f$lambda
    if (all(is.finite(lam))) {
      ks <- ks_distance(x, lam)
      kr <- if (ks_tests > 0) ks_resample(x, lam, no.test = ks_tests, prop = ks_prop, alpha = alpha, seed = seed) else NA_real_
      fm <- gld_fitted_moments(lam); sp <- gld_support(lam)
    } else { ks <- c(D = NA, p = NA); kr <- NA; fm <- gld_fitted_moments(lam); sp <- c(lower = NA, upper = NA) }
    data.frame(period = p, method = method_short(m), code = m, n = length(x),
               l1 = lam[1], l2 = lam[2], l3 = lam[3], l4 = lam[4],
               mean = fm[["mean"]], variance = fm[["variance"]], skewness = fm[["skewness"]], kurtosis = fm[["kurtosis"]],
               ksd_D = ks[["D"]], ksd_p = ks[["p"]], ksr_pct = kr,
               support_lower = sp[["lower"]], support_upper = sp[["upper"]],
               secs = f$secs, error = f$error, stringsAsFactors = FALSE)
  }
  n <- nrow(grid)
  rows <- vector("list", n)
  if (workers > 1 && n > 1) {
    cl <- parallel::makeCluster(min(workers, n))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, { suppressPackageStartupMessages({library(gld); library(GLDEX)}); NULL })
    # give the worker function a lean environment (no Shiny session, no progress closure) and export it once;
    # helper functions live where this file was sourced (Shiny's app environment, not necessarily globalenv)
    one <- lean_worker(one, list(rets = rets, grid = grid, seed = seed, t1 = t1, t2 = t2, ks_tests = ks_tests, ks_prop = ks_prop, alpha = alpha))
    parallel::clusterExport(cl, "one", envir = environment(one))
    parallel::clusterExport(cl, c("fit_gld_one", "ks_distance", "ks_resample", "gld_fitted_moments", "gld_support",
                                  "gld_cdf", "method_short", "GLD_METHODS"), envir = environment(fit_gld_one))
    chunks <- split(seq_len(n), ceiling(seq_len(n) / min(workers, n)))
    done <- 0
    for (ch in chunks) {
      rows[ch] <- parallel::parLapply(cl, ch, "one")
      done <- done + length(ch)
      if (!is.null(progress)) progress(done / n, sprintf("%s (%d of %d fits)", period_pretty(grid$period[max(ch)], block), done, n))
    }
  } else {
    for (i in seq_len(n)) {
      rows[[i]] <- one(i)
      if (!is.null(progress)) progress(i / n, sprintf("%s - %s (%d of %d fits)", period_pretty(grid$period[i], block), method_short(grid$method[i]), i, n))
    }
  }
  out <- do.call(rbind, rows)
  out$start <- block_start(out$period, block)
  out$label <- period_pretty(out$period, block)
  out$region <- gld_region(out$l3, out$l4)
  out$region_name <- region_name(out$region)
  out$class <- gld_class(out$l3, out$l4)
  out$symmetric <- is.finite(out$l3) & abs(out$l3 - out$l4) < sym_tol
  out$rejected <- rejection_flag(out, reject_rule, alpha, ksr_threshold)
  out$method <- factor(out$method, levels = unique(method_short(methods)))
  out <- out[order(out$method, out$period), ]
  rownames(out) <- NULL
  out
}

rejection_flag <- function(res, rule = "ksd", alpha = 0.05, ksr_threshold = 70) {
  d <- !is.na(res$ksd_p) & res$ksd_p < alpha
  r <- !is.na(res$ksr_pct) & res$ksr_pct < ksr_threshold
  out <- switch(rule, ksd = d, ksr = r, both = d & r, either = d | r, d)
  out[is.na(res$l1)] <- NA
  out
}

# ---- rolling-window estimation (PhD thesis chapter 5; Owusu Junior, Alagidede & Tweneboah 2020) ----
# series_list: named list of data.frames(time, ret) (one per series); window m, step s:
# windows end at observations m, m+s, ... and k = N - m + 1 windows when s = 1.
# Returns a long data.frame with one row per (series, window, method).
fit_gld_rolling <- function(series_list, window = 20, step = 1, methods = "gldex_ml", seed = 1000,
                            ks_d = FALSE, sym_tol = 1e-4, t1 = 0, t2 = 0, workers = 1, progress = NULL) {
  tasks <- do.call(rbind, lapply(names(series_list), function(s) {
    N <- nrow(series_list[[s]])
    if (N < window) return(NULL)
    ends <- seq(window, N, by = max(1, step))
    expand.grid(series = s, end = ends, method = methods, stringsAsFactors = FALSE)
  }))
  if (is.null(tasks) || !nrow(tasks)) stop("No series is longer than the window.")
  one <- function(i) {
    s <- tasks$series[i]; e <- tasks$end[i]; m <- tasks$method[i]
    d <- series_list[[s]]
    x <- d$ret[(e - window + 1):e]
    f <- fit_gld_one(x, m, seed = seed, t1 = t1, t2 = t2)
    lam <- f$lambda
    fm <- gld_fitted_moments(lam)
    ks <- if (ks_d && all(is.finite(lam))) ks_distance(x, lam) else c(D = NA_real_, p = NA_real_)
    data.frame(series = s, time = d$time[e], window_start = d$time[e - window + 1], index = e, n = window,
               method = method_short(m), code = m, l1 = lam[1], l2 = lam[2], l3 = lam[3], l4 = lam[4],
               mean = fm[["mean"]], variance = fm[["variance"]], skewness = fm[["skewness"]], kurtosis = fm[["kurtosis"]],
               ksd_D = ks[["D"]], ksd_p = ks[["p"]], secs = f$secs, error = f$error, stringsAsFactors = FALSE)
  }
  n <- nrow(tasks)
  rows <- vector("list", n)
  if (workers > 1 && n > 1) {
    cl <- parallel::makeCluster(min(workers, n))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, { suppressPackageStartupMessages({library(gld); library(GLDEX)}); NULL })
    one <- lean_worker(one, list(series_list = series_list, tasks = tasks, window = window, seed = seed, t1 = t1, t2 = t2, ks_d = ks_d))
    parallel::clusterExport(cl, "one", envir = environment(one))
    parallel::clusterExport(cl, c("fit_gld_one", "ks_distance", "gld_fitted_moments", "gld_cdf", "method_short", "GLD_METHODS"), envir = environment(fit_gld_one))
    chunk_size <- max(min(workers, n), min(50, ceiling(n / 40)))
    chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
    done <- 0
    for (ch in chunks) {
      rows[ch] <- parallel::parLapply(cl, ch, "one")
      done <- done + length(ch)
      if (!is.null(progress)) progress(done / n, sprintf("%s (%d of %d window fits)", tasks$series[max(ch)], done, n))
    }
  } else {
    every <- max(1, floor(n / 200))
    for (i in seq_len(n)) {
      rows[[i]] <- one(i)
      if (!is.null(progress) && (i %% every == 0 || i == n)) progress(i / n, sprintf("%s - %s (%d of %d window fits)", tasks$series[i], method_short(tasks$method[i]), i, n))
    }
  }
  out <- do.call(rbind, rows)
  out$region <- gld_region(out$l3, out$l4)
  out$region_name <- region_name(out$region)
  out$symmetric <- is.finite(out$l3) & abs(out$l3 - out$l4) < sym_tol
  out$rejected <- if (ks_d) (!is.na(out$ksd_p) & out$ksd_p < 0.05) else FALSE
  out$series <- factor(out$series, levels = names(series_list))
  out$method <- factor(out$method, levels = unique(method_short(methods)))
  out <- out[order(out$series, out$method, out$index), ]
  rownames(out) <- NULL
  out
}

# wide table: one row per window end date, columns <series>_<method>_<parameter> (ready for connectedness / wavelet tools)
rolling_wide <- function(res, params = c("l1", "l2", "l3", "l4", "skewness", "kurtosis")) {
  dates <- sort(unique(res$time))
  out <- data.frame(date = dates)
  single_method <- nlevels(res$method) == 1
  for (s in levels(res$series)) for (m in levels(res$method)) {
    r <- res[res$series == s & res$method == m, ]
    if (!nrow(r)) next
    idx <- match(dates, r$time)
    for (p in params) {
      nm <- if (single_method) paste(s, toupper(sub("^l", "L", p)), sep = "_") else paste(s, m, toupper(sub("^l", "L", p)), sep = "_")
      out[[nm]] <- r[[p]][idx]
    }
  }
  out
}

rolling_overview <- function(res) {
  do.call(rbind, lapply(split(res, list(res$series, res$method), drop = TRUE), function(r) {
    ok <- !is.na(r$l1)
    data.frame(series = as.character(r$series[1]), method = as.character(r$method[1]), windows = nrow(r), failed = sum(!ok),
               region4 = sum(r$region %in% 4), mean_l3 = mean(r$l3, na.rm = TRUE), mean_l4 = mean(r$l4, na.rm = TRUE),
               secs = sum(r$secs, na.rm = TRUE), stringsAsFactors = FALSE)
  }))
}

# %-differences of fitted GLD moments relative to the descriptive moments (Table 2 of the paper)
moment_bias_table <- function(res, desc) {
  d <- desc[, c("period", "skewness", "kurtosis")]
  names(d) <- c("period", "d_skew", "d_kurt")
  m <- merge(res[, c("period", "label", "method", "skewness", "kurtosis")], d, by = "period")
  pct <- function(f, d) 100 * (f - d) / abs(d)
  state <- function(f, d) ifelse(is.na(f) | is.na(d), NA, ifelse(f > d, "Over", ifelse(f < d, "Under", "Null")))
  m$skew_pct <- pct(m$skewness, m$d_skew); m$skew_state <- state(m$skewness, m$d_skew)
  m$kurt_pct <- pct(m$kurtosis, m$d_kurt); m$kurt_state <- state(m$kurtosis, m$d_kurt)
  m <- m[order(m$method, m$period), c("period", "label", "method", "d_skew", "skewness", "skew_pct", "skew_state",
                                     "d_kurt", "kurtosis", "kurt_pct", "kurt_state")]
  rownames(m) <- NULL
  m
}

# counts of fits by region, per method
region_summary <- function(res) {
  res <- res[!is.na(res$region), ]
  tab <- as.data.frame(table(method = res$method, region = factor(res$region, levels = 1:4)))
  rej <- as.data.frame(table(method = res$method[res$rejected %in% TRUE], region = factor(res$region[res$rejected %in% TRUE], levels = 1:4)))
  tab$rejected <- rej$Freq[match(paste(tab$method, tab$region), paste(rej$method, rej$region))]
  tab$rejected[is.na(tab$rejected)] <- 0
  tab$region_label <- names(REGION_LABELS)[as.integer(as.character(tab$region))]
  tab
}

# one-line verdict per method (for value boxes)
method_overview <- function(res) {
  do.call(rbind, lapply(split(res, res$method), function(r) {
    ok <- !is.na(r$l1)
    data.frame(method = as.character(r$method[1]), fits = sum(ok), failed = sum(!ok),
               rejected = sum(r$rejected %in% TRUE), region4 = sum(r$region %in% 4),
               symmetric = sum(r$symmetric %in% TRUE),
               median_ksr = stats::median(r$ksr_pct, na.rm = TRUE), secs = sum(r$secs, na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
}
