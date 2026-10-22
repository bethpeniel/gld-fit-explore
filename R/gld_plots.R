# ---------------------------------------------------------------------------
# Figures (ggplot2). One palette, fixed colour per estimation method, status
# of a fit (accepted / rejected / symmetric) carried by shape + colour + legend.
# ---------------------------------------------------------------------------
library(ggplot2)

# categorical slots (validated palette, fixed order); a method keeps its colour whatever else is drawn
METHOD_COLOURS <- c("MM" = "#2a78d6", "MLE" = "#eb6834", "MPS" = "#1baf7a", "QM" = "#1baf7a",
                    "Starship" = "#eda100", "L-mom" = "#e87ba4", "TM" = "#008300", "TL-mom" = "#6250d6",
                    "DLA" = "#e34948", "Descriptive" = "#52514e")
STATUS_COLOURS <- c("Accepted" = "#2a78d6", "Rejected" = "#d03b3b", "Symmetric (λ3 = λ4)" = "#0b0b0b")
STATUS_SHAPES  <- c("Accepted" = 16, "Rejected" = 4, "Symmetric (λ3 = λ4)" = 18)
INK <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781", grid = "#e1e0d9", axis = "#c3c2b7",
         positive = "#2a78d6", negative = "#e34948", surface = "#fcfcfb")

app_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = INK[["grid"]], linewidth = 0.35),
          axis.line.x = element_line(colour = INK[["axis"]], linewidth = 0.4),
          axis.text = element_text(colour = INK[["secondary"]]),
          axis.title = element_text(colour = INK[["secondary"]]),
          strip.text = element_text(face = "bold", colour = INK[["primary"]], hjust = 0),
          plot.title = element_text(face = "bold", colour = INK[["primary"]], size = rel(1.05), lineheight = 1.05),
          plot.title.position = "plot", plot.caption.position = "plot",
          plot.subtitle = element_text(colour = INK[["secondary"]]),
          plot.caption = element_text(colour = INK[["muted"]], hjust = 0, size = rel(0.8), lineheight = 1.05),
          plot.background = element_rect(fill = INK[["surface"]], colour = NA),
          legend.position = "top", legend.justification = "left",
          legend.title = element_text(face = "bold", size = rel(0.85)), legend.text = element_text(size = rel(0.85)),
          plot.margin = margin(8, 14, 8, 8))
}

# wrap a title / caption so that it stays within the figure (width in characters;
# full-width figures take ~75 characters per title line, half-width ones ~42)
wrap_lab <- function(x, width = 75) {
  if (is.null(x) || !nzchar(x)) return(x)
  paste(unlist(lapply(strsplit(x, "\n", fixed = TRUE)[[1]], strwrap, width = width)), collapse = "\n")
}
# labs() with wrapped title / caption; `wrap` = c(title width, caption width)
wlabs <- function(..., title = NULL, caption = NULL, wrap = c(75, 110)) {
  labs(..., title = wrap_lab(title, wrap[1]), caption = wrap_lab(caption, wrap[2]))
}

method_scale <- function(methods, name = "Method") {
  methods <- as.character(unique(methods))
  cols <- METHOD_COLOURS[methods]
  cols[is.na(cols)] <- INK[["muted"]]
  names(cols) <- methods
  scale_colour_manual(name, values = cols, breaks = methods, drop = FALSE)
}

# date axis: month names when the span is short, years otherwise
date_scale <- function(block, dates = NULL) {
  if (block %in% c("full")) return(NULL)
  span <- if (is.null(dates) || all(is.na(dates))) Inf else as.numeric(diff(range(dates, na.rm = TRUE)))
  lab <- if (span < 1100) "%b %Y" else "%Y"
  scale_x_date(date_labels = lab, expand = expansion(mult = c(0.02, 0.02)))
}

# ---- data figures (Figures 1-2 of the paper) --------------------------------
# x: a numeric vector (one series) or a data.frame of several series (one facet each)
plot_levels <- function(time, x, series = "price", title = NULL) {
  if (is.data.frame(x)) {
    df <- do.call(rbind, lapply(names(x), function(v) data.frame(time = time, x = x[[v]], series = v)))
    df$series <- factor(df$series, levels = names(x))
  } else df <- data.frame(time = time, x = x, series = series[1])
  df <- df[is.finite(df$x), ]
  p <- ggplot(df, aes(time, x)) + geom_line(colour = METHOD_COLOURS[["MM"]], linewidth = 0.35) +
    wlabs(x = NULL, y = NULL, title = title %||% sprintf("%s: levels", paste(series, collapse = ", "))) + app_theme()
  if (length(unique(df$series)) > 1) p + facet_wrap(~series, ncol = 1, scales = "free_y") else p + labs(y = series[1])
}

plot_returns <- function(rets, series = "returns", kind = "log", title = NULL) {
  ylab <- switch(kind, log = "log return", simple = "simple return", "return")
  p <- ggplot(rets, aes(time, ret)) + geom_line(colour = METHOD_COLOURS[["MM"]], linewidth = 0.3) +
    geom_hline(yintercept = 0, colour = INK[["axis"]], linewidth = 0.4) +
    wlabs(x = NULL, y = ylab, title = title %||% sprintf("%s: filtered %ss", series, ylab)) + app_theme()
  if ("series" %in% names(rets) && length(unique(rets$series)) > 1) p <- p + facet_wrap(~series, ncol = 1, scales = "free_y")
  p
}

# ---- rolling-window series (thesis Figure 5.7) ------------------------------
plot_rolling_series <- function(res, params = c("l3", "l4"), title = "Rolling-window GLD estimates") {
  n_series <- length(unique(res$series))
  labs_p <- c(l1 = "λ1 (location)", l2 = "λ2 (scale)", l3 = "λ3 (shape, left tail / skewness)", l4 = "λ4 (shape, right tail / kurtosis)",
              mean = "GLD mean", variance = "GLD variance", skewness = "GLD skewness", kurtosis = "GLD excess kurtosis")
  params <- params[params %in% names(labs_p)]
  long <- do.call(rbind, lapply(params, function(v)
    data.frame(time = res$time, series = res$series, method = res$method, param = labs_p[[v]], value = res[[v]])))
  long$param <- factor(long$param, levels = labs_p[params])
  ggplot(long, aes(time, value, colour = method, group = method)) + geom_line(linewidth = 0.45) +
    facet_grid(rows = vars(param), cols = vars(series), scales = "free_y") +
    method_scale(levels(res$method)) +
    wlabs(x = NULL, y = NULL, title = title, caption = "One estimate per window end date.") +
    app_theme() + theme(strip.text.y = element_text(angle = if (n_series > 2) 90 else 0, hjust = 0))
}

# descriptive moments per sub-sample, coloured by sign (the colour-coded bars of the working paper)
plot_desc_moments <- function(desc, block) {
  long <- do.call(rbind, lapply(c("mean", "variance", "skewness", "kurtosis"), function(v)
    data.frame(start = desc$start, label = desc$label %||% desc$period, moment = v, value = desc[[v]])))
  long$moment <- factor(long$moment, levels = c("mean", "variance", "skewness", "kurtosis"),
                        labels = c("Mean", "Variance", "Skewness", "Excess kurtosis"))
  long$sign <- ifelse(long$value >= 0, "Positive", "Negative")
  if (block == "full" || all(is.na(long$start))) { long$start <- long$label; xs <- NULL } else xs <- date_scale(block, long$start)
  ggplot(long, aes(start, value, fill = sign)) +
    geom_col(width = if (block == "month") 24 else if (block == "year") 300 else if (block == "quarter") 75 else if (block == "week") 6 else 0.8) +
    geom_hline(yintercept = 0, colour = INK[["axis"]], linewidth = 0.4) +
    facet_wrap(~moment, ncol = 1, scales = "free_y") + xs +
    scale_fill_manual("Sign", values = c(Positive = INK[["positive"]], Negative = INK[["negative"]])) +
    wlabs(x = NULL, y = NULL, title = "Descriptive moments by sub-sample") + app_theme()
}

# ---- fit diagnostics (Figures 2-3 of the paper) -----------------------------
plot_fit_hist <- function(x, fits, bins = NULL, title = NULL, show_normal = TRUE, wrap = c(42, 60)) {
  bins <- bins %||% max(10, grDevices::nclass.FD(x))
  curves <- do.call(rbind, lapply(names(fits), function(m) { d <- gld_density_curve(fits[[m]]); d$method <- m; d }))
  rng <- range(x)
  curves <- curves[curves$x >= rng[1] - 0.05 * diff(rng) & curves$x <= rng[2] + 0.05 * diff(rng), ]
  p <- ggplot() +
    geom_histogram(data = data.frame(x = x), aes(x, after_stat(density)), bins = bins,
                   fill = "#d9d8d1", colour = INK[["surface"]], linewidth = 0.2) +
    geom_line(data = curves, aes(x, y, colour = method), linewidth = 0.9) +
    method_scale(c(names(fits), if (show_normal) "Descriptive"), "Fitted density") +
    wlabs(x = "return", y = "density", title = title, wrap = wrap) + app_theme()
  if (show_normal) {
    nd <- data.frame(x = seq(rng[1], rng[2], length.out = 300))
    nd$y <- stats::dnorm(nd$x, mean(x), stats::sd(x)); nd$method <- "Descriptive"
    p <- p + geom_line(data = nd, aes(x, y, colour = method), linewidth = 0.6, linetype = "22")
  }
  p
}

plot_fit_qq <- function(x, fits, title = NULL, wrap = c(42, 60)) {
  xs <- sort(x); pp <- stats::ppoints(length(xs))
  df <- do.call(rbind, lapply(names(fits), function(m) data.frame(method = m, theoretical = gld_quantile(pp, fits[[m]]), sample = xs)))
  df$method <- factor(df$method, levels = names(fits))
  ggplot(df, aes(theoretical, sample, colour = method)) +
    geom_abline(slope = 1, intercept = 0, colour = INK[["axis"]], linewidth = 0.5) +
    geom_point(size = 1.1, alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~method, scales = "free") + method_scale(names(fits)) +
    wlabs(x = "fitted GLD quantiles", y = "sample quantiles", title = title, wrap = wrap) + app_theme()
}

# ---- time series of parameters and moments (Figures 4-5) --------------------
plot_lambda_series <- function(res, block, title = "GLD parameters by sub-sample") {
  long <- do.call(rbind, lapply(c("l1", "l2", "l3", "l4"), function(v)
    data.frame(start = res$start, label = res$label, method = res$method, param = v, value = res[[v]], rejected = res$rejected %in% TRUE)))
  long$param <- factor(long$param, levels = c("l1", "l2", "l3", "l4"), labels = c("λ1 (location)", "λ2 (scale)", "λ3 (shape, left tail)", "λ4 (shape, right tail)"))
  single <- block == "full" || all(is.na(long$start))
  if (single) long$start <- long$label
  p <- ggplot(long, aes(start, value, colour = method, group = method)) + facet_wrap(~param, ncol = 1, scales = "free_y") +
    method_scale(levels(res$method)) + wlabs(x = NULL, y = NULL, title = title, caption = "× = rejected fit") + app_theme()
  if (single) p + geom_point(size = 3) else
    p + geom_line(linewidth = 0.6) + geom_point(data = long[long$rejected, ], shape = 4, size = 2.6, stroke = 1, colour = STATUS_COLOURS[["Rejected"]]) +
      date_scale(block, long$start)
}

plot_moment_series <- function(res, desc, block, title = "Descriptive vs fitted GLD moments") {
  mom <- c("mean", "variance", "skewness", "kurtosis")
  fitted <- do.call(rbind, lapply(mom, function(v) data.frame(start = res$start, label = res$label, method = as.character(res$method), moment = v, value = res[[v]])))
  d <- do.call(rbind, lapply(mom, function(v) data.frame(start = desc$start, label = desc$label %||% desc$period, method = "Descriptive", moment = v, value = desc[[v]])))
  long <- rbind(d, fitted)
  long$moment <- factor(long$moment, levels = mom, labels = c("Mean", "Variance", "Skewness", "Excess kurtosis"))
  lev <- c("Descriptive", levels(res$method)); long$method <- factor(long$method, levels = lev)
  single <- block == "full" || all(is.na(long$start))
  if (single) long$start <- long$label
  p <- ggplot(long, aes(start, value, colour = method, group = method)) + facet_wrap(~moment, ncol = 1, scales = "free_y") +
    method_scale(lev) + wlabs(x = NULL, y = NULL, title = title, caption = "Fitted = theoretical GLD moments (excess kurtosis); blank where λ3 or λ4 ≤ -1/4.") + app_theme()
  if (single) p + geom_point(size = 3, position = position_dodge(width = 0.3)) else
    p + geom_line(aes(linewidth = method == "Descriptive")) + scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.55), guide = "none") +
      date_scale(block, long$start)
}

# ---- lambda space (Figures 1, 6, 7) -----------------------------------------
fit_status <- function(res) {
  s <- ifelse(res$symmetric %in% TRUE, "Symmetric (λ3 = λ4)", ifelse(res$rejected %in% TRUE, "Rejected", "Accepted"))
  factor(s, levels = names(STATUS_COLOURS))
}

plot_lambda_space <- function(res, label_points = FALSE, common_limits = TRUE, title = "λ3-λ4 space of the fitted GLDs", ncol = 2, facet = "method") {
  df <- res[is.finite(res$l3) & is.finite(res$l4), ]
  df$status <- fit_status(df)
  grid_layout <- facet == "panel" && all(c("series", "method") %in% names(df))   # rolling scheme: methods x series grid
  if (grid_layout) { df$series <- factor(df$series, levels = levels(res$series)); df$orig_method <- df$method }
  if (facet != "method") {   # facet by another column (e.g. series x method in the rolling scheme)
    df$method <- factor(df[[facet]], levels = unique(res[[facet]]))
    res <- df
  }
  df$method <- factor(df$method, levels = levels(res$method))
  pt_alpha <- if (nrow(df) > 500) 0.35 else 1
  show_regions <- nlevels(df$method) <= 4
  limfun <- function(v) { r <- range(v); pad <- 0.12 * max(diff(r), 0.02); c(min(r[1] - pad, -0.02), max(r[2] + pad, 0.02)) }
  lims <- do.call(rbind, lapply(levels(df$method), function(m) {
    v <- if (common_limits) c(df$l3, df$l4) else c(df$l3[df$method == m], df$l4[df$method == m])
    if (!length(v)) v <- c(-0.1, 0.1)
    l <- limfun(v); data.frame(method = m, lo = l[1], hi = l[2])
  }))
  lims$method <- factor(lims$method, levels = levels(df$method))
  reg <- do.call(rbind, lapply(seq_len(nrow(lims)), function(i) {
    lo <- lims$lo[i]; hi <- lims$hi[i]; inset <- 0.03 * (hi - lo)
    data.frame(method = lims$method[i], x = c(lo + inset, hi - inset, hi - inset, lo + inset), y = c(hi - inset, lo + inset, hi - inset, lo + inset),
               hjust = c(0, 1, 1, 0), vjust = c(1, 0, 1, 0),
               lab = c("Region 1: lower infinite support", "Region 2: upper infinite support", "Region 3: finite support", "Region 4: infinite support"))
  }))
  p <- ggplot(df, aes(l3, l4)) +
    geom_blank(data = lims, aes(x = lo, y = lo)) + geom_blank(data = lims, aes(x = hi, y = hi)) +
    geom_rect(data = lims, aes(xmin = lo, xmax = 0, ymin = lo, ymax = 0), inherit.aes = FALSE, fill = "#f0efec", alpha = 0.6) +
    geom_hline(yintercept = 0, colour = INK[["secondary"]], linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = INK[["secondary"]], linewidth = 0.4) +
    geom_abline(slope = 1, intercept = 0, colour = INK[["muted"]], linetype = "22", linewidth = 0.5) +
    (if (show_regions) geom_text(data = reg, aes(x, y, label = lab, hjust = hjust, vjust = vjust), size = 2.9, colour = INK[["muted"]])) +
    geom_point(aes(colour = status, shape = status, size = status), stroke = 1.1, alpha = pt_alpha) +
    scale_colour_manual("Fit status", values = STATUS_COLOURS, drop = FALSE) +
    scale_shape_manual("Fit status", values = STATUS_SHAPES, drop = FALSE) +
    scale_size_manual("Fit status", values = c(2.2, 3.4, 3.8), drop = FALSE) +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
    (if (grid_layout) facet_grid(rows = vars(orig_method), cols = vars(series), scales = if (common_limits) "fixed" else "free")
     else facet_wrap(~method, ncol = ncol, scales = if (common_limits) "fixed" else "free")) +
    wlabs(x = "λ3 (left-tail shape)", y = "λ4 (right-tail shape)", title = title,
          caption = "Dashed diagonal: λ3 = λ4 (symmetry). Shaded: Region 4 (infinite support).") +
    app_theme() + theme(aspect.ratio = 1)
  if (label_points && nrow(df) <= 60) {
    p <- p + if (requireNamespace("ggrepel", quietly = TRUE))
      ggrepel::geom_text_repel(aes(label = label), size = 3, colour = INK[["secondary"]], max.overlaps = 40, seed = 1) else
      geom_text(aes(label = label), size = 3, colour = INK[["secondary"]], vjust = -0.9)
  }
  p
}

# scatter of one parameter under two methods (the L3-L4 cross plots)
plot_cross_methods <- function(res, m1, m2, label_points = FALSE) {
  a <- res[res$method == m1, c("period", "label", "l3", "l4", "rejected")]
  b <- res[res$method == m2, c("period", "l3", "l4", "rejected")]
  m <- merge(a, b, by = "period", suffixes = c("_a", "_b"))
  if (!nrow(m)) return(NULL)
  long <- rbind(data.frame(period = m$period, label = m$label, param = "λ3", x = m$l3_a, y = m$l3_b, rejected = m$rejected_a %in% TRUE | m$rejected_b %in% TRUE),
                data.frame(period = m$period, label = m$label, param = "λ4", x = m$l4_a, y = m$l4_b, rejected = m$rejected_a %in% TRUE | m$rejected_b %in% TRUE))
  long$status <- factor(ifelse(long$rejected, "Rejected", "Accepted"), levels = c("Accepted", "Rejected"))
  r <- range(c(long$x, long$y), na.rm = TRUE); pad <- 0.08 * diff(r); lims <- c(r[1] - pad, r[2] + pad)
  p <- ggplot(long, aes(x, y)) +
    geom_hline(yintercept = 0, colour = INK[["secondary"]], linewidth = 0.4) + geom_vline(xintercept = 0, colour = INK[["secondary"]], linewidth = 0.4) +
    geom_abline(slope = 1, intercept = 0, colour = INK[["muted"]], linetype = "22", linewidth = 0.5) +
    geom_point(aes(colour = status, shape = status, size = status), stroke = 1.1) +
    scale_colour_manual("Fit status (either method)", values = STATUS_COLOURS[1:2], drop = FALSE) +
    scale_shape_manual("Fit status (either method)", values = STATUS_SHAPES[1:2], drop = FALSE) +
    scale_size_manual("Fit status (either method)", values = c(2.2, 3.4), drop = FALSE) +
    coord_cartesian(xlim = lims, ylim = lims, expand = FALSE) + facet_wrap(~param) +
    wlabs(x = sprintf("%s estimate", m1), y = sprintf("%s estimate", m2), title = sprintf("Shape parameters: %s vs %s", m2, m1),
          caption = "Dashed diagonal: both methods agree.") + app_theme() + theme(aspect.ratio = 1)
  if (label_points && nrow(m) <= 60) p <- p + geom_text(aes(label = label), size = 3, colour = INK[["secondary"]], vjust = -0.9)
  p
}

# ---- regions, goodness-of-fit ---------------------------------------------
plot_region_counts <- function(tab) {
  tab$region_short <- paste("Region", tab$region)
  ggplot(tab, aes(region_short, Freq, fill = method)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.62) +
    geom_text(aes(label = ifelse(rejected > 0, paste0(rejected, " rej."), ""), group = method), position = position_dodge(width = 0.7),
              vjust = -0.4, size = 3, colour = INK[["secondary"]]) +
    scale_fill_manual("Method", values = METHOD_COLOURS[levels(tab$method)]) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    wlabs(x = NULL, y = "number of sub-samples", title = "Support regions of the fits",
          caption = "Labels: rejected fits.", wrap = c(42, 60)) + app_theme()
}

plot_gof_series <- function(res, block, alpha = 0.05, ksr_threshold = 70) {
  long <- rbind(data.frame(start = res$start, label = res$label, method = res$method, stat = "KS-R: % of re-samples not rejecting", value = res$ksr_pct, ref = ksr_threshold),
                data.frame(start = res$start, label = res$label, method = res$method, stat = "KS-D: p-value", value = res$ksd_p, ref = alpha))
  long <- long[!is.na(long$value), ]
  if (!nrow(long)) return(NULL)
  single <- block == "full" || all(is.na(long$start))
  if (single) long$start <- long$label
  p <- ggplot(long, aes(start, value, colour = method, group = method)) +
    geom_hline(aes(yintercept = ref), colour = STATUS_COLOURS[["Rejected"]], linetype = "22", linewidth = 0.5) +
    facet_wrap(~stat, ncol = 1, scales = "free_y") + method_scale(levels(res$method)) +
    wlabs(x = NULL, y = NULL, title = "Goodness-of-fit by sub-sample", caption = "Dashed line: rejection threshold.") + app_theme()
  if (single) p + geom_point(size = 3, position = position_dodge(width = 0.3)) else p + geom_line(linewidth = 0.55) + geom_point(size = 1.3) + date_scale(block, long$start)
}

# Figure 1 of the paper: the support regions of the FMKL GLD
plot_regions_diagram <- function() {
  reg <- data.frame(x = c(-0.5, 0.5, 0.5, -0.5), y = c(0.5, -0.5, 0.5, -0.5),
                    lab = c("Region 1\nλ3 <= 0, λ4 > 0\nlower tail unbounded, upper tail bounded",
                            "Region 2\nλ3 > 0, λ4 <= 0\nlower tail bounded, upper tail unbounded",
                            "Region 3\nλ3 > 0, λ4 > 0\nfinite support",
                            "Region 4\nλ3 <= 0, λ4 <= 0\ninfinite support (both tails unbounded)"))
  ggplot() + annotate("rect", xmin = -1, xmax = 0, ymin = -1, ymax = 0, fill = "#f0efec") +
    geom_hline(yintercept = 0, colour = INK[["secondary"]]) + geom_vline(xintercept = 0, colour = INK[["secondary"]]) +
    geom_abline(slope = 1, intercept = 0, colour = INK[["muted"]], linetype = "22") +
    geom_text(data = reg, aes(x, y, label = lab), size = 3.6, colour = INK[["primary"]], lineheight = 1) +
    coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1), expand = FALSE) +
    wlabs(x = "λ3", y = "λ4", title = "FMKL-GLD support regions", caption = "After Chalabi, Scott & Wuertz (2010, 2012). Dashed diagonal: λ3 = λ4.", wrap = c(42, 60)) +
    app_theme() + theme(aspect.ratio = 1)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
