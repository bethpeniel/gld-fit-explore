# ---------------------------------------------------------------------------
# Published / stored monthly estimates shipped with the app, converted to the
# same long format produced by fit_gld_blocks() so that every figure of the
# papers can be redrawn from them.
# ---------------------------------------------------------------------------

PUBLISHED <- list(
  dji30  = list(label = "DJI30 index 5-min returns, Jan 2001 - Dec 2016 (Cogent Economics & Finance, 2022)",
                file = "data/published_dji30_2001_2016.rds")
)
published_choices <- function() stats::setNames(names(PUBLISHED), vapply(PUBLISHED, `[[`, "", "label"))

load_published <- function(key) {
  p <- readRDS(PUBLISHED[[key]]$file)
  e <- p$estimates
  e$code <- ifelse(e$method == "MM", "gldex_mm", "gldex_ml")
  e$period_date <- e$period
  e$period <- format(e$period, "%Y-%m")
  e$start <- block_start(e$period, "month")
  e$label <- period_pretty(e$period, "month")
  e$n <- NA_integer_
  e$ksd_D <- NA_real_
  e$region <- gld_region(e$l3, e$l4)
  e$region_name <- region_name(e$region)
  e$class <- gld_class(e$l3, e$l4)
  sp <- t(vapply(seq_len(nrow(e)), function(i) gld_support(c(e$l1[i], e$l2[i], e$l3[i], e$l4[i])), numeric(2)))
  e$support_lower <- sp[, 1]; e$support_upper <- sp[, 2]
  e$secs <- NA_real_; e$error <- NA_character_
  e$method <- factor(e$method, levels = c("MM", "MLE"))
  e <- e[order(e$method, e$period), ]
  d <- p$descriptive
  d$period <- format(d$period, "%Y-%m")
  d$start <- block_start(d$period, "month")
  d$label <- period_pretty(d$period, "month")
  rownames(e) <- NULL; rownames(d) <- NULL
  list(title = p$title, source = p$source, notes = p$notes, block = "month", estimates = e, descriptive = d)
}
