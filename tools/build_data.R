# ---------------------------------------------------------------------------
# Builds the example datasets bundled with the app (gld_app/data/*.rds) from
# the raw files in the project folder (C:/Users/peteo/GLD). Run once:
#   Rscript gld_app/tools/build_data.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({library(data.table); library(readxl)})
invisible(Sys.setlocale("LC_TIME", "C"))   # month abbreviations: some locales use "Sept"
src <- "C:/Users/peteo/GLD"
out <- file.path(src, "gld_app", "data")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
save <- function(obj, name) { f <- file.path(out, name); saveRDS(obj, f, compress = "xz"); cat(sprintf("%-40s %6.2f MB\n", name, file.size(f) / 1024^2)) }

# ---- 5-minute S&P 500 index prices (Tick History export, us_SP500.csv) -----
read_trth <- function(file, label) {
  d <- fread(file.path(src, file), colClasses = list(character = "Date-Time"))
  ts <- substr(d[["Date-Time"]], 1, 19)          # local exchange time, offset dropped
  time <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  df <- data.frame(time = time, price = as.numeric(d$Last))
  names(df)[2] <- label
  df <- df[!is.na(df$time) & is.finite(df[[2]]), ]
  df[order(df$time), ]
}
spx <- read_trth("us_SP500.csv", "SPX");  cat("SPX:", nrow(spx), format(range(spx$time)), "\n")
save(spx, "spx_5min_2010_2020.rds")

# ---- DJIA daily closes ------------------------------------------------------
dj <- read.csv(file.path(src, "DJI-daily.csv"))
dj <- data.frame(time = as.POSIXct(dj$Date, tz = "UTC"), DJIA = dj$Close, DJIA_adj = dj$Adj.Close)
dj <- dj[!is.na(dj$time) & is.finite(dj$DJIA), ]
cat("DJIA daily:", nrow(dj), format(range(dj$time)), "\n")
save(dj, "djia_daily_2000_2017.rds")

# ---- REITs daily index levels (REITs.xlsx: Australia, Hong Kong, Japan, US) -
re <- as.data.frame(read_excel(file.path(src, "REITs.xlsx"), sheet = 1))
time <- as.POSIXct(format(re$Date), tz = "UTC")
vals <- as.data.frame(lapply(re[-1], function(v) suppressWarnings(as.numeric(v))))
names(vals) <- c("Australia", "HongKong", "Japan", "US")
reits <- cbind(time = time, vals)
reits <- reits[!is.na(reits$time), ]
reits <- reits[order(reits$time), ]
rownames(reits) <- NULL
cat("REITs:", nrow(reits), format(range(reits$time)), " NA per column:", colSums(is.na(vals)), "\n")
save(reits, "reits_daily_2013_2018.rds")

# ---- Published monthly estimates for the DJI30 index (paper, 2001-2016) ----
parse_gof <- function(g) {
  g <- trimws(as.character(g))
  pct <- suppressWarnings(as.numeric(sub("\\s*\\(.*$", "", g)))
  p <- suppressWarnings(as.numeric(sub("^.*\\(([^)]*)\\).*$", "\\1", g)))
  p[!grepl("\\(", g)] <- NA
  data.frame(ksr_pct = pct, ksd_p = p, rejected = grepl("\\(", g))
}
read_cp <- function(sheet, method) {
  x <- as.data.frame(read_excel(file.path(src, "coloured points.xlsx"), sheet = sheet, .name_repair = "minimal"))
  g <- parse_gof(x[["Gof (%)"]])
  data.frame(period = as.Date(x$Period), method = method,
             l1 = x[["λ1"]], l2 = x[["λ2"]], l3 = x[["λ3"]], l4 = x[["λ4"]],
             ksr_pct = g$ksr_pct, ksd_p = g$ksd_p, rejected = g$rejected,
             symmetric = x$symmetry == 1, stringsAsFactors = FALSE)
}
pub <- rbind(read_cp("mm", "MM"), read_cp("mle", "MLE"))

# monthly descriptive moments and fitted GLD moments from the working document
wip <- readLines(file.path(src, "gld_app", "tools", "wip_tables.txt"), encoding = "UTF-8", warn = FALSE)
parse_table <- function(lines, start_pat, next_pat) {
  s <- grep(start_pat, lines)[1]; e <- grep(next_pat, lines); e <- e[e > s][1]
  if (is.na(e)) e <- length(lines)
  blk <- trimws(lines[(s + 1):(e - 1)])
  blk <- blk[nzchar(blk)]
  i <- grep("^[A-Z][a-z]+ [0-9]{4}$", blk)
  do.call(rbind, lapply(i, function(k) {
    v <- suppressWarnings(as.numeric(blk[k + 1:4]))
    data.frame(period = as.Date(paste("01", blk[k]), format = "%d %B %Y"),
               mean = v[1], variance = v[2], skewness = v[3], kurtosis = v[4])
  }))
}
desc <- parse_table(wip, "^Table 1: Monthly moments", "^Table 2:")
mm_m <- parse_table(wip, "^Table 3: Monthly GLD moments", "^Figure 1:")
ml_m <- parse_table(wip, "^Table 5: Monthly GLD moments", "^Figure 3:")
cat("descriptive moments:", nrow(desc), " MM moments:", nrow(mm_m), " MLE moments:", nrow(ml_m), "\n")
mm_m$method <- "MM"; ml_m$method <- "MLE"
fm <- rbind(mm_m, ml_m)
pub <- merge(pub, fm, by = c("period", "method"), all.x = TRUE)
pub <- pub[order(pub$method, pub$period), ]
dji_pub <- list(
  title = "DJI30 index, 5-min log-returns, monthly sub-samples, Jan 2001 - Dec 2016 (Owusu Junior, Jeyasreedharan & Alagidede, 2022)",
  source = "Owusu Junior, P., Jeyasreedharan, N., & Alagidede, I. P. (2022). On the goodness-of-fits of the generalized lambda distribution on high-frequency stock index returns. Cogent Economics & Finance, 10(1), 2095764.",
  block = "month", estimates = pub, descriptive = desc,
  notes = c("FMKL GLD fitted by moment matching (MM) and maximum likelihood (MLE) with GLDEX; 5-min returns with overnight and zero returns removed and winsorised.",
            "KS-R = share of 1000 Kolmogorov-Smirnov re-samples not rejecting the fit; KS-D p-values are only recorded for the months rejected at 5%.",
            "Rejection flags are those used for Figure 6 of the paper (red points); the symmetric MLE fit is December 2005."))
cat("published DJI30 rows:", nrow(pub), " rejected MM:", sum(pub$rejected[pub$method == "MM"]), " MLE:", sum(pub$rejected[pub$method == "MLE"]), "\n")
save(dji_pub, "published_dji30_2001_2016.rds")

cat("done\n")
