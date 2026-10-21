# GLD_FitExplore – fitting the generalised lambda distribution to financial returns (Shiny app)

**Author:** Peterson Owusu Junior, University of Cape Coast, Ghana
— <peterson.owusu@ucc.edu.gh> · <powusujunior@gmail.com> · <https://positivecarryacademy.com/>

A browser-based companion to the author's generalised lambda distribution (GLD) studies:

* Owusu Junior, P., Tweneboah, G., Ijasan, K., & Jeyasreedharan, N. (2019). Modelling return behaviour of global real estate investment trusts equities: Evidence from generalised lambda distribution. *Journal of European Real Estate Research*, 12(3), 311–328.
* Owusu Junior, P. (2020). *Emerging financial markets: Spatial risks, elicitability of risk models, and shape shift contagion.* Doctoral thesis, University of the Witwatersrand (chapter 5: rolling-window GLD shape parameters).
* Owusu Junior, P., Alagidede, I., & Tweneboah, G. (2020). Shape-shift contagion in emerging markets equities: Evidence from frequency- and time-domain analysis. *Economics and Business Letters*, 9(3), 146–156.
* Owusu Junior, P., Jeyasreedharan, N., & Alagidede, I. P. (2022). On the goodness-of-fits of the generalized lambda distribution on high-frequency stock index returns. *Cogent Economics & Finance*, 10(1), 2095764.

The app fits the four-parameter FMKL GLD to returns by moment matching (MM), maximum likelihood
(MLE) and other methods under two sampling schemes:

* **Non-overlapping sub-samples** (Cogent 2022, JERER 2019): monthly (or other) blocks, Kolmogorov–Smirnov
  tests, and every figure of the 2022 paper (parameter and moment time series, the λ3–λ4 lambda space with
  rejected and symmetric fits, MLE-vs-MM scatter, regions of support, %-differences of fitted and
  descriptive moments), for your own data or for the stored published estimates.
* **Rolling window** (thesis chapter 5, EBL 2020): the GLD is re-estimated in windows of m = 20 daily
  returns moved one day at a time (k = N − m + 1 windows), for one or several series at once, giving daily
  time series of λ1–λ4 and of the GLD skewness and kurtosis. The series are plotted and can be downloaded
  as a wide CSV (date × `<series>_<parameter>`) or a long CSV for separate use. The connectedness measures
  applied to them in the thesis are outside the app.

## Status

| Component | State |
|---|---|
| Source (GitHub) | <https://github.com/bethpeniel/gld-fit-explore> — public, branch `main` |
| Live app (Posit Connect Cloud) | **Online** at <https://01a160c2-0f18-1896-5a9f-de608cb2c2b2.share.connect.posit.cloud/> — public, built from the GitHub repository (`manifest.json` lists the R version and packages); every push to `main` republishes it |
| Launcher page | `gld_app.html` embeds the public app; opened with `?local=1` (what `start_app.bat` does) it embeds the local server on port 4322 instead |
| Local copy | `C:\Users\peteo\GLD\gld_app`; `start_app.bat` starts the server on port 4322 and opens `gld_app.html?local=1` |

### Publishing on Posit Connect Cloud (git-backed, like the wavelet app)

1. Sign in at <https://connect.posit.cloud> with GitHub (account `bethpeniel`).
2. **Publish** → **Shiny (R)** → repository `bethpeniel/gld-fit-explore`, branch `main`, primary file `app.R`; `manifest.json` is detected automatically.
3. Choose public or private access and publish. Connect Cloud installs the packages from `manifest.json` (gld, GLDEX, shiny, bslib, DT, ggplot2, shinycssloaders, readxl, lubridate, zip and their dependencies) and starts the app.
4. Every later `git push` to `main` republishes the app automatically. After adding an R package, rerun `rsconnect::writeManifest()` from the app folder and push the new `manifest.json`.
5. The published address is stored as `PUBLIC_URL` at the top of `gld_app.html`; change it there if a custom Connect Cloud URL is set later.

### Embedding on a website

```html
<iframe src="https://01a160c2-0f18-1896-5a9f-de608cb2c2b2.share.connect.posit.cloud/"
        title="GLD_FitExplore" style="width:100%; height:calc(100vh - 120px); border:0;" allow="fullscreen"></iframe>
```

Connect Cloud allows framing; adjust the `120px` to the site's header height.

## Folder layout

```
gld_app/
├── app.R                UI + server (bslib page_sidebar layout, as in the wavelet app)
├── R/
│   ├── data_utils.R     file reading, time-stamp parsing, returns, filters, sub-sampling, descriptive stats
│   ├── gld_fit.R        fitting (gld / GLDEX), KS-D and KS-R tests, fitted moments, regions, batch estimation
│   ├── gld_plots.R      all ggplot2 figures (one palette, fixed colour per method)
│   └── published.R      loader for the stored published estimates
├── data/                example datasets and published estimates (compressed .rds)
├── tools/build_data.R   rebuilds data/ from the raw files in the project folder
├── www/custom.css
├── start_app.bat        double-click to run locally (port 4322)
└── README.md
```

## Run locally

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "gld", "GLDEX", "shinycssloaders", "readxl", "lubridate", "zip"))
shiny::runApp("C:/Users/peteo/GLD/gld_app")
```

or from PowerShell (Rscript is not on the PATH on this machine):

```powershell
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" -e "shiny::runApp('C:/Users/peteo/GLD/gld_app', port = 4322, launch.browser = TRUE)"
```

`ggrepel` is optional (nicer point labels in the lambda-space plots).

## Workflow (sidebar, top to bottom)

| Panel | Choices | Paper default |
|---|---|---|
| **Data** | example dataset or own file (csv / txt / xlsx / rds with a time-stamp column and price or return columns); series; prices → log / simple returns, or returns as supplied; date range | 5-min DJI30 prices → log returns |
| **Return filters** | remove overnight returns, remove zero returns, winsorise (remove or clip) at chosen quantiles, within each sub-sample or over the whole sample | remove overnight and zero returns, winsorise each month at 0.001 % / 99.999 % |
| **Sub-samples or rolling window** | scheme: non-overlapping sub-samples (monthly, weekly, quarterly, yearly, daily, whole sample; minimum returns per sub-sample) or rolling window (window length, step, one or more series, optional KS-D per window) | monthly / window 20, step 1 |
| **Estimation** | engine **GLDEX** (`fun.RMFMKL.mm`, `fun.RMFMKL.ml`, `fun.RMFMKL.qs`, starship – the functions used in the papers) or **gld** (`fit.fkml`: Mom, ML, MSP, TM, SM, Lmom, TL, DLA); seed; parallel workers | GLDEX MM + MLE |
| **Goodness-of-fit** | KS-R re-samples and sample share, significance level, KS-R threshold, rejection rule (KS-D p < α, KS-R below threshold, both, either), symmetry tolerance | 1 000 re-samples of 90 %, α = 5 %, rejection by KS-D |

Then **Fit GLDs!**. A monthly sub-sample of 5-minute returns (≈1 600 returns) takes about 1 s (MM) and
5 s (MLE) with GLDEX, or about 1.5 s with `gld::fit.fkml("ML")`. A 20-return rolling window costs about
1.5 s per fit with GLDEX MLE but only 0.2 s with gld MLE (0.02 s with gld MM), so a daily series of ten
years (≈2 500 windows) takes roughly one hour with GLDEX or eight minutes with gld on one core; the app
shows an estimate before it starts. Use the date range and the parallel workers to keep long samples
manageable.

## Output tabs

| Tab | Content | Paper |
|---|---|---|
| Data, Levels | the raw series | Figure 1 (supplement) |
| Transformed data | the returns of the selected series with the current settings, before or after the filters, available before any fitting | Figure 2 (supplement) |
| Returns | filtered returns and the filtering counts | Figure 2 (supplement) |
| Descriptive statistics | per sub-sample N, mean, variance, skewness, excess kurtosis, min, max, Jarque–Bera; sign-coded bar charts | Table 1 (supplement) |
| GLD estimates | value boxes per method and the full table: λ1–λ4, KS-R %, KS-D statistic and p-value, rejection flag, region, symmetry, theoretical GLD moments, support bounds, Freimer class | Table 1 |
| Rolling estimates | (rolling scheme) time series of the chosen parameters per series and method, summary, table, and the two CSV downloads (wide and long) | thesis Figure 5.7 |
| Fit diagnostics | histogram with the fitted densities and the normal; Q-Q plots; moment comparison for one sub-sample | Figures 2–3 |
| Parameters, Moments | time series of λ1–λ4 (rejections marked) and of descriptive vs fitted moments | Figures 4–5 |
| Lambda space | λ4 against λ3 per method, regions of support, symmetry diagonal, rejected (red ×) and symmetric (black ◆) fits, optional point labels and year filter | Figures 1, 6, 7 |
| Method comparison | λ3 and λ4 of one method against another | L3–L4 cross plots |
| Regions & biases | counts by region and the %-differences of fitted skewness and kurtosis vs the descriptive moments (Over / Under) | Table 2 |
| Goodness-of-fit | KS-R % and KS-D p-values over time | GoF plots |
| Published results | the same figures redrawn from the stored estimates of the paper (DJI30, 192 months 2001–2016, with the paper's rejection and symmetry flags) | Figures 4–7 |

**Download results** saves a zip with the CSV tables (estimates, descriptive statistics,
%-differences, region summary, filtered returns) and PNG figures.

## Example datasets

| Dataset | File | Content |
|---|---|---|
| S&P 500, 5-min | `spx_5min_2010_2020.rds` | 210 582 five-minute closes, Nov 2010 – Oct 2020 (`.SPX`) |
| REITs daily | `reits_daily_2013_2018.rds` | daily REIT index levels for Australia, Hong Kong, Japan and the US, Jul 2013 – Jul 2018 (from `REITs.xlsx`); the natural example for the rolling window |
| DJIA daily | `djia_daily_2000_2017.rds` | daily closes, Jan 2000 – Jun 2017 |
| Published DJI30 estimates | `published_dji30_2001_2016.rds` | monthly MM and MLE λ1–λ4, KS-R %, KS-D p (rejected months), symmetry flag, descriptive and fitted moments |

The example files are built by `tools/build_data.R` from `us_SP500.csv`, `REITs.xlsx`,
`DJI-daily.csv`, `coloured points.xlsx` and the working-paper tables in the project folder.

## Methods in brief

* **FMKL GLD**: Q(u) = λ1 + [(u^λ3 − 1)/λ3 − ((1 − u)^λ4 − 1)/λ4] / λ2, λ2 > 0; valid for all λ3, λ4.
  The k-th moment exists when λ3, λ4 > −1/k. Support is bounded below when λ3 > 0 and above when λ4 > 0,
  giving the four regions of Chalabi, Scott & Wuertz (2010, 2012).
* **MM** matches the first four sample moments (Karian & Dudewicz, 2000); **MLE** maximises the numerical
  log-likelihood after a Sobol-sequence search for starting values (Su, 2007).
* **KS-D** is the one-sample Kolmogorov–Smirnov test against the fitted GLD. **KS-R** (`GLDEX::fun.diag.ks.g`)
  is the share of random sub-samples for which the KS test does not reject.
* Fitted moments are the theoretical moments of the estimated GLD (`GLDEX::fun.theo.mv.gld`, excess
  kurtosis), compared with the descriptive moments (`GLDEX::fun.moments.r`).

## Software references

* King, R., Dean, B., Klinke, S., & van Staden, P. (2025). gld: Estimation and use of the generalised (Tukey) lambda distribution. R package 2.6.8.
* Su, S. (2007). Fitting single and mixture of generalized lambda distributions to data via discretized and maximum likelihood methods: GLDEX in R. *Journal of Statistical Software*, 21(9).
