# ===========================================================================
# GLD_FitExplore - Fitting the generalised lambda distribution to (high-frequency)
# financial returns: moment matching vs maximum likelihood, goodness-of-fit and
# the lambda-space view of Owusu Junior, Jeyasreedharan & Alagidede (2022).
# Author : Peterson Owusu Junior (peterson.owusu@ucc.edu.gh)
#
# Engines : gld   (King, Dean, Klinke & van Staden) - fit.fkml: ML, MPS, TM,
#                 starship, L-/TL-moments, DLA, method of moments
#           GLDEX (Su)                              - fun.RMFMKL.mm / .ml / .qs, starship
#                 (the functions used in the published papers)
# ===========================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
  library(gld)
  library(GLDEX)
  library(shinycssloaders)
})

if (!exists("fit_gld_blocks")) {
  for (f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) source(f, local = FALSE)
}

options(shiny.maxRequestSize = 100 * 1024^2, scipen = 999, spinner.type = 6, spinner.color = "#2a78d6")
invisible(Sys.setlocale("LC_TIME", "C"))

APP_NAME <- "GLD_FitExplore"
APP_AUTHOR <- list(
  name        = "Peterson Owusu Junior",
  affiliation = "University of Cape Coast, Ghana",
  email       = "peterson.owusu@ucc.edu.gh",
  emails      = c("peterson.owusu@ucc.edu.gh", "powusujunior@gmail.com"),
  website     = "https://positivecarryacademy.com/",
  app_url     = "https://01a160c2-0f18-1896-5a9f-de608cb2c2b2.share.connect.posit.cloud/",
  repo_url    = "https://github.com/bethpeniel/gld-fit-explore",
  links       = c("Website"        = "https://positivecarryacademy.com/",
                  "Google Scholar" = "https://scholar.google.com/citations?user=r6ZueU4AAAAJ&hl=en",
                  "Scopus"         = "https://www.scopus.com/authid/detail.uri?authorId=58833238800",
                  "LinkedIn"       = "https://www.linkedin.com/in/peterson-owusu-junior-phd-80858334/",
                  "ResearchGate"   = "https://www.researchgate.net/profile/Peterson-Owusu-Junior",
                  "IDEAS/RePEc"    = "https://ideas.repec.org/e/pow25.html")
)

BLOCK_TABS <- c("tab_returns", "tab_desc", "tab_est", "tab_diag", "tab_params", "tab_moments", "tab_space",
                "tab_cross", "tab_regions", "tab_gof")
ROLL_TABS  <- c("tab_returns", "tab_roll", "tab_space")
RESULT_TABS <- union(BLOCK_TABS, ROLL_TABS)
# rough cost of one fit (seconds) per engine, used for the time estimate shown before a rolling run
FIT_COST <- c(gldex_mm = 1.4, gldex_ml = 1.5, gldex_qs = 2, gldex_ss = 3, gld_Mom = 0.02, gld_ML = 0.2, gld_MSP = 0.3,
              gld_TM = 0.3, gld_SM = 2, gld_Lmom = 0.04, gld_TL = 0.04, gld_DLA = 0.3)
REJECT_CHOICES <- c("KS-D p-value below alpha (as reported in the paper)" = "ksd",
                    "KS-R share below the threshold" = "ksr",
                    "Both KS-D and KS-R reject" = "both",
                    "Either KS-D or KS-R rejects" = "either")
N_CORES <- max(1L, parallel::detectCores(logical = TRUE) - 1L)

plot_card <- function(id, height = "560px", ...) card(full_screen = TRUE, card_body(..., withSpinner(plotOutput(id, height = height))))
lam <- function(i) tags$span(class = "g-lambda", HTML(paste0("&lambda;<sub>", i, "</sub>")))

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = tags$span(tags$strong(APP_NAME),
                    tags$small(class = "text-muted ms-2", "Generalised lambda distribution fits of financial returns: moment matching vs maximum likelihood")),
  theme = bs_theme(version = 5, primary = "#2a78d6", secondary = "#52514e", success = "#006300", info = "#2a78d6",
                   warning = "#eda100", danger = "#d03b3b", bg = "#f9f9f7", fg = "#0b0b0b",
                   base_font = font_google("Source Sans 3", local = FALSE), heading_font = font_google("Source Sans 3", local = FALSE)),
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  fillable = FALSE,

  sidebar = sidebar(
    width = 390, open = "always",
    accordion(
      multiple = TRUE, open = c("Data", "Estimation"),
      accordion_panel(
        "Data", icon = icon("database"),
        radioButtons("source", NULL, c("Example dataset" = "rep", "Own dataset" = "own"), selected = "rep", inline = TRUE),
        conditionalPanel(
          "input.source == 'rep'",
          selectInput("rep_dataset", "Example dataset", choices = example_choices(), selected = "spx"),
          uiOutput("rep_note")
        ),
        conditionalPanel(
          "input.source == 'own'",
          fileInput("file", "Upload csv / txt / xlsx / rds", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".rds")),
          helpText("One row per observation: a date or date-time column plus one or more price (or return) columns. ",
                   "Separators and most time-stamp formats (e.g. 2010-11-01T08:30:00, 01/11/2010 08:30, 04-Jan-2001 09:30:00, Excel serials) are detected."),
          checkboxInput("has_header", "First row contains column names", value = TRUE),
          layout_columns(col_widths = c(6, 6),
                         selectInput("time_col", "Time column", choices = c("First column" = "")),
                         selectInput("time_format", "Time format", DATETIME_FORMAT_CHOICES, selected = "auto"))
        ),
        conditionalPanel("input.scheme != 'rolling'", selectInput("series", "Series", choices = NULL)),
        conditionalPanel("input.scheme == 'rolling'",
                         selectizeInput("roll_series", "Series (one or more)", choices = NULL, multiple = TRUE,
                                        options = list(plugins = list("remove_button")))),
        selectInput("kind", "Series contains", c("Prices - compute log returns" = "log", "Prices - compute simple returns" = "simple",
                                                 "Returns already" = "none"), selected = "log"),
        dateRangeInput("range", "Date range to analyse", start = NULL, end = NULL),
        helpText("Each monthly sub-sample of 5-min returns with MLE takes about 1-5 seconds; shorten the range for a quick first look.")
      ),
      accordion_panel(
        "Return filters", icon = icon("filter"),
        checkboxInput("overnight", "Remove overnight (between-session) returns", value = TRUE),
        checkboxInput("zeros", "Remove zero returns", value = TRUE),
        selectInput("winsor", "Winsorising", WINSOR_CHOICES, selected = "remove"),
        conditionalPanel(
          "input.winsor != 'none'",
          layout_columns(col_widths = c(6, 6),
                         numericInput("q_lo", "Lower quantile (%)", value = 0.001, min = 0, max = 10, step = 0.001),
                         numericInput("q_hi", "Upper quantile (%)", value = 99.999, min = 90, max = 100, step = 0.001)),
          radioButtons("winsor_scope", NULL, c("Quantiles within each sub-sample" = "block", "Quantiles over the whole sample" = "all"),
                       selected = "block")
        ),
        helpText("The paper removes overnight and zero returns and winsorises each monthly sub-sample at the 0.001% / 99.999% quantiles.")
      ),
      accordion_panel(
        "Sub-samples or rolling window", icon = icon("calendar-days"),
        radioButtons("scheme", "Sampling scheme",
                     c("Non-overlapping sub-samples (Cogent 2022 paper)" = "block",
                       "Rolling window (PhD thesis ch. 5; EBL 2020 paper)" = "rolling"), selected = "block"),
        conditionalPanel(
          "input.scheme != 'rolling'",
          selectInput("block", "Split the sample into", BLOCK_CHOICES, selected = "month"),
          numericInput("min_obs", "Minimum returns per sub-sample", value = 30, min = 10, step = 10)
        ),
        conditionalPanel(
          "input.scheme == 'rolling'",
          layout_columns(col_widths = c(6, 6),
                         numericInput("roll_window", "Window length (returns)", value = 20, min = 10, step = 1),
                         numericInput("roll_step", "Step (returns)", value = 1, min = 1, step = 1)),
          checkboxGroupInput("roll_params", "Series to plot", inline = TRUE,
                             choices = c("λ1" = "l1", "λ2" = "l2", "λ3" = "l3", "λ4" = "l4", "GLD skewness" = "skewness", "GLD excess kurtosis" = "kurtosis"),
                             selected = c("l3", "l4")),
          checkboxInput("roll_ksd", "Also compute the KS-D test for every window (slower)", value = FALSE),
          helpText("The thesis fits the FKML GLD by MLE to daily log returns in windows of 20 trading days (one month), ",
                   "moved one day at a time, giving k = N - m + 1 daily series of λ3 (skewness) and λ4 (kurtosis). ",
                   "The resulting series can be downloaded from the 'Rolling estimates' tab. The KS-R test is not run in this scheme.")
        )
      ),
      accordion_panel(
        "Estimation", icon = icon("sliders"),
        radioButtons("engine", "Fitting engine",
                     c("GLDEX (Su 2007) - functions used in the papers" = "gldex",
                       "gld (King et al.) - fit.fkml, more methods" = "gld"), selected = "gldex"),
        checkboxGroupInput("methods", "Estimation methods (FMKL parameterisation)", choices = method_choices("gldex"),
                           selected = c("gldex_mm", "gldex_ml")),
        conditionalPanel("input.methods && input.methods.indexOf('gld_TL') > -1",
                         layout_columns(col_widths = c(6, 6),
                                        numericInput("t1", "TL-moments: trim left", value = 1, min = 0, step = 1),
                                        numericInput("t2", "TL-moments: trim right", value = 1, min = 0, step = 1))),
        layout_columns(col_widths = c(6, 6),
                       numericInput("seed", "Random seed", value = 1000, min = 1, step = 1),
                       numericInput("workers", sprintf("Parallel workers (max %d)", N_CORES), value = 1, min = 1, max = N_CORES, step = 1)),
        helpText("MM and MLE from GLDEX reproduce the papers (fun.RMFMKL.mm / fun.RMFMKL.ml). The gld package adds maximum product of spacings, ",
                 "Titterington, starship, L-/TL-moments and distributional least absolutes.")
      ),
      accordion_panel(
        "Goodness-of-fit", icon = icon("check-double"),
        layout_columns(col_widths = c(6, 6),
                       numericInput("ks_tests", "KS-R re-samples (0 = skip)", value = 1000, min = 0, max = 5000, step = 100),
                       numericInput("ks_prop", "KS-R sample share", value = 0.9, min = 0.1, max = 1, step = 0.05)),
        layout_columns(col_widths = c(6, 6),
                       numericInput("alpha", "Significance level", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
                       numericInput("ksr_threshold", "KS-R threshold (%)", value = 70, min = 0, max = 100, step = 5)),
        selectInput("reject_rule", "A fit is rejected when", REJECT_CHOICES, selected = "ksd"),
        numericInput("sym_tol", "Symmetry tolerance |λ3 - λ4| <", value = 1e-4, min = 0, step = 1e-4),
        helpText("KS-D: one-sample Kolmogorov-Smirnov distance test against the fitted GLD. KS-R: share of re-samples (Su 2007) ",
                 "for which the KS test does not reject at the chosen level.")
      )
    ),
    actionButton("run", "Fit GLDs!", class = "btn-primary w-100 mt-2", icon = icon("play")),
    downloadButton("download", "Download results", class = "btn-outline-secondary w-100 mt-2"),
    uiOutput("status")
  ),

  uiOutput("data_info"),

  navset_card_underline(
    id = "tabs",
    nav_panel("Data", value = "tab_data", withSpinner(DTOutput("data_table"))),
    nav_panel("Levels", value = "tab_levels", withSpinner(plotOutput("levels_plot", height = "560px"))),
    nav_panel("Transformed data", value = "tab_trans",
              layout_columns(col_widths = c(7, 5),
                             radioButtons("trans_stage", NULL, inline = TRUE,
                                          c("Returns as transformed" = "raw", "Returns after the sidebar filters" = "filtered"), selected = "filtered"),
                             uiOutput("trans_summary")),
              withSpinner(plotOutput("trans_plot", height = "560px")),
              tags$p(class = "g-note mt-2", "The series as it enters the estimation: the return transformation chosen under 'Series contains', over the selected date range, ",
                     "before or after removing overnight and zero returns and winsorising (quantiles taken over the whole range here). Updates with the sidebar settings without fitting.")),
    nav_panel("Returns", value = "tab_returns",
              uiOutput("filter_summary"),
              withSpinner(plotOutput("returns_plot", height = "520px"))),
    nav_panel("Descriptive statistics", value = "tab_desc",
              withSpinner(DTOutput("desc_table")),
              tags$p(class = "g-note mt-2", "Moments as in the papers (GLDEX::fun.moments.r, normalised): variance is the population variance, kurtosis is excess kurtosis. JB = Jarque-Bera statistic."),
              withSpinner(plotOutput("desc_plot", height = "720px"))),
    nav_panel("GLD estimates", value = "tab_est",
              uiOutput("overview_boxes"),
              withSpinner(DTOutput("est_table")),
              tags$p(class = "g-note mt-2", HTML("Table 1 of the paper, for every sub-sample and method: FMKL parameters, goodness-of-fit (KS-R %, KS-D statistic and p-value), support region, theoretical GLD moments (excess kurtosis) and the support bounds. Rejected fits are marked in red."))),
    nav_panel("Rolling estimates", value = "tab_roll",
              uiOutput("roll_summary"),
              withSpinner(plotOutput("roll_plot", height = "700px")),
              layout_columns(col_widths = c(3, 3, 6),
                             downloadButton("roll_download_wide", "Download series (wide CSV)", class = "btn-outline-secondary w-100"),
                             downloadButton("roll_download_long", "Download estimates (long CSV)", class = "btn-outline-secondary w-100"),
                             tags$p(class = "g-note mb-0", "Wide: one row per window end date, one column per series and parameter (e.g. US_L3), ready for connectedness or wavelet tools. ",
                                    "Long: one row per series, window and method with all parameters, GLD moments, region and timing.")),
              h5(class = "mt-3", "Rolling-window estimates"),
              withSpinner(DTOutput("roll_table"))),
    nav_panel("Fit diagnostics", value = "tab_diag",
              uiOutput("diag_controls"),
              layout_columns(col_widths = c(6, 6),
                             withSpinner(plotOutput("diag_hist", height = "480px")),
                             withSpinner(plotOutput("diag_qq", height = "480px"))),
              withSpinner(DTOutput("diag_table")),
              tags$p(class = "g-note mt-2", "Histogram (Freedman-Diaconis bins) with the fitted GLD densities and the normal density with the same mean and variance; Q-Q plots of the sample against each fitted GLD (Figures 2-3 of the paper).")),
    nav_panel("Parameters", value = "tab_params", withSpinner(plotOutput("params_plot", height = "820px"))),
    nav_panel("Moments", value = "tab_moments", withSpinner(plotOutput("moments_plot", height = "820px"))),
    nav_panel("Lambda space", value = "tab_space",
              uiOutput("space_controls"),
              uiOutput("space_plot_ui")),
    nav_panel("Method comparison", value = "tab_cross",
              uiOutput("cross_controls"),
              withSpinner(plotOutput("cross_plot", height = "520px"))),
    nav_panel("Regions & biases", value = "tab_regions",
              layout_columns(col_widths = c(5, 7),
                             withSpinner(plotOutput("region_plot", height = "420px")),
                             withSpinner(DTOutput("region_table"))),
              h5(class = "mt-3", "%-differences of fitted skewness and kurtosis relative to the descriptive moments"),
              withSpinner(DTOutput("bias_table")),
              tags$p(class = "g-note mt-2", "Table 2 of the paper: Over = the fitted GLD moment exceeds the sample moment, Under = it falls short.")),
    nav_panel("Goodness-of-fit", value = "tab_gof",
              withSpinner(plotOutput("gof_plot", height = "600px")),
              withSpinner(DTOutput("gof_table"))),
    nav_panel("Published results", value = "tab_pub", icon = icon("book-open"),
              layout_columns(col_widths = c(7, 5),
                             selectInput("pub_dataset", "Stored estimates", choices = published_choices(), selected = "dji30", width = "100%"),
                             selectizeInput("pub_years", "Years shown in the lambda-space plot (blank = all)", choices = NULL, multiple = TRUE, width = "100%")),
              uiOutput("pub_info"),
              navset_underline(
                nav_panel("Lambda space", withSpinner(plotOutput("pub_space", height = "600px"))),
                nav_panel("Parameters", withSpinner(plotOutput("pub_params", height = "820px"))),
                nav_panel("Moments", withSpinner(plotOutput("pub_moments", height = "820px"))),
                nav_panel("MLE vs MM", withSpinner(plotOutput("pub_cross", height = "520px"))),
                nav_panel("Regions", withSpinner(plotOutput("pub_regions", height = "420px"))),
                nav_panel("Goodness-of-fit", withSpinner(plotOutput("pub_gof", height = "420px"))),
                nav_panel("Table", withSpinner(DTOutput("pub_table")))
              )),
    nav_spacer(),
    nav_panel("About", value = "tab_about", icon = icon("circle-info"), uiOutput("about"))
  ),

  tags$footer(class = "text-muted small mt-3",
              HTML(sprintf('<strong>%s</strong> by <a href="mailto:%s">%s</a>, %s (<a href="%s" target="_blank">%s</a>). ',
                           APP_NAME, APP_AUTHOR$email, APP_AUTHOR$name, APP_AUTHOR$affiliation,
                           APP_AUTHOR$website, sub("^https?://", "", sub("/$", "", APP_AUTHOR$website)))),
              "Built with R Shiny, gld and GLDEX. Companion to the author's GLD studies: Owusu Junior, Tweneboah, Ijasan & Jeyasreedharan (2019, J. European Real Estate Research); ",
              "Owusu Junior (2020, PhD thesis, University of the Witwatersrand); Owusu Junior, Alagidede & Tweneboah (2020, Economics and Business Letters); ",
              "Owusu Junior, Jeyasreedharan & Alagidede (2022, Cogent Economics & Finance).")
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- data --------------------------------------------------------------
  file_df <- reactive({
    req(input$file)
    tryCatch(read_user_file(input$file$datapath, input$file$name, header = isTRUE(input$has_header)),
             error = function(e) { showNotification(paste("Could not read file:", conditionMessage(e)), type = "error", duration = 10); NULL })
  })

  observeEvent(file_df(), {
    df <- file_df(); req(df)
    cols <- names(df)
    updateSelectInput(session, "time_col", choices = stats::setNames(cols, cols), selected = cols[1])
  })

  output$rep_note <- renderUI({ req(input$rep_dataset); helpText(EXAMPLES[[input$rep_dataset]]$note) })

  raw <- reactive({
    if (input$source == "rep") {
      ex <- EXAMPLES[[input$rep_dataset]]
      prepare_dataset(readRDS(ex$file), "time")
    } else {
      df <- file_df(); req(df)
      tryCatch(prepare_dataset(df, input$time_col, input$time_format %||% "auto"),
               error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL })
    }
  })

  observeEvent(raw(), {
    r <- raw(); req(r)
    vars <- names(r$values)
    updateSelectInput(session, "series", choices = vars, selected = vars[1])
    updateSelectizeInput(session, "roll_series", choices = vars, selected = vars[seq_len(min(4, length(vars)))])
    rng <- as.Date(range(r$time))
    if (input$source == "rep") {
      ex <- EXAMPLES[[input$rep_dataset]]
      updateDateRangeInput(session, "range", start = max(rng[1], as.Date(ex$range[1])), end = min(rng[2], as.Date(ex$range[2])), min = rng[1], max = rng[2])
      updateSelectInput(session, "block", selected = ex$block)
      updateSelectInput(session, "kind", selected = ex$kind)
      updateRadioButtons(session, "scheme", selected = ex$scheme %||% "block")
    } else {
      updateDateRangeInput(session, "range", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
    }
  })

  observeEvent(input$engine, {
    ch <- method_choices(input$engine)
    sel <- if (input$engine == "gldex") c("gldex_mm", "gldex_ml") else c("gld_Mom", "gld_ML")
    updateCheckboxGroupInput(session, "methods", choices = ch, selected = sel)
  })

  output$data_info <- renderUI({
    if (input$source == "own" && is.null(input$file))
      return(div(class = "alert alert-info small py-2 mb-3", icon("upload"),
                 " Upload a file in the sidebar (or pick an example dataset), choose the filters, sub-sample length and methods, then click ",
                 tags$b("Fit GLDs!"), ". The ", tags$b("Published results"), " tab shows the stored estimates of the papers without any computation."))
    r <- raw(); req(r)
    div(class = "alert alert-light border small py-2 mb-3",
        tags$strong(sprintf("%s observations, %d series", format(length(r$time), big.mark = ","), ncol(r$values))),
        sprintf(" | %s to %s | %s | ", format(min(r$time)), format(max(r$time)), if (r$intraday) "intraday" else "daily or lower frequency"),
        paste(head(names(r$values), 12), collapse = ", "), if (ncol(r$values) > 12) ", ...",
        if (length(r$note)) tags$div(class = "text-warning mt-1", paste(r$note, collapse = " ")))
  })

  # ---- tab visibility ----------------------------------------------------
  observe({ for (t in RESULT_TABS) nav_hide("tabs", t) })
  observeEvent(res(), {
    show <- if (res()$mode == "rolling") ROLL_TABS else BLOCK_TABS
    for (t in RESULT_TABS) if (t %in% show) nav_show("tabs", t) else nav_hide("tabs", t)
    nav_select("tabs", if (res()$mode == "rolling") "tab_roll" else "tab_est")
  })
  blk  <- reactive({ r <- res(); validate(need(r$mode == "block", "Run the non-overlapping sub-sample scheme to see this tab.")); r })
  roll <- reactive({ r <- res(); validate(need(r$mode == "rolling", "Run the rolling-window scheme to see this tab.")); r })

  # ---- common outputs (before the run) -----------------------------------
  output$data_table <- renderDT({
    r <- raw(); req(r)
    df <- cbind(time = format(r$time), r$values)
    datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE), class = "compact stripe") |>
      formatRound(names(r$values), 4)
  })
  output$levels_plot <- renderPlot({
    r <- raw(); req(r)
    sel <- if (identical(input$scheme, "rolling")) intersect(input$roll_series, names(r$values)) else input$series
    validate(need(length(sel) >= 1 && all(sel %in% names(r$values)), "Please choose a series."))
    keep <- rep(TRUE, length(r$time))
    if (!any(is.na(input$range))) keep <- as.Date(r$time) >= input$range[1] & as.Date(r$time) <= input$range[2]
    validate(need(any(keep), "No observations in the selected date range."))
    plot_levels(r$time[keep], r$values[sel][keep, , drop = FALSE], sel,
                sprintf("%s: %s", paste(sel, collapse = ", "), if (input$kind == "none") "returns as supplied" else "price levels"))
  }, res = 100)

  # ---- transformed data (before any fitting) -----------------------------
  trans <- reactive({
    r <- raw(); req(r)
    sel <- if (identical(input$scheme, "rolling")) intersect(input$roll_series, names(r$values)) else input$series
    validate(need(length(sel) >= 1 && all(sel %in% names(r$values)), "Please choose a series."))
    filtered <- identical(input$trans_stage, "filtered")
    parts <- lapply(sel, function(v) {
      rr <- tryCatch(build_returns(r$time, r$values[[v]], series = v, kind = input$kind,
                                   remove_overnight = filtered && isTRUE(input$overnight), remove_zero = filtered && isTRUE(input$zeros),
                                   block = "full", date_range = input$range, winsor = if (filtered) input$winsor else "none",
                                   q_lo = input$q_lo / 100, q_hi = input$q_hi / 100, winsor_scope = "all", min_obs = 10),
                     error = function(e) NULL)
      if (is.null(rr)) return(NULL)
      list(data = cbind(rr$data[, c("time", "ret")], series = v), counts = rr$counts)
    })
    parts <- Filter(Negate(is.null), parts)
    validate(need(length(parts) >= 1, "The returns could not be computed for the selected series and range (prices must be positive and the range non-empty)."))
    data <- do.call(rbind, lapply(parts, `[[`, "data"))
    counts <- Reduce(function(a, b) Map(`+`, a, b), lapply(parts, `[[`, "counts"))
    list(data = data, counts = counts, series = sel, filtered = filtered)
  })
  output$trans_summary <- renderUI({
    t <- trans(); cnt <- t$counts
    kv <- function(k, v) tags$span(class = "g-kv", k, ": ", tags$b(format(v, big.mark = ",")))
    div(class = "small text-muted",
        kv("Observations", cnt$prices), kv("Returns", cnt$returns),
        if (t$filtered) tagList(kv("Overnight removed", cnt$overnight), kv("Zeros removed", cnt$zeros), kv("Winsorised out", cnt$winsorised)),
        kv("Plotted", nrow(t$data)))
  })
  output$trans_plot <- renderPlot({
    t <- trans()
    plot_returns(t$data, paste(t$series, collapse = ", "), input$kind,
                 title = sprintf("%s: %s%s", paste(t$series, collapse = ", "),
                                 switch(input$kind, log = "log returns", simple = "simple returns", "returns as supplied"),
                                 if (t$filtered) " after the filters" else " before the filters"))
  }, res = 100)

  # ---- analysis ----------------------------------------------------------
  res <- eventReactive(input$run, {
    r <- raw()
    validate(need(!is.null(r), "Please load a dataset first."),
             need(length(input$methods) >= 1, "Please select at least one estimation method."))
    if (identical(input$scheme, "rolling")) return(run_rolling(r))
    validate(need(input$series %in% names(r$values), "Please choose a series."))
    t0 <- Sys.time()
    withProgress(message = "Fitting generalised lambda distributions", value = 0, {
      incProgress(0.02, detail = "Building the return series")
      rr <- tryCatch(build_returns(r$time, r$values[[input$series]], series = input$series, kind = input$kind,
                                   remove_overnight = isTRUE(input$overnight), remove_zero = isTRUE(input$zeros),
                                   block = input$block, date_range = input$range, winsor = input$winsor,
                                   q_lo = input$q_lo / 100, q_hi = input$q_hi / 100, winsor_scope = input$winsor_scope,
                                   min_obs = max(10, input$min_obs)),
                     error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
      validate(need(!is.null(rr), "The return series could not be built."))
      if (length(rr$note)) showNotification(paste(rr$note, collapse = " "), type = "warning", duration = 12)
      desc <- summary_by_block(rr$data, input$block)
      desc$label <- period_pretty(desc$period, input$block)
      n_blocks <- length(unique(rr$data$period))
      fits <- fit_gld_blocks(rr$data, input$block, input$methods, seed = input$seed, ks_tests = max(0, input$ks_tests),
                             ks_prop = input$ks_prop, alpha = input$alpha, reject_rule = input$reject_rule,
                             ksr_threshold = input$ksr_threshold, sym_tol = input$sym_tol, t1 = input$t1 %||% 0, t2 = input$t2 %||% 0,
                             workers = max(1, min(N_CORES, input$workers)),
                             progress = function(v, d) setProgress(0.05 + 0.93 * v, detail = d))
      failed <- fits[!is.na(fits$error), ]
      if (nrow(failed)) showNotification(sprintf("%d fit(s) failed, e.g. %s (%s): %s", nrow(failed), failed$label[1], failed$method[1], failed$error[1]),
                                         type = "warning", duration = 15)
      list(mode = "block", raw = r, rets = rr, desc = desc, fits = fits, block = input$block, series = input$series, kind = input$kind,
           methods = levels(fits$method), engine = input$engine, alpha = input$alpha, ksr_threshold = input$ksr_threshold,
           reject_rule = input$reject_rule, n_blocks = n_blocks,
           elapsed = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))
    })
  })

  # rolling-window scheme (thesis chapter 5): one fit per window of `roll_window` returns, moved by `roll_step`
  run_rolling <- function(r) {
    sel <- intersect(input$roll_series, names(r$values))
    validate(need(length(sel) >= 1, "Please choose at least one series for the rolling window."))
    w <- max(10, as.integer(input$roll_window)); s <- max(1, as.integer(input$roll_step))
    t0 <- Sys.time()
    withProgress(message = "Rolling-window GLD estimation", value = 0, {
      incProgress(0.02, detail = "Building the return series")
      series_list <- list(); counts <- list(prices = 0, returns = 0, overnight = 0, zeros = 0, winsorised = 0, final = 0); notes <- character(0)
      for (v in sel) {
        rr <- tryCatch(build_returns(r$time, r$values[[v]], series = v, kind = input$kind,
                                     remove_overnight = isTRUE(input$overnight), remove_zero = isTRUE(input$zeros),
                                     block = "full", date_range = input$range, winsor = input$winsor,
                                     q_lo = input$q_lo / 100, q_hi = input$q_hi / 100, winsor_scope = "all", min_obs = w),
                       error = function(e) { showNotification(sprintf("%s: %s", v, conditionMessage(e)), type = "error", duration = 12); NULL })
        if (is.null(rr)) next
        series_list[[v]] <- rr$data[, c("time", "ret")]
        for (k in names(counts)) counts[[k]] <- counts[[k]] + (rr$counts[[k]] %||% 0)
        notes <- c(notes, rr$note)
      }
      validate(need(length(series_list) >= 1, "No series could be turned into returns."))
      n_fits <- sum(vapply(series_list, function(d) length(seq(w, nrow(d), by = s)), numeric(1))) * length(input$methods)
      est <- sum(FIT_COST[input$methods], na.rm = TRUE) / length(input$methods) * n_fits / max(1, min(N_CORES, input$workers))
      showNotification(sprintf("%s window fits to run; rough estimate %s min with %d worker(s).", format(n_fits, big.mark = ","),
                               format(round(est / 60, 1)), max(1, min(N_CORES, input$workers))), type = "message", duration = 10)
      if (length(unique(notes))) showNotification(paste(unique(notes), collapse = " "), type = "warning", duration = 12)
      fits <- fit_gld_rolling(series_list, window = w, step = s, methods = input$methods, seed = input$seed,
                              ks_d = isTRUE(input$roll_ksd), sym_tol = input$sym_tol, t1 = input$t1 %||% 0, t2 = input$t2 %||% 0,
                              workers = max(1, min(N_CORES, input$workers)),
                              progress = function(v, d) setProgress(0.05 + 0.93 * v, detail = d))
      failed <- fits[!is.na(fits$error), ]
      if (nrow(failed)) showNotification(sprintf("%d window fit(s) failed, e.g. %s %s: %s", nrow(failed), failed$series[1], format(failed$time[1]), failed$error[1]),
                                         type = "warning", duration = 15)
      fits$panel <- paste(fits$series, fits$method, sep = " · ")
      rets <- do.call(rbind, lapply(names(series_list), function(v) cbind(series_list[[v]], series = v)))
      rets$period <- "Whole sample"
      list(mode = "rolling", raw = r, rets = list(data = rets, counts = counts, note = notes), fits = fits, wide = rolling_wide(fits),
           window = w, step = s, series = names(series_list), kind = input$kind, methods = levels(fits$method), engine = input$engine,
           block = "full", n_blocks = length(unique(fits$index)), ks_d = isTRUE(input$roll_ksd),
           elapsed = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))
    })
  }

  output$status <- renderUI({
    r <- res()
    if (r$mode == "rolling")
      div(class = "small text-success mt-2", icon("check"),
          sprintf(" %s window fits (%d series x %d methods, window %d, step %d) in %s s.",
                  format(nrow(r$fits), big.mark = ","), length(r$series), length(r$methods), r$window, r$step, r$elapsed))
    else
      div(class = "small text-success mt-2", icon("check"),
          sprintf(" %d sub-samples x %d methods fitted in %s s (%s returns after filtering).",
                  r$n_blocks, length(r$methods), r$elapsed, format(nrow(r$rets$data), big.mark = ",")))
  })

  # ---- rolling outputs ---------------------------------------------------
  output$roll_summary <- renderUI({
    r <- roll(); ov <- rolling_overview(r$fits)
    kv <- function(k, v) tags$span(class = "g-kv", k, ": ", tags$b(v))
    tagList(
      div(class = "alert alert-light border small py-2 mb-2",
          kv("Window", sprintf("%d returns", r$window)), kv("Step", r$step), kv("Windows per series", format(r$n_blocks, big.mark = ",")),
          kv("Series", paste(r$series, collapse = ", ")), kv("Methods", paste(r$methods, collapse = ", ")),
          kv("Failed fits", sum(ov$failed)), kv("Fits in Region 4", sprintf("%.0f%%", 100 * sum(ov$region4) / max(1, nrow(r$fits))))),
      tags$p(class = "g-note", "Daily rolling estimates of λ3 (skewness) and λ4 (kurtosis) are the inputs of the connectedness analysis in chapter 5 of the thesis ",
             "and in Owusu Junior, Alagidede & Tweneboah (2020); those connectedness measures are outside the scope of this app."))
  })
  output$roll_plot <- renderPlot({
    r <- roll(); pars <- input$roll_params; if (!length(pars)) pars <- c("l3", "l4")
    plot_rolling_series(r$fits, pars, title = sprintf("Rolling-window GLD estimates (window = %d, step = %d)", r$window, r$step))
  }, res = 100)
  output$roll_table <- renderDT({
    r <- roll()
    d <- r$fits[, c("series", "time", "window_start", "method", "n", "l1", "l2", "l3", "l4", "skewness", "kurtosis", "region_name", "ksd_p", "secs")]
    d$time <- format(d$time); d$window_start <- format(d$window_start)
    names(d) <- c("Series", "Window end", "Window start", "Method", "N", "λ1", "λ2", "λ3", "λ4", "GLD skewness", "GLD excess kurtosis", "Region", "KS-D p", "Seconds")
    datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE), class = "compact stripe") |>
      formatSignif("λ1", 4) |> formatRound(c("λ2", "λ3", "λ4"), 5) |> formatRound(c("GLD skewness", "GLD excess kurtosis", "KS-D p"), 3) |> formatRound("Seconds", 2)
  })
  output$roll_download_wide <- downloadHandler(
    filename = function() paste0("gld_rolling_series_wide_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content = function(file) { r <- roll(); w <- r$wide; w$date <- format(w$date); utils::write.csv(w, file, row.names = FALSE) })
  output$roll_download_long <- downloadHandler(
    filename = function() paste0("gld_rolling_estimates_long_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content = function(file) { r <- roll(); utils::write.csv(r$fits, file, row.names = FALSE) })

  # ---- returns & descriptive --------------------------------------------
  output$filter_summary <- renderUI({
    r <- res(); cnt <- r$rets$counts
    kv <- function(k, v) tags$span(class = "g-kv", k, ": ", tags$b(format(v, big.mark = ",")))
    div(class = "alert alert-light border small py-2 mb-2",
        kv("Observations in range", cnt$prices), kv("Returns", cnt$returns), kv("Overnight removed", cnt$overnight),
        kv("Zero returns removed", cnt$zeros), kv("Winsorised out", cnt$winsorised), kv("Returns analysed", cnt$final),
        if (r$mode == "rolling") kv("Windows per series", r$n_blocks) else kv("Sub-samples", r$n_blocks))
  })
  output$returns_plot <- renderPlot({
    r <- res()
    plot_returns(r$rets$data, paste(r$series, collapse = ", "), r$kind)
  }, res = 100)

  output$desc_table <- renderDT({
    r <- blk(); d <- r$desc[, c("label", "n", "mean", "variance", "sd", "skewness", "kurtosis", "min", "max", "jb", "jb_p")]
    names(d) <- c("Sub-sample", "N", "Mean", "Variance", "Std. dev.", "Skewness", "Excess kurtosis", "Min", "Max", "JB", "JB p-value")
    datatable(d, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE), class = "compact stripe") |>
      formatSignif(c("Mean", "Variance", "Std. dev.", "Min", "Max"), 4) |> formatRound(c("Skewness", "Excess kurtosis", "JB"), 3) |> formatRound("JB p-value", 4)
  })
  output$desc_plot <- renderPlot({ r <- blk(); plot_desc_moments(r$desc, r$block) }, res = 100)

  # ---- estimates ---------------------------------------------------------
  output$overview_boxes <- renderUI({
    r <- blk(); ov <- method_overview(r$fits)
    boxes <- lapply(seq_len(nrow(ov)), function(i) {
      o <- ov[i, ]
      value_box(title = sprintf("%s: rejected fits", o$method), value = sprintf("%d of %d", o$rejected, o$fits),
                showcase = icon(if (o$rejected == 0) "circle-check" else "triangle-exclamation"),
                theme = value_box_theme(bg = if (o$rejected == 0) "#e6f2ea" else "#fde5db", fg = "#0b0b0b"),
                p(class = "small mb-0", sprintf("Region 4: %d · symmetric: %d · median KS-R: %s%% · failed: %d",
                                                o$region4, o$symmetric, ifelse(is.na(o$median_ksr), "n/a", round(o$median_ksr, 1)), o$failed)))
    })
    layout_columns(col_widths = rep(12 %/% max(1, min(4, nrow(ov))), nrow(ov)), !!!boxes)
  })

  est_display <- function(f) {
    d <- f[, c("label", "method", "n", "l1", "l2", "l3", "l4", "ksr_pct", "ksd_D", "ksd_p", "rejected", "region_name", "symmetric",
               "mean", "variance", "skewness", "kurtosis", "support_lower", "support_upper", "class")]
    names(d) <- c("Sub-sample", "Method", "N", "λ1", "λ2", "λ3", "λ4", "KS-R (%)", "KS-D", "KS-D p", "Rejected", "Region", "Symmetric",
                  "Mean", "Variance", "Skewness", "Excess kurtosis", "Support lower", "Support upper", "Class")
    d
  }
  est_dt <- function(d) {
    datatable(d, rownames = FALSE, filter = "top", extensions = "Buttons",
              options = list(pageLength = 12, scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv")), class = "compact stripe") |>
      formatSignif(c("λ1", "Mean", "Variance", "Support lower", "Support upper"), 4) |>
      formatRound(c("λ2", "λ3", "λ4"), 5) |> formatRound(c("KS-R (%)", "Skewness", "Excess kurtosis"), 2) |>
      formatRound(c("KS-D", "KS-D p"), 3) |>
      formatStyle("Rejected", target = "row", color = styleEqual(TRUE, "#d03b3b"), fontWeight = styleEqual(TRUE, "bold"))
  }
  output$est_table <- renderDT({ est_dt(est_display(blk()$fits)) })

  # ---- fit diagnostics ---------------------------------------------------
  output$diag_controls <- renderUI({
    r <- blk(); per <- unique(r$fits$period)
    choices <- stats::setNames(per, period_pretty(per, r$block))
    worst <- r$fits$period[which.min(r$fits$ksr_pct)]
    layout_columns(col_widths = c(5, 4, 3),
                   selectInput("diag_period", "Sub-sample", choices = choices, selected = if (length(worst)) worst else per[1]),
                   checkboxGroupInput("diag_methods", "Methods", choices = r$methods, selected = r$methods, inline = TRUE),
                   numericInput("diag_bins", "Histogram bins (blank = Freedman-Diaconis)", value = NA, min = 5, step = 5))
  })
  diag_fits <- reactive({
    r <- blk(); req(input$diag_period)
    f <- r$fits[r$fits$period == input$diag_period & r$fits$method %in% (input$diag_methods %||% r$methods) & !is.na(r$fits$l1), ]
    validate(need(nrow(f) > 0, "No successful fit for this sub-sample and method selection."))
    fits <- lapply(seq_len(nrow(f)), function(i) c(f$l1[i], f$l2[i], f$l3[i], f$l4[i]))
    names(fits) <- as.character(f$method)
    list(x = r$rets$data$ret[r$rets$data$period == input$diag_period], fits = fits, rows = f, label = period_pretty(input$diag_period, r$block))
  })
  output$diag_hist <- renderPlot({
    d <- diag_fits(); bins <- if (is.na(input$diag_bins)) NULL else input$diag_bins
    plot_fit_hist(d$x, d$fits, bins = bins, title = sprintf("%s: histogram and fitted GLD densities (n = %d)", d$label, length(d$x)))
  }, res = 100)
  output$diag_qq <- renderPlot({ d <- diag_fits(); plot_fit_qq(d$x, d$fits, title = sprintf("%s: Q-Q plots", d$label)) }, res = 100)
  output$diag_table <- renderDT({
    d <- diag_fits(); f <- d$rows; m <- desc_moments(d$x)
    tab <- data.frame(Method = c("Descriptive", as.character(f$method)),
                      `λ1` = c(NA, f$l1), `λ2` = c(NA, f$l2), `λ3` = c(NA, f$l3), `λ4` = c(NA, f$l4),
                      `KS-R (%)` = c(NA, f$ksr_pct), `KS-D p` = c(NA, f$ksd_p), Region = c(NA, f$region_name),
                      Mean = c(m[["mean"]], f$mean), Variance = c(m[["variance"]], f$variance),
                      Skewness = c(m[["skewness"]], f$skewness), `Excess kurtosis` = c(m[["kurtosis"]], f$kurtosis), check.names = FALSE)
    datatable(tab, rownames = FALSE, options = list(dom = "t"), class = "compact stripe") |>
      formatSignif(c("λ1", "Mean", "Variance"), 4) |> formatRound(c("λ2", "λ3", "λ4"), 5) |> formatRound(c("KS-R (%)", "Skewness", "Excess kurtosis"), 3) |> formatRound("KS-D p", 3)
  })

  # ---- time-series figures ----------------------------------------------
  output$params_plot <- renderPlot({ r <- blk(); plot_lambda_series(r$fits, r$block) }, res = 100)
  output$moments_plot <- renderPlot({ r <- blk(); plot_moment_series(r$fits, r$desc, r$block) }, res = 100)

  # ---- lambda space ------------------------------------------------------
  years_of <- function(f) sort(unique(format(f$start, "%Y")))
  output$space_controls <- renderUI({
    r <- res()
    dates <- if (r$mode == "rolling") r$fits$time else r$fits$start
    layout_columns(col_widths = c(6, 3, 3),
                   selectizeInput("space_years", "Years shown (blank = all)", choices = sort(unique(format(dates, "%Y"))), multiple = TRUE),
                   checkboxInput("space_labels", "Label the points (up to 60)", value = r$mode == "block" && r$n_blocks <= 24),
                   checkboxInput("space_common", "Same axis limits for all panels", value = TRUE))
  })
  output$space_plot_ui <- renderUI({
    r <- res()
    h <- if (r$mode == "rolling") 150 + 300 * length(r$methods) else if (length(r$methods) <= 2) 620 else 320 * ceiling(length(r$methods) / 3) + 120
    withSpinner(plotOutput("space_plot", height = paste0(h, "px")))
  })
  space_data <- function(f, years, mode = "block") {
    if (length(years)) { d <- if (mode == "rolling") f$time else f$start; f <- f[format(d, "%Y") %in% years, ] }
    f
  }
  output$space_plot <- renderPlot({
    r <- res(); f <- space_data(r$fits, input$space_years, r$mode)
    validate(need(nrow(f) > 0, "No fits for the selected years."))
    yrs <- if (length(input$space_years)) paste(input$space_years, collapse = ", ") else if (r$mode == "rolling") "all windows" else "all sub-samples"
    if (r$mode == "rolling")
      plot_lambda_space(f, label_points = FALSE, common_limits = !isFALSE(input$space_common), facet = "panel",
                        title = sprintf("λ3-λ4 space of the rolling-window GLD fits (%s)", yrs))
    else
      plot_lambda_space(f, label_points = isTRUE(input$space_labels), common_limits = !isFALSE(input$space_common),
                        title = sprintf("λ3-λ4 space of the fitted GLDs (%s)", yrs), ncol = if (length(r$methods) <= 2) 2 else 3)
  }, res = 100)

  # ---- method comparison -------------------------------------------------
  output$cross_controls <- renderUI({
    r <- blk(); validate(need(length(r$methods) >= 2, "Select at least two estimation methods to compare them."))
    layout_columns(col_widths = c(4, 4, 4),
                   selectInput("cross_m1", "Reference method (x)", choices = r$methods, selected = r$methods[1]),
                   selectInput("cross_m2", "Compared method (y)", choices = r$methods, selected = r$methods[2]),
                   checkboxInput("cross_labels", "Label the points (up to 60)", value = r$n_blocks <= 24))
  })
  output$cross_plot <- renderPlot({
    r <- blk(); req(input$cross_m1, input$cross_m2)
    validate(need(input$cross_m1 != input$cross_m2, "Choose two different methods."))
    p <- plot_cross_methods(r$fits, input$cross_m1, input$cross_m2, label_points = isTRUE(input$cross_labels))
    validate(need(!is.null(p), "No common sub-samples with successful fits."))
    p
  }, res = 100)

  # ---- regions & biases --------------------------------------------------
  output$region_plot <- renderPlot({ plot_region_counts(region_summary(blk()$fits)) }, res = 100)
  output$region_table <- renderDT({
    tab <- region_summary(blk()$fits)
    d <- data.frame(Method = tab$method, Region = tab$region_label, Fits = tab$Freq, Rejected = tab$rejected,
                    `Share (%)` = round(100 * tab$Freq / ave(tab$Freq, tab$method, FUN = sum), 1), check.names = FALSE)
    datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 40), class = "compact stripe")
  })
  output$bias_table <- renderDT({
    r <- blk(); b <- moment_bias_table(r$fits, r$desc)
    d <- b[, c("label", "method", "d_skew", "skewness", "skew_pct", "skew_state", "d_kurt", "kurtosis", "kurt_pct", "kurt_state")]
    names(d) <- c("Sub-sample", "Method", "Skewness (data)", "Skewness (GLD)", "Skewness %-diff", "State", "Kurtosis (data)", "Kurtosis (GLD)", "Kurtosis %-diff", "State ")
    datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE), class = "compact stripe") |>
      formatRound(c("Skewness (data)", "Skewness (GLD)", "Kurtosis (data)", "Kurtosis (GLD)"), 4) |> formatRound(c("Skewness %-diff", "Kurtosis %-diff"), 2)
  })

  # ---- goodness-of-fit ---------------------------------------------------
  output$gof_plot <- renderPlot({
    r <- blk(); p <- plot_gof_series(r$fits, r$block, r$alpha, r$ksr_threshold)
    validate(need(!is.null(p), "No goodness-of-fit statistics (KS-R re-samples set to 0 and KS-D unavailable)."))
    p
  }, res = 100)
  output$gof_table <- renderDT({
    r <- blk(); d <- r$fits[, c("label", "method", "n", "ksr_pct", "ksd_D", "ksd_p", "rejected", "region_name")]
    names(d) <- c("Sub-sample", "Method", "N", "KS-R (%)", "KS-D", "KS-D p", "Rejected", "Region")
    datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 12), class = "compact stripe") |>
      formatRound("KS-R (%)", 2) |> formatRound(c("KS-D", "KS-D p"), 3) |>
      formatStyle("Rejected", target = "row", color = styleEqual(TRUE, "#d03b3b"), fontWeight = styleEqual(TRUE, "bold"))
  })

  # ---- published results -------------------------------------------------
  pub <- reactive({ req(input$pub_dataset); load_published(input$pub_dataset) })
  observeEvent(pub(), {
    updateSelectizeInput(session, "pub_years", choices = years_of(pub()$estimates), selected = character(0))
  })
  output$pub_info <- renderUI({
    p <- pub()
    div(class = "alert alert-light border small py-2 mb-2",
        tags$strong(p$title), tags$br(), tags$span(class = "g-note", p$source),
        tags$ul(class = "g-note mb-0 mt-1", lapply(p$notes, tags$li)))
  })
  output$pub_space <- renderPlot({
    p <- pub(); f <- space_data(p$estimates, input$pub_years)
    validate(need(nrow(f) > 0, "No estimates for the selected years."))
    yrs <- if (length(input$pub_years)) paste(input$pub_years, collapse = ", ") else "all months"
    plot_lambda_space(f, label_points = nrow(f) <= 60, title = sprintf("λ3-λ4 space of the published MM and MLE fits (%s)", yrs))
  }, res = 100)
  output$pub_params <- renderPlot({ p <- pub(); plot_lambda_series(p$estimates, "month", "Published GLD parameters by month") }, res = 100)
  output$pub_moments <- renderPlot({ p <- pub(); plot_moment_series(p$estimates, p$descriptive, "month", "Published: descriptive vs fitted GLD moments") }, res = 100)
  output$pub_cross <- renderPlot({ p <- pub(); plot_cross_methods(p$estimates, "MM", "MLE", label_points = FALSE) }, res = 100)
  output$pub_regions <- renderPlot({ p <- pub(); plot_region_counts(region_summary(p$estimates)) }, res = 100)
  output$pub_gof <- renderPlot({
    p <- pub(); g <- plot_gof_series(p$estimates, "month", 0.05, 70)
    validate(need(!is.null(g), "Goodness-of-fit statistics were not stored for this dataset."))
    g
  }, res = 100)
  output$pub_table <- renderDT({ est_dt(est_display(pub()$estimates)) })

  # ---- download ----------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() paste0("gld_results_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip"),
    content = function(file) {
      r <- res()
      td <- file.path(tempdir(), paste0("gld_", as.integer(Sys.time())))
      dir.create(td, showWarnings = FALSE)
      write_results(r, td)
      zip::zip(file, files = list.files(td), root = td)
    }
  )

  output$about <- renderUI(about_ui())
}

# ---------------------------------------------------------------------------
# Results export
# ---------------------------------------------------------------------------
write_results <- function(r, dir) {
  save_gg <- function(name, p, w = 11, h = 6.5) if (!is.null(p)) ggplot2::ggsave(file.path(dir, name), p, width = w, height = h, dpi = 130, bg = "white")
  if (r$mode == "rolling") {
    w <- r$wide; w$date <- format(w$date)
    utils::write.csv(w, file.path(dir, "rolling_series_wide.csv"), row.names = FALSE)
    utils::write.csv(r$fits, file.path(dir, "rolling_estimates_long.csv"), row.names = FALSE)
    utils::write.csv(r$rets$data, file.path(dir, "filtered_returns.csv"), row.names = FALSE)
    save_gg("rolling_lambda3_lambda4.png", plot_rolling_series(r$fits, c("l3", "l4")), h = 7)
    save_gg("rolling_all_parameters.png", plot_rolling_series(r$fits, c("l1", "l2", "l3", "l4")), h = 10)
    save_gg("rolling_gld_moments.png", plot_rolling_series(r$fits, c("skewness", "kurtosis")), h = 7)
    save_gg("lambda_space.png", plot_lambda_space(r$fits, facet = "panel"), w = 3 * length(r$series) + 2, h = 3 * length(r$methods) + 1.5)
    save_gg("returns.png", plot_returns(r$rets$data, paste(r$series, collapse = ", "), r$kind), h = 8)
    writeLines(c(paste(APP_NAME, "- rolling-window results export"),
                 paste0("Application by ", APP_AUTHOR$name, " (", APP_AUTHOR$affiliation, "), ", paste(APP_AUTHOR$emails, collapse = " / ")),
                 paste("Generated:", format(Sys.time())),
                 paste("Series:", paste(r$series, collapse = ", "), "| returns:", r$kind, "| engine:", r$engine, "| methods:", paste(r$methods, collapse = ", ")),
                 sprintf("Window: %d returns, step %d; one estimate per window end date (k = N - m + 1 windows when step = 1).", r$window, r$step),
                 "Files: rolling_series_wide.csv (date x <series>_<parameter>), rolling_estimates_long.csv (all parameters, GLD moments, region per window), filtered_returns.csv, PNG figures."),
               file.path(dir, "README.txt"))
    return(invisible(NULL))
  }
  utils::write.csv(r$rets$data[, c("time", "ret", "period")], file.path(dir, "filtered_returns.csv"), row.names = FALSE)
  utils::write.csv(r$desc, file.path(dir, "descriptive_statistics.csv"), row.names = FALSE)
  utils::write.csv(r$fits, file.path(dir, "gld_estimates.csv"), row.names = FALSE)
  utils::write.csv(region_summary(r$fits), file.path(dir, "region_summary.csv"), row.names = FALSE)
  utils::write.csv(moment_bias_table(r$fits, r$desc), file.path(dir, "moment_percent_differences.csv"), row.names = FALSE)
  save_gg("returns.png", plot_returns(r$rets$data, r$series, r$kind))
  save_gg("descriptive_moments.png", plot_desc_moments(r$desc, r$block), h = 9)
  save_gg("gld_parameters.png", plot_lambda_series(r$fits, r$block), h = 9)
  save_gg("gld_moments.png", plot_moment_series(r$fits, r$desc, r$block), h = 9)
  save_gg("lambda_space.png", plot_lambda_space(r$fits, label_points = r$n_blocks <= 24), w = 10, h = 6)
  if (length(r$methods) >= 2) save_gg("method_comparison.png", plot_cross_methods(r$fits, r$methods[1], r$methods[2]), w = 10, h = 5.5)
  save_gg("regions.png", plot_region_counts(region_summary(r$fits)), w = 8, h = 5)
  save_gg("goodness_of_fit.png", plot_gof_series(r$fits, r$block, r$alpha, r$ksr_threshold))
  writeLines(c(paste(APP_NAME, "- results export"),
               paste0("Application by ", APP_AUTHOR$name, " (", APP_AUTHOR$affiliation, "), ", paste(APP_AUTHOR$emails, collapse = " / ")),
               paste("Generated:", format(Sys.time())),
               paste("Series:", r$series, "| returns:", r$kind, "| sub-samples:", r$block, "| engine:", r$engine),
               paste("Methods:", paste(r$methods, collapse = ", ")),
               paste("Rejection rule:", r$reject_rule, "| alpha:", r$alpha, "| KS-R threshold:", r$ksr_threshold),
               "Files: gld_estimates.csv (one row per sub-sample and method), descriptive_statistics.csv, moment_percent_differences.csv, region_summary.csv, filtered_returns.csv and PNG figures."),
             file.path(dir, "README.txt"))
}

# ---------------------------------------------------------------------------
# About page
# ---------------------------------------------------------------------------
about_ui <- function() {
  div(class = "g-about",
    div(class = "alert alert-light border mb-3",
        h4(class = "mb-1", "Author"),
        tags$p(class = "mb-1", HTML(sprintf("<strong>%s</strong>, %s", APP_AUTHOR$name, APP_AUTHOR$affiliation))),
        tags$p(class = "mb-1 small", icon("envelope"), " ",
               HTML(paste(sprintf('<a href="mailto:%s">%s</a>', APP_AUTHOR$emails, APP_AUTHOR$emails), collapse = " | "))),
        tags$p(class = "mb-0 small", icon("link"), " ",
               HTML(paste(sprintf('<a href="%s" target="_blank">%s</a>', APP_AUTHOR$links, names(APP_AUTHOR$links)), collapse = " | "))),
        tags$p(class = "mb-0 small text-muted mt-2",
               HTML(sprintf("Suggested citation: %s (%s). <em>%s: A Shiny application for fitting the generalised lambda distribution to financial returns</em>. R Shiny application, <a href=\"%s\" target=\"_blank\">%s</a>. Source code: <a href=\"%s\" target=\"_blank\">GitHub</a>.",
                            APP_AUTHOR$name, format(Sys.Date(), "%Y"), APP_NAME, APP_AUTHOR$app_url, sub("^https?://", "", sub("/$", "", APP_AUTHOR$app_url)), APP_AUTHOR$repo_url)))),
    h4("What this app does"),
    p("The app reproduces, and lets you extend to your own data, the generalised lambda distribution (GLD) analyses of the author's papers and PhD thesis. ",
      "In the ", tags$b("sub-sample scheme"), " of ", tags$em("On the goodness-of-fits of the generalized lambda distribution on high-frequency stock index returns"),
      " (Owusu Junior, Jeyasreedharan & Alagidede, 2022) and of the REIT study (Owusu Junior, Tweneboah, Ijasan & Jeyasreedharan, 2019), the four-parameter FMKL GLD is fitted to ",
      "non-overlapping sub-samples of returns by moment matching (MM), maximum likelihood (MLE) and other methods, the fits are tested with Kolmogorov-Smirnov statistics, ",
      "and the estimated shape parameters are mapped into the λ3-λ4 lambda space to read off the support region, symmetry and the sub-samples where the fit is rejected. ",
      "In the ", tags$b("rolling-window scheme"), " of chapter 5 of the PhD thesis (Owusu Junior, 2020) and of ",
      tags$em("Shape-shift contagion in emerging markets equities"), " (Owusu Junior, Alagidede & Tweneboah, 2020), the GLD is re-estimated by MLE in windows of 20 daily returns moved ",
      "one day at a time, which turns the shape parameters λ3 (skewness) and λ4 (kurtosis) into daily time series. The app produces and exports these series; ",
      "the wavelet and spillover connectedness measures that the thesis applies to them are not part of the app."),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("The distribution"),
           card_body(
             p("The FMKL GLD (Freimer, Kollia, Mudholkar & Lin, 1988) is defined by its quantile function"),
             p(class = "text-center", HTML("Q(u) = &lambda;<sub>1</sub> + [ (u<sup>&lambda;<sub>3</sub></sup> &minus; 1)/&lambda;<sub>3</sub> &minus; ((1 &minus; u)<sup>&lambda;<sub>4</sub></sup> &minus; 1)/&lambda;<sub>4</sub> ] / &lambda;<sub>2</sub>")),
             p("with location λ1, scale λ2 > 0 and the shape parameters λ3 (left tail) and λ4 (right tail). It is a valid distribution for every real λ3 and λ4. ",
               "The k-th moment exists when λ3 and λ4 exceed -1/k, so skewness needs λ3, λ4 > -1/3 and kurtosis λ3, λ4 > -1/4."),
             tags$ul(class = "small",
               tags$li(tags$b("Region 1"), " (λ3 <= 0, λ4 > 0): lower tail unbounded, upper tail bounded"),
               tags$li(tags$b("Region 2"), " (λ3 > 0, λ4 <= 0): lower tail bounded, upper tail unbounded"),
               tags$li(tags$b("Region 3"), " (both > 0): finite support"),
               tags$li(tags$b("Region 4"), " (both <= 0): infinite support, the region where most financial returns fall")),
             plotOutput("about_regions", height = "360px"))),
      card(card_header("Estimation and testing"),
           card_body(
             tags$ul(
               tags$li(tags$b("MM"), ": the parameters that match the first four sample moments (Karian & Dudewicz, 2000). GLDEX fun.RMFMKL.mm, or gld fit.fkml 'Mom'."),
               tags$li(tags$b("MLE"), ": numerical maximum likelihood after a Sobol-sequence search for starting values (Su, 2007). GLDEX fun.RMFMKL.ml, or gld fit.fkml 'ML'."),
               tags$li(tags$b("Other methods (gld)"), ": maximum product of spacings, Titterington's method, the starship (King & MacGillivray, 1999), L- and trimmed L-moments, distributional least absolutes."),
               tags$li(tags$b("KS-D"), ": the one-sample Kolmogorov-Smirnov distance between the empirical distribution and the fitted GLD, with its p-value."),
               tags$li(tags$b("KS-R"), ": Su's re-sampling test (fun.diag.ks.g): the share of random sub-samples (default 90% of the data, 1,000 draws) for which the KS test does not reject the fit; the higher the better."),
               tags$li(tags$b("Fitted moments"), ": the theoretical mean, variance, skewness and excess kurtosis of the fitted GLD, compared with the descriptive moments (%-differences, Table 2 of the paper).")
             ))),
      card(card_header("The two sampling schemes"),
           card_body(
             p(class = "mb-1", tags$b("Sub-samples (Cogent 2022):")),
             tags$ol(class = "small",
               tags$li("5-minute index prices are turned into log returns; overnight (between-session) returns are removed, then zero returns."),
               tags$li("The sample is split into monthly sub-samples to allow for the non-stationarity of the return process; each is winsorised at the 0.001% / 99.999% quantiles."),
               tags$li("MM and MLE GLDs are fitted to every sub-sample; the lambda-space scatter marks rejected fits in red and symmetric fits (λ3 = λ4) with a diamond.")
             ),
             p(class = "mb-1", tags$b("Rolling window (PhD thesis ch. 5; EBL 2020):")),
             tags$ol(class = "small",
               tags$li("Daily index prices are turned into log returns r_t = ln P_t - ln P_t-1."),
               tags$li("The series is partitioned into k = N - m + 1 overlapping windows of m = 20 trading days (one month), moved one day at a time."),
               tags$li("The FKML GLD is fitted by MLE in every window; the window-end series of λ3 and λ4 are the daily rolling skewness and kurtosis estimates, exported as CSV for further use (in the thesis: wavelet multiple correlation and Baruník-Křehlík spillovers).")
             ),
             p(class = "small text-muted", "The sidebar exposes every one of these choices (scheme, window length and step, filters, quantiles, methods, tests), so departures from the papers are explicit.")))
    ),
    h4("How to use"),
    tags$ol(
      tags$li("Pick an example dataset or upload a file with a time-stamp column and one or more price/return columns. Choose the series and the date range."),
      tags$li("Set the filters and the sampling scheme: non-overlapping sub-samples (monthly as in the 2022 paper, or weekly / quarterly / yearly / whole sample) or the rolling window (window length and step; one or more series at once)."),
      tags$li("Choose the engine and the estimation methods (MM and MLE replicate the papers), the goodness-of-fit settings and the rejection rule, then click ", tags$b("Fit GLDs!"), "."),
      tags$li("Browse the tabs: estimates, fit diagnostics for any sub-sample, parameter and moment time series, lambda space, method comparison, regions and biases, goodness-of-fit; ",
              "or, for the rolling window, the rolling estimates tab with its CSV downloads. ", tags$b("Download results"), " bundles the CSV tables and PNG figures."),
      tags$li("The ", tags$b("Published results"), " tab redraws the figures of the paper from the stored monthly estimates for the DJI30 (2001-2016).")
    ),
    h4("Papers and thesis using these methods"),
    tags$ul(class = "small",
      tags$li("Owusu Junior, P., Tweneboah, G., Ijasan, K., & Jeyasreedharan, N. (2019). Modelling return behaviour of global real estate investment trusts equities: Evidence from generalised lambda distribution. Journal of European Real Estate Research, 12(3), 311-328. ",
              tags$a(href = "https://doi.org/10.1108/JERER-09-2018-0043", target = "_blank", "doi:10.1108/JERER-09-2018-0043"),
              " (MM, MLE and quantile matching for seven REIT markets, full sample and post-GFC)"),
      tags$li("Owusu Junior, P. (2020). Emerging financial markets: Spatial risks, elicitability of risk models, and shape shift contagion. Doctoral thesis, Graduate School of Business Administration, University of the Witwatersrand, Johannesburg (supervisor: Imhotep Paul Alagidede). ",
              "Chapter 5 estimates rolling-window (20-day) MLE GLD shape parameters of nine MSCI emerging markets and the US, 2001-2019."),
      tags$li("Owusu Junior, P., Alagidede, I., & Tweneboah, G. (2020). Shape-shift contagion in emerging markets equities: Evidence from frequency- and time-domain analysis. Economics and Business Letters, 9(3), 146-156. ",
              tags$a(href = "https://doi.org/10.17811/ebl.9.3.2020.146-156", target = "_blank", "doi:10.17811/ebl.9.3.2020.146-156"),
              " (daily one-month rolling λ3 and λ4 series as inputs to Baruník-Křehlík spillovers)"),
      tags$li("Owusu Junior, P., Jeyasreedharan, N., & Alagidede, I. P. (2022). On the goodness-of-fits of the generalized lambda distribution on high-frequency stock index returns. Cogent Economics & Finance, 10(1), 2095764. ",
              tags$a(href = "https://doi.org/10.1080/23322039.2022.2095764", target = "_blank", "doi:10.1080/23322039.2022.2095764"),
              " (monthly sub-samples of 5-min DJI30 returns, MM vs MLE, lambda space)")
    ),
    h4("Software"),
    tags$ul(class = "small",
      tags$li("King, R., Dean, B., Klinke, S., & van Staden, P. (2025). gld: Estimation and use of the generalised (Tukey) lambda distribution. R package version 2.6.8. ",
              tags$a(href = "https://cran.r-project.org/package=gld", target = "_blank", "CRAN"), " | ", tags$a(href = "https://github.com/newystats/gld/", target = "_blank", "GitHub")),
      tags$li("Su, S. (2007). Fitting single and mixture of generalized lambda distributions to data via discretized and maximum likelihood methods: GLDEX in R. Journal of Statistical Software, 21(9), 1-17. ",
              tags$a(href = "https://doi.org/10.18637/jss.v021.i09", target = "_blank", "doi:10.18637/jss.v021.i09")),
      tags$li("Su, S. (2007). Numerical maximum log likelihood estimation for generalized lambda distributions. Computational Statistics & Data Analysis, 51(8), 3983-3998."),
      tags$li("Su, S. (2005). A discretized approach to flexibly fit generalized lambda distributions to data. Journal of Modern Applied Statistical Methods, 4(2), 408-424.")
    ),
    h4("Methodological references"),
    tags$ul(class = "small",
      tags$li("Ramberg, J. S., & Schmeiser, B. W. (1974). An approximate method for generating asymmetric random variables. Communications of the ACM, 17(2), 78-82."),
      tags$li("Freimer, M., Kollia, G., Mudholkar, G. S., & Lin, C. T. (1988). A study of the generalized Tukey lambda family. Communications in Statistics - Theory and Methods, 17(10), 3547-3567."),
      tags$li("Karian, Z. A., & Dudewicz, E. J. (2000). Fitting statistical distributions: The generalized lambda distribution and generalized bootstrap methods. CRC Press."),
      tags$li("Karian, Z. A., & Dudewicz, E. J. (2016). Handbook of fitting statistical distributions with R. CRC Press."),
      tags$li("King, R. A. R., & MacGillivray, H. L. (1999). A starship estimation method for the generalized lambda distributions. Australian & New Zealand Journal of Statistics, 41(3), 353-374."),
      tags$li("Chalabi, Y., Scott, D. J., & Wuertz, D. (2012). Flexible distribution modeling with the generalized lambda distribution. MPRA Paper 43333."),
      tags$li("Corlu, C. G., & Meterelliyoz, M. (2016). Estimating the parameters of the generalized lambda distribution: Which method performs best? Communications in Statistics - Simulation and Computation, 45(7), 2276-2296."),
      tags$li("Lakhany, A., & Mausser, H. (2000). Estimating the parameters of the generalized lambda distribution. Algo Research Quarterly, 3(3), 47-58."),
      tags$li("Wang, B., & Wang, X.-F. (2016). Fitting the generalized lambda distribution to pre-binned data. Journal of Statistical Computation and Simulation, 86(9), 1785-1797.")
    )
  )
}

shinyApp(ui, server)
