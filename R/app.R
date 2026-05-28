# SAP FI Synthetic Data Generator - Shiny Dashboard
# Run from project root:
#   install.packages(c("shiny", "DBI", "RSQLite", "dplyr", "ggplot2", "DT", "scales"))
#   Rscript -e "shiny::runApp('R/app.R', launch.browser = TRUE)"

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(grid)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

resolve_default_db <- function() {
  candidates <- c("data/sap_fi.sqlite", "../data/sap_fi.sqlite")
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) normalizePath(existing[[1]], winslash = "/", mustWork = TRUE) else "data/sap_fi.sqlite"
}

DEFAULT_DB <- resolve_default_db()

TABLE_LABELS <- c(
  "T001" = "Company Codes",
  "KNA1" = "Customer General Master",
  "KNB1" = "Customer Company Code Master",
  "LFA1" = "Vendor General Master",
  "LFB1" = "Vendor Company Code Master",
  "SKA1" = "G/L Account Chart of Accounts Master",
  "SKB1" = "G/L Account Company Code Master",
  "BKPF" = "Accounting Document Header",
  "BSEG" = "Accounting Document Segment",
  "BSID" = "AR Open Customer Items",
  "BSAD" = "AR Cleared Customer Items",
  "BSIK" = "AP Open Vendor Items",
  "BSAK" = "AP Cleared Vendor Items",
  "BSIS" = "G/L Open Items",
  "BSAS" = "G/L Cleared Items",
  "ACDOCA" = "Universal Journal Line Items"
)

FIELD_LABELS <- c(
  "MANDT" = "Client", "BUKRS" = "Company Code", "BUTXT" = "Company Name", "LAND1" = "Country Key",
  "WAERS" = "Currency Key", "SPRAS" = "Language Key", "KUNNR" = "Customer Number", "LIFNR" = "Vendor Number",
  "NAME1" = "Name", "ORT01" = "City", "PSTLZ" = "Postal Code", "STRAS" = "Street Address",
  "KTOKD" = "Customer Account Group", "KTOKK" = "Vendor Account Group", "ERDAT" = "Created On",
  "AKONT" = "Reconciliation Account", "ZTERM" = "Payment Terms", "SAKNR" = "G/L Account Number",
  "KTOPL" = "Chart of Accounts", "TXT20" = "Short Text", "TXT50" = "Long Text", "XOPVW" = "Open Item Management",
  "BELNR" = "Accounting Document Number", "GJAHR" = "Fiscal Year", "BLART" = "Document Type",
  "BLDAT" = "Document Date", "BUDAT" = "Posting Date", "MONAT" = "Fiscal Period", "CPUDT" = "Entry Date",
  "USNAM" = "User Name", "TCODE" = "Transaction Code", "XBLNR" = "Reference Document Number", "BKTXT" = "Document Header Text",
  "BUZEI" = "Line Item Number", "BSCHL" = "Posting Key", "KOART" = "Account Type", "HKONT" = "G/L Account",
  "SHKZG" = "Debit/Credit Indicator", "DMBTR" = "Amount in Local Currency", "WRBTR" = "Amount in Document Currency",
  "SGTXT" = "Line Item Text", "AUGBL" = "Clearing Document Number", "AUGDT" = "Clearing Date",
  "ZFBDT" = "Baseline Date", "FAEDT" = "Due Date", "PRCTR" = "Profit Center", "KOSTL" = "Cost Center",
  "MWSKZ" = "Tax Code", "RLDNR" = "Ledger", "RBUKRS" = "Company Code", "RACCT" = "Account Number",
  "DOCLN" = "Document Line Number", "HSL" = "Amount in Company Code Currency", "WSL" = "Amount in Transaction Currency",
  "table_name" = "Table Technical Name", "table_label" = "Table Name", "rows" = "Rows", "month" = "Posting Month",
  "documents" = "Documents", "line_items" = "Line Items", "open_items" = "Open Items", "amount" = "Amount",
  "balance" = "Balance", "aging_bucket" = "Aging Bucket", "risk_signal" = "Risk Signal", "risk_score" = "Risk Score",
  "reason" = "Reason", "signed_amount" = "Signed Amount", "debit" = "Debit", "credit" = "Credit", "net_balance" = "Net Balance"
)

friendly_field <- function(x) {
  out <- FIELD_LABELS[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}

friendly_table <- function(x) {
  out <- TABLE_LABELS[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}

label_col <- function(x) {
  label <- friendly_field(x)
  ifelse(label == x, x, paste0(label, " (", x, ")"))
}

label_columns <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(df)
  names(df) <- label_col(names(df))
  df
}

field_dictionary <- function(table_name, df) {
  data.frame(
    table_technical_name = table_name,
    table_name = friendly_table(table_name),
    field_technical_name = names(df),
    field_name = friendly_field(names(df)),
    stringsAsFactors = FALSE
  )
}

PALETTES <- list(
  "Default" = list(primary = "#2C3E50", accent = "#337AB7", fill = "#337AB7", fill2 = "#337AB7", bg = "#FFFFFF", panel = "#F5F5F5", text = "#222222"),
  "SAP Blue" = list(primary = "#0A6ED1", accent = "#0854A0", fill = "#0A6ED1", fill2 = "#0A6ED1", bg = "#F7FBFF", panel = "#EAF3FF", text = "#1F2D3D"),
  "Finance Green" = list(primary = "#1B5E20", accent = "#2E7D32", fill = "#2E7D32", fill2 = "#2E7D32", bg = "#FAFFFA", panel = "#EEF7EE", text = "#1B1B1B"),
  "Executive Dark" = list(primary = "#111827", accent = "#6366F1", fill = "#6366F1", fill2 = "#6366F1", bg = "#F9FAFB", panel = "#E5E7EB", text = "#111827"),
  "Warm Amber" = list(primary = "#7C2D12", accent = "#D97706", fill = "#D97706", fill2 = "#D97706", bg = "#FFFBEB", panel = "#FEF3C7", text = "#2B2118")
)

safe_query <- function(db_path, sql) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sql)
}

sql_quote <- function(x) paste0("'", gsub("'", "''", as.character(x)), "'")

sql_in_filter <- function(col, values) {
  values <- values %||% character(0)
  values <- values[!is.na(values) & values != "" & values != "All"]
  if (length(values) == 0) return(NULL)
  paste0(col, " IN (", paste(sql_quote(values), collapse = ","), ")")
}

build_where <- function(parts) {
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) "" else paste("WHERE", paste(parts, collapse = " AND "))
}

qualify_where <- function(where_clause, alias, cols) {
  if (is.null(where_clause) || where_clause == "") return("")
  out <- where_clause
  for (col in cols) {
    out <- gsub(paste0("\\b", col, "\\b"), paste0(alias, ".", col), out, perl = TRUE)
  }
  out
}

fmt_amount <- function(x) comma(round(x, 2))
fmt_int <- function(x) comma(as.integer(round(x, 0)))

make_dt <- function(df, page_length = 20) {
  datatable(
    df,
    extensions = "Buttons",
    selection = "single",
    options = list(pageLength = page_length, scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "excel"))
  )
}

base_plot_theme <- function() {
  theme_minimal() + theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))
}

empty_plot <- function(message = "No data available") {
  ggplot() + annotate("text", x = 0, y = 0, label = message, size = 5) + theme_void()
}

kpi_card <- function(title, value_output, subtitle = NULL) {
  wellPanel(class = "kpi-card", h4(title), div(class = "kpi-value", textOutput(value_output)), if (!is.null(subtitle)) small(subtitle))
}

ui <- fluidPage(
  uiOutput("palette_css"),
  titlePanel("SAP FI Synthetic Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("palette", "Color palette", choices = names(PALETTES), selected = "Default"),
      hr(),
      h4("Global filters"),
      uiOutput("filter_bukrs"),
      uiOutput("filter_gjahr"),
      uiOutput("filter_blart"),
      uiOutput("filter_customer"),
      uiOutput("filter_vendor"),
      uiOutput("filter_gl"),
      actionButton("clear_filters", "Clear filters"),
      hr(),
      downloadButton("export_current_pdf", "Export current page PDF"),
      downloadButton("export_all_pdf", "Export all pages PDF"),
      hr(),
      helpText("Filters affect the dashboard, PDF exports, and drill-down tables. Tables include Copy / CSV / Excel buttons.")
    ),
    mainPanel(
      tabsetPanel(id = "main_tabs",
        tabPanel("Overview",
          br(),
          fluidRow(
            column(3, kpi_card("Documents", "kpi_docs")),
            column(3, kpi_card("Line items", "kpi_lines")),
            column(3, kpi_card("Open AR", "kpi_ar_amount")),
            column(3, kpi_card("Open AP", "kpi_ap_amount"))
          ),
          fluidRow(
            column(3, kpi_card("Customers", "kpi_customers")),
            column(3, kpi_card("Vendors", "kpi_vendors")),
            column(3, kpi_card("Unbalanced docs", "kpi_unbalanced")),
            column(3, kpi_card("Risk signals", "kpi_risk_signals"))
          ),
          plotOutput("plot_monthly"),
          DTOutput("tbl_counts")
        ),
        tabPanel("AR",
          br(),
          fluidRow(
            column(4, kpi_card("Open AR items", "ar_open_count")),
            column(4, kpi_card("Open AR amount", "ar_open_amount")),
            column(4, kpi_card("Cleared AR items", "ar_cleared_count"))
          ),
          fluidRow(column(6, plotOutput("plot_ar_aging")), column(6, plotOutput("plot_ar_customers"))),
          h4("Top open AR customers"),
          DTOutput("tbl_ar_top")
        ),
        tabPanel("AP",
          br(),
          fluidRow(
            column(4, kpi_card("Open AP items", "ap_open_count")),
            column(4, kpi_card("Open AP amount", "ap_open_amount")),
            column(4, kpi_card("Cleared AP items", "ap_cleared_count"))
          ),
          fluidRow(column(6, plotOutput("plot_ap_aging")), column(6, plotOutput("plot_ap_vendors"))),
          h4("Top open AP vendors"),
          DTOutput("tbl_ap_top")
        ),
        tabPanel("GL",
          br(),
          fluidRow(
            column(4, kpi_card("GL open items", "gl_open_count")),
            column(4, kpi_card("GL debit amount", "gl_debit_amount")),
            column(4, kpi_card("GL credit amount", "gl_credit_amount"))
          ),
          fluidRow(column(6, plotOutput("plot_gl_accounts")), column(6, plotOutput("plot_gl_composition"))),
          h4("Top open GL accounts"),
          DTOutput("tbl_gl_top")
        ),
        tabPanel("Documents",
          br(),
          fluidRow(column(6, plotOutput("plot_doc_types")), column(6, plotOutput("plot_debit_credit"))),
          h4("Potentially unbalanced documents"),
          DTOutput("tbl_unbalanced")
        ),
        tabPanel("Journal Explorer",
          br(),
          fluidRow(
            column(4, uiOutput("journal_doc_picker")),
            column(4, kpi_card("Selected document balance", "journal_balance")),
            column(4, kpi_card("Selected document lines", "journal_lines"))
          ),
          h4("Document header"),
          DTOutput("tbl_journal_header"),
          h4("Document line items"),
          DTOutput("tbl_journal_lines")
        ),
        tabPanel("Risk Signals",
          br(),
          fluidRow(
            column(4, kpi_card("Large documents", "risk_large_count")),
            column(4, kpi_card("Round-number items", "risk_round_count")),
            column(4, kpi_card("Weekend postings", "risk_weekend_count"))
          ),
          plotOutput("plot_risk_signals"),
          h4("Accounting risk signals"),
          DTOutput("tbl_risk_signals")
        ),
        tabPanel("Data Browser",
          br(),
          selectInput("table_name", "Table", choices = NULL),
          h4(textOutput("selected_table_title")),
          h5("Field dictionary"),
          DTOutput("tbl_field_dictionary"),
          h5("Selected table sample"),
          DTOutput("tbl_browser")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  refresh_token <- reactiveVal(0)

  db_path <- reactive(DEFAULT_DB)
  pal <- reactive(PALETTES[[input$palette]])

  output$palette_css <- renderUI({
    p <- pal()
    tags$style(HTML(sprintf("\n      body { background-color: %s; color: %s; }\n      .well { background-color: %s; border-color: %s; }\n      .kpi-card { min-height: 118px; }\n      .kpi-value { font-size: 22px; font-weight: 700; color: %s; }\n      h1, h2, h3, h4 { color: %s; }\n      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {\n        border-top: 3px solid %s; color: %s; font-weight: 600;\n      }\n      .btn-default, .btn { border-radius: 6px; }\n      .btn-primary, .download-button { background-color: %s; border-color: %s; color: white; }\n    ", p$bg, p$text, p$panel, p$panel, p$accent, p$primary, p$accent, p$primary, p$accent, p$accent)))
  })

  tables <- reactive({
    refresh_token(); req(file.exists(db_path()))
    con <- dbConnect(SQLite(), db_path()); on.exit(dbDisconnect(con), add = TRUE)
    dbListTables(con)
  })

  choices_from_query <- function(sql, col) {
    refresh_token(); req(file.exists(db_path()))
    x <- tryCatch(safe_query(db_path(), sql)[[col]], error = function(e) character(0))
    sort(unique(as.character(x[!is.na(x)])))
  }

  bukrs_choices <- reactive(choices_from_query("SELECT DISTINCT BUKRS FROM T001 ORDER BY BUKRS", "BUKRS"))
  gjahr_choices <- reactive(choices_from_query("SELECT DISTINCT GJAHR FROM BKPF ORDER BY GJAHR", "GJAHR"))
  blart_choices <- reactive(choices_from_query("SELECT DISTINCT BLART FROM BKPF ORDER BY BLART", "BLART"))
  customer_choices <- reactive(choices_from_query("SELECT DISTINCT KUNNR FROM BSID UNION SELECT DISTINCT KUNNR FROM BSAD ORDER BY KUNNR", "KUNNR"))
  vendor_choices <- reactive(choices_from_query("SELECT DISTINCT LIFNR FROM BSIK UNION SELECT DISTINCT LIFNR FROM BSAK ORDER BY LIFNR", "LIFNR"))
  gl_choices <- reactive(choices_from_query("SELECT DISTINCT HKONT FROM BSEG WHERE HKONT IS NOT NULL ORDER BY HKONT", "HKONT"))

  output$filter_bukrs <- renderUI(selectInput("filter_bukrs", "Company Code (BUKRS)", choices = c("All", bukrs_choices()), selected = "All", multiple = TRUE))
  output$filter_gjahr <- renderUI(selectInput("filter_gjahr", "Fiscal Year (GJAHR)", choices = c("All", gjahr_choices()), selected = "All", multiple = TRUE))
  output$filter_blart <- renderUI(selectInput("filter_blart", "Document Type (BLART)", choices = c("All", blart_choices()), selected = "All", multiple = TRUE))
  output$filter_customer <- renderUI(selectizeInput("filter_customer", "Customer (KUNNR)", choices = c("All", customer_choices()), selected = "All", multiple = TRUE))
  output$filter_vendor <- renderUI(selectizeInput("filter_vendor", "Vendor (LIFNR)", choices = c("All", vendor_choices()), selected = "All", multiple = TRUE))
  output$filter_gl <- renderUI(selectizeInput("filter_gl", "G/L Account (HKONT)", choices = c("All", gl_choices()), selected = "All", multiple = TRUE))

  observeEvent(input$clear_filters, {
    updateSelectInput(session, "filter_bukrs", selected = "All")
    updateSelectInput(session, "filter_gjahr", selected = "All")
    updateSelectInput(session, "filter_blart", selected = "All")
    updateSelectizeInput(session, "filter_customer", selected = "All")
    updateSelectizeInput(session, "filter_vendor", selected = "All")
    updateSelectizeInput(session, "filter_gl", selected = "All")
  })

  bkpf_where <- reactive(build_where(list(sql_in_filter("BUKRS", input$filter_bukrs), sql_in_filter("GJAHR", input$filter_gjahr), sql_in_filter("BLART", input$filter_blart))))
  bseg_where <- reactive(build_where(list(sql_in_filter("BUKRS", input$filter_bukrs), sql_in_filter("GJAHR", input$filter_gjahr), sql_in_filter("KUNNR", input$filter_customer), sql_in_filter("LIFNR", input$filter_vendor), sql_in_filter("HKONT", input$filter_gl))))
  ar_where <- reactive(build_where(list(sql_in_filter("BUKRS", input$filter_bukrs), sql_in_filter("GJAHR", input$filter_gjahr), sql_in_filter("KUNNR", input$filter_customer))))
  ap_where <- reactive(build_where(list(sql_in_filter("BUKRS", input$filter_bukrs), sql_in_filter("GJAHR", input$filter_gjahr), sql_in_filter("LIFNR", input$filter_vendor))))
  gl_where <- reactive(build_where(list(sql_in_filter("BUKRS", input$filter_bukrs), sql_in_filter("GJAHR", input$filter_gjahr), sql_in_filter("HKONT", input$filter_gl))))

  observe({
    updateSelectInput(session, "table_name", choices = setNames(tables(), paste0(friendly_table(tables()), " (", tables(), ")")))
  })

  row_counts <- reactive({
    tabs <- tables()
    data.frame(table_name = tabs, table_label = friendly_table(tabs), rows = sapply(tabs, function(t) safe_query(db_path(), paste0("SELECT COUNT(*) AS n FROM ", t))$n), row.names = NULL)
  })

  monthly <- reactive({
    safe_query(db_path(), paste0("\n      SELECT substr(BUDAT, 1, 7) AS month, COUNT(*) AS documents\n      FROM BKPF ", bkpf_where(), "\n      GROUP BY substr(BUDAT, 1, 7)\n      ORDER BY month\n    "))
  })

  ar_open <- reactive(safe_query(db_path(), paste("SELECT * FROM BSID", ar_where())))
  ar_cleared <- reactive(safe_query(db_path(), paste("SELECT * FROM BSAD", ar_where())))
  ap_open <- reactive(safe_query(db_path(), paste("SELECT * FROM BSIK", ap_where())))
  ap_cleared <- reactive(safe_query(db_path(), paste("SELECT * FROM BSAK", ap_where())))
  gl_open <- reactive(safe_query(db_path(), paste("SELECT * FROM BSIS", gl_where())))

  aging_query <- function(table_name, where_clause) {
    safe_query(db_path(), sprintf("\n      SELECT\n        CASE\n          WHEN julianday('now') - julianday(ZFBDT) <= 30 THEN '000-030'\n          WHEN julianday('now') - julianday(ZFBDT) <= 60 THEN '031-060'\n          WHEN julianday('now') - julianday(ZFBDT) <= 90 THEN '061-090'\n          ELSE '090+'\n        END AS aging_bucket,\n        COUNT(*) AS open_items,\n        ROUND(SUM(DMBTR), 2) AS amount\n      FROM %s %s\n      GROUP BY aging_bucket\n      ORDER BY aging_bucket\n    ", table_name, where_clause))
  }

  ar_top <- reactive(safe_query(db_path(), paste0("\n      SELECT KUNNR, COUNT(*) AS open_items, ROUND(SUM(DMBTR), 2) AS amount\n      FROM BSID ", ar_where(), "\n      GROUP BY KUNNR ORDER BY amount DESC LIMIT 20\n    ")))

  ap_top <- reactive(safe_query(db_path(), paste0("\n      SELECT LIFNR, COUNT(*) AS open_items, ROUND(SUM(DMBTR), 2) AS amount\n      FROM BSIK ", ap_where(), "\n      GROUP BY LIFNR ORDER BY amount DESC LIMIT 20\n    ")))

  gl_top <- reactive(safe_query(db_path(), paste0("\n      SELECT HKONT, COUNT(*) AS line_items, ROUND(SUM(DMBTR), 2) AS amount\n      FROM BSIS ", gl_where(), "\n      GROUP BY HKONT ORDER BY amount DESC LIMIT 20\n    ")))

  gl_composition <- reactive({
    where_clause <- qualify_where(gl_where(), "b", c("BUKRS", "GJAHR", "HKONT"))
    safe_query(db_path(), paste0("\n      SELECT COALESCE(s.GLACCOUNT_TYPE, 'UNKNOWN') AS gl_account_type, ROUND(SUM(b.DMBTR), 2) AS amount\n      FROM BSIS b\n      LEFT JOIN SKA1 s ON b.HKONT = s.SAKNR\n      ", where_clause, "\n      GROUP BY COALESCE(s.GLACCOUNT_TYPE, 'UNKNOWN')\n      ORDER BY amount DESC\n    "))
  })

  doc_types <- reactive(safe_query(db_path(), paste0("\n      SELECT BLART, COUNT(*) AS documents\n      FROM BKPF ", bkpf_where(), "\n      GROUP BY BLART ORDER BY documents DESC\n    ")))

  debit_credit <- reactive(safe_query(db_path(), paste0("\n      SELECT SHKZG, COUNT(*) AS line_items, ROUND(SUM(DMBTR), 2) AS amount\n      FROM BSEG ", bseg_where(), "\n      GROUP BY SHKZG ORDER BY SHKZG\n    ")))

  unbalanced <- reactive(safe_query(db_path(), paste0("\n      SELECT BUKRS, BELNR, GJAHR,\n             ROUND(SUM(CASE WHEN SHKZG = 'S' THEN DMBTR ELSE -DMBTR END), 2) AS balance\n      FROM BSEG ", bseg_where(), "\n      GROUP BY BUKRS, BELNR, GJAHR\n      HAVING ABS(balance) > 0.01\n      LIMIT 50\n    ")))

  risk_signals <- reactive({
    large_docs <- safe_query(db_path(), paste0("\n      SELECT 'Large document' AS risk_signal, 70 AS risk_score, BUKRS, BELNR, GJAHR,\n             ROUND(MAX(DMBTR), 2) AS amount, 'High line item amount' AS reason\n      FROM BSEG ", bseg_where(), "\n      GROUP BY BUKRS, BELNR, GJAHR\n      HAVING MAX(DMBTR) >= 5000\n    "))
    round_items <- safe_query(db_path(), paste0("\n      SELECT 'Round-number item' AS risk_signal, 45 AS risk_score, BUKRS, BELNR, GJAHR,\n             ROUND(MAX(DMBTR), 2) AS amount, 'Amount divisible by 1000' AS reason\n      FROM BSEG ", bseg_where(), "\n      GROUP BY BUKRS, BELNR, GJAHR\n      HAVING MAX(CASE WHEN ABS(DMBTR - ROUND(DMBTR / 1000) * 1000) < 0.01 THEN 1 ELSE 0 END) = 1\n    "))

    weekend_where <- if (bkpf_where() == "") {
      "WHERE strftime('%w', BUDAT) IN ('0','6')"
    } else {
      paste0(bkpf_where(), " AND strftime('%w', BUDAT) IN ('0','6')")
    }

    weekend <- safe_query(db_path(), paste0("\n      SELECT 'Weekend posting' AS risk_signal, 55 AS risk_score, BUKRS, BELNR, GJAHR,\n             CAST(NULL AS REAL) AS amount, 'Posting date falls on Saturday/Sunday' AS reason\n      FROM BKPF ", weekend_where, "\n    "))

    normalize_risk_df <- function(x) {
      cols <- c("risk_signal", "risk_score", "BUKRS", "BELNR", "GJAHR", "amount", "reason")
      for (nm in cols) if (!nm %in% names(x)) x[[nm]] <- NA
      x <- x[, cols, drop = FALSE]
      x$risk_signal <- as.character(x$risk_signal)
      x$risk_score <- as.numeric(x$risk_score)
      x$BUKRS <- as.character(x$BUKRS)
      x$BELNR <- as.character(x$BELNR)
      x$GJAHR <- as.character(x$GJAHR)
      x$amount <- as.numeric(x$amount)
      x$reason <- as.character(x$reason)
      x
    }

    out <- bind_rows(
      normalize_risk_df(large_docs),
      normalize_risk_df(round_items),
      normalize_risk_df(weekend)
    )
    if (nrow(out) == 0) out else out %>% arrange(desc(risk_score), desc(coalesce(amount, 0))) %>% head(200)
  })

  browser_data <- reactive({ req(input$table_name); safe_query(db_path(), paste0("SELECT * FROM ", input$table_name, " LIMIT 500")) })

  output$selected_table_title <- renderText({ req(input$table_name); paste0(friendly_table(input$table_name), " (", input$table_name, ")") })
  output$tbl_field_dictionary <- renderDT({ req(input$table_name); make_dt(label_columns(field_dictionary(input$table_name, browser_data())), 20) })

  plot_monthly_obj <- reactive({
    df <- monthly(); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = month, y = documents, group = 1)) + geom_line(color = pal()$primary) + geom_point(color = pal()$accent) +
      labs(title = "Monthly posting trend", x = "Posting Date (BUDAT) month", y = "Documents") + base_plot_theme()
  })

  plot_ar_aging_obj <- reactive({
    df <- aging_query("BSID", ar_where()); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = aging_bucket, y = amount)) + geom_col(fill = pal()$fill) +
      labs(title = "AR open aging", x = "Aging Bucket", y = "Amount in Local Currency (DMBTR)") + base_plot_theme()
  })

  plot_ap_aging_obj <- reactive({
    df <- aging_query("BSIK", ap_where()); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = aging_bucket, y = amount)) + geom_col(fill = pal()$fill) +
      labs(title = "AP open aging", x = "Aging Bucket", y = "Amount in Local Currency (DMBTR)") + base_plot_theme()
  })

  plot_ar_customers_obj <- reactive({
    df <- head(ar_top(), 10); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = reorder(KUNNR, amount), y = amount)) + geom_col(fill = pal()$fill) + coord_flip() +
      labs(title = "Top AR customers", x = "Customer Number (KUNNR)", y = "Open amount") + base_plot_theme() + theme(axis.text.x = element_text(angle = 0))
  })

  plot_ap_vendors_obj <- reactive({
    df <- head(ap_top(), 10); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = reorder(LIFNR, amount), y = amount)) + geom_col(fill = pal()$fill) + coord_flip() +
      labs(title = "Top AP vendors", x = "Vendor Number (LIFNR)", y = "Open amount") + base_plot_theme() + theme(axis.text.x = element_text(angle = 0))
  })

  plot_gl_accounts_obj <- reactive({
    df <- gl_top(); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = reorder(HKONT, amount), y = amount)) + geom_col(fill = pal()$fill) + coord_flip() +
      labs(title = "Top open GL accounts", x = "G/L Account (HKONT)", y = "Amount in Local Currency (DMBTR)") + base_plot_theme() + theme(axis.text.x = element_text(angle = 0))
  })

  plot_gl_composition_obj <- reactive({
    df <- gl_composition(); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = reorder(gl_account_type, amount), y = amount)) + geom_col(fill = pal()$fill) + coord_flip() +
      labs(title = "GL composition by account type", x = "GL account type", y = "Amount") + base_plot_theme() + theme(axis.text.x = element_text(angle = 0))
  })

  plot_doc_types_obj <- reactive({
    df <- doc_types(); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = reorder(BLART, documents), y = documents)) + geom_col(fill = pal()$fill) + coord_flip() +
      labs(title = "Documents by type", x = "Document Type (BLART)", y = "Documents") + base_plot_theme() + theme(axis.text.x = element_text(angle = 0))
  })

  plot_debit_credit_obj <- reactive({
    df <- debit_credit(); if (nrow(df) == 0) return(empty_plot())
    ggplot(df, aes(x = SHKZG, y = amount)) + geom_col(fill = pal()$fill) +
      labs(title = "Debit / credit amount", x = "Debit/Credit Indicator (SHKZG)", y = "Amount") + base_plot_theme()
  })

  plot_risk_signals_obj <- reactive({
    df <- risk_signals(); if (nrow(df) == 0) return(empty_plot("No risk signals under current filters"))
    agg <- df %>% count(risk_signal, name = "signals")
    ggplot(agg, aes(x = reorder(risk_signal, signals), y = signals)) + geom_col(fill = pal()$fill) + coord_flip() +
      labs(title = "Risk signals by type", x = "Risk Signal", y = "Signals") + base_plot_theme() + theme(axis.text.x = element_text(angle = 0))
  })

  output$tbl_counts <- renderDT(make_dt(label_columns(row_counts()), 20))
  output$kpi_docs <- renderText(fmt_int(safe_query(db_path(), paste("SELECT COUNT(*) n FROM BKPF", bkpf_where()))$n))
  output$kpi_lines <- renderText(fmt_int(safe_query(db_path(), paste("SELECT COUNT(*) n FROM BSEG", bseg_where()))$n))
  output$kpi_customers <- renderText(fmt_int(safe_query(db_path(), "SELECT COUNT(*) n FROM KNA1")$n))
  output$kpi_vendors <- renderText(fmt_int(safe_query(db_path(), "SELECT COUNT(*) n FROM LFA1")$n))
  output$kpi_ar_amount <- renderText(fmt_amount(sum(ar_open()$DMBTR, na.rm = TRUE)))
  output$kpi_ap_amount <- renderText(fmt_amount(sum(ap_open()$DMBTR, na.rm = TRUE)))
  output$kpi_unbalanced <- renderText(fmt_int(nrow(unbalanced())))
  output$kpi_risk_signals <- renderText(fmt_int(nrow(risk_signals())))

  output$plot_monthly <- renderPlot(plot_monthly_obj())

  output$ar_open_count <- renderText(fmt_int(nrow(ar_open())))
  output$ar_open_amount <- renderText(fmt_amount(sum(ar_open()$DMBTR, na.rm = TRUE)))
  output$ar_cleared_count <- renderText(fmt_int(nrow(ar_cleared())))
  output$plot_ar_aging <- renderPlot(plot_ar_aging_obj())
  output$plot_ar_customers <- renderPlot(plot_ar_customers_obj())
  output$tbl_ar_top <- renderDT(make_dt(label_columns(ar_top()), 20))

  output$ap_open_count <- renderText(fmt_int(nrow(ap_open())))
  output$ap_open_amount <- renderText(fmt_amount(sum(ap_open()$DMBTR, na.rm = TRUE)))
  output$ap_cleared_count <- renderText(fmt_int(nrow(ap_cleared())))
  output$plot_ap_aging <- renderPlot(plot_ap_aging_obj())
  output$plot_ap_vendors <- renderPlot(plot_ap_vendors_obj())
  output$tbl_ap_top <- renderDT(make_dt(label_columns(ap_top()), 20))

  output$gl_open_count <- renderText(fmt_int(nrow(gl_open())))
  output$gl_debit_amount <- renderText(fmt_amount(sum(gl_open()$DMBTR[gl_open()$SHKZG == "S"], na.rm = TRUE)))
  output$gl_credit_amount <- renderText(fmt_amount(sum(gl_open()$DMBTR[gl_open()$SHKZG == "H"], na.rm = TRUE)))
  output$plot_gl_accounts <- renderPlot(plot_gl_accounts_obj())
  output$plot_gl_composition <- renderPlot(plot_gl_composition_obj())
  output$tbl_gl_top <- renderDT(make_dt(label_columns(gl_top()), 20))

  output$plot_doc_types <- renderPlot(plot_doc_types_obj())
  output$plot_debit_credit <- renderPlot(plot_debit_credit_obj())
  output$tbl_unbalanced <- renderDT(make_dt(label_columns(unbalanced()), 20))

  document_choices <- reactive({
    safe_query(db_path(), paste0("\n      SELECT BUKRS, BELNR, GJAHR, BLART, BUDAT\n      FROM BKPF ", bkpf_where(), "\n      ORDER BY BUDAT DESC, BELNR DESC LIMIT 500\n    "))
  })

  output$journal_doc_picker <- renderUI({
    df <- document_choices()
    if (nrow(df) == 0) return(helpText("No documents under current filters."))
    keys <- paste(df$BUKRS, df$BELNR, df$GJAHR, sep = "|")
    labels <- paste0(df$BELNR, " / ", df$BUKRS, " / ", df$GJAHR, " / ", df$BLART, " / ", df$BUDAT)
    selectizeInput("journal_doc", "Accounting Document", choices = setNames(keys, labels), selected = keys[[1]])
  })

  selected_doc_parts <- reactive({
    req(input$journal_doc)
    strsplit(input$journal_doc, "\\|", fixed = FALSE)[[1]]
  })

  journal_header <- reactive({
    p <- selected_doc_parts()
    safe_query(db_path(), sprintf("SELECT * FROM BKPF WHERE BUKRS = %s AND BELNR = %s AND GJAHR = %s", sql_quote(p[1]), sql_quote(p[2]), sql_quote(p[3])))
  })

  journal_lines <- reactive({
    p <- selected_doc_parts()
    safe_query(db_path(), sprintf("\n      SELECT *, CASE WHEN SHKZG = 'S' THEN DMBTR ELSE -DMBTR END AS signed_amount\n      FROM BSEG WHERE BUKRS = %s AND BELNR = %s AND GJAHR = %s\n      ORDER BY BUZEI\n    ", sql_quote(p[1]), sql_quote(p[2]), sql_quote(p[3])))
  })

  output$journal_balance <- renderText(fmt_amount(sum(journal_lines()$signed_amount, na.rm = TRUE)))
  output$journal_lines <- renderText(fmt_int(nrow(journal_lines())))
  output$tbl_journal_header <- renderDT(make_dt(label_columns(journal_header()), 5))
  output$tbl_journal_lines <- renderDT(make_dt(label_columns(journal_lines()), 10))

  output$risk_large_count <- renderText(fmt_int(sum(risk_signals()$risk_signal == "Large document", na.rm = TRUE)))
  output$risk_round_count <- renderText(fmt_int(sum(risk_signals()$risk_signal == "Round-number item", na.rm = TRUE)))
  output$risk_weekend_count <- renderText(fmt_int(sum(risk_signals()$risk_signal == "Weekend posting", na.rm = TRUE)))
  output$plot_risk_signals <- renderPlot(plot_risk_signals_obj())
  output$tbl_risk_signals <- renderDT(make_dt(label_columns(risk_signals()), 20))

  output$tbl_browser <- renderDT(make_dt(label_columns(browser_data()), 25))

  table_text_for_pdf <- function(df, max_rows = 14) {
    if (is.null(df) || nrow(df) == 0) return("No rows")
    df <- head(df, max_rows)
    df[] <- lapply(df, function(x) substr(as.character(x), 1, 24))
    paste(capture.output(print(df, row.names = FALSE)), collapse = "\n")
  }

  draw_dashboard_pdf_page <- function(page_title, plot_obj = NULL, table_title = NULL, table_df = NULL) {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 1, heights = unit(c(0.10, 0.53, 0.07, 0.30), c("npc", "npc", "npc", "npc")))))
    grid.text(page_title, x = unit(0.02, "npc"), y = unit(0.65, "npc"), just = c("left", "center"), gp = gpar(fontsize = 18, fontface = "bold", col = pal()$primary), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    if (!is.null(plot_obj)) print(plot_obj, vp = viewport(layout.pos.row = 2, layout.pos.col = 1)) else grid.text("No chart on this tab", gp = gpar(fontsize = 12, col = "grey40"), vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    if (!is.null(table_title)) grid.text(table_title, x = unit(0.02, "npc"), y = unit(0.5, "npc"), just = c("left", "center"), gp = gpar(fontsize = 12, fontface = "bold", col = pal()$primary), vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
    grid.text(table_text_for_pdf(table_df), x = unit(0.02, "npc"), y = unit(0.98, "npc"), just = c("left", "top"), gp = gpar(fontsize = 7.5, fontfamily = "mono"), vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
    popViewport()
    invisible(NULL)
  }

  render_report_page <- function(page) {
    if (page == "Overview") draw_dashboard_pdf_page("Overview", plot_monthly_obj(), "Row counts", label_columns(row_counts()))
    else if (page == "AR") draw_dashboard_pdf_page("Accounts Receivable", plot_ar_aging_obj(), "Top open AR customers", label_columns(ar_top()))
    else if (page == "AP") draw_dashboard_pdf_page("Accounts Payable", plot_ap_aging_obj(), "Top open AP vendors", label_columns(ap_top()))
    else if (page == "GL") draw_dashboard_pdf_page("General Ledger", plot_gl_accounts_obj(), "Top open GL accounts", label_columns(gl_top()))
    else if (page == "Documents") draw_dashboard_pdf_page("Documents", plot_doc_types_obj(), "Potentially unbalanced documents", label_columns(unbalanced()))
    else if (page == "Journal Explorer") draw_dashboard_pdf_page("Journal Explorer", NULL, "Selected document line items", label_columns(journal_lines()))
    else if (page == "Risk Signals") draw_dashboard_pdf_page("Risk Signals", plot_risk_signals_obj(), "Accounting risk signals", label_columns(risk_signals()))
    else if (page == "Data Browser") draw_dashboard_pdf_page(paste("Data Browser:", friendly_table(input$table_name), "(", input$table_name, ")"), NULL, "Selected table sample", label_columns(browser_data()))
  }

  output$export_current_pdf <- downloadHandler(
    filename = function() paste0("sap_fi_", gsub(" ", "_", tolower(input$main_tabs)), "_report.pdf"),
    content = function(file) { pdf(file, width = 11, height = 8.5); on.exit(dev.off(), add = TRUE); render_report_page(input$main_tabs) }
  )

  output$export_all_pdf <- downloadHandler(
    filename = function() "sap_fi_all_pages_report.pdf",
    content = function(file) {
      pdf(file, width = 11, height = 8.5); on.exit(dev.off(), add = TRUE)
      pages <- c("Overview", "AR", "AP", "GL", "Documents", "Journal Explorer", "Risk Signals", "Data Browser")
      for (pg in pages) render_report_page(pg)
    }
  )
}

shinyApp(ui, server)
