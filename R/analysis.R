# R/analysis.R
# Basic EDA for SAP Synthetic Data Generator
# Usage:
#   Rscript R/analysis.R --db=data/sap_fi.sqlite --out=outputs/eda

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- grep(paste0('^--', name, '='), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0('^--', name, '='), '', hit[[1]])
}

db_path <- get_arg('db', 'data/sap_fi.sqlite')
out_dir <- get_arg('out', 'outputs/eda')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

cat('Connected to:', db_path, '\n\n')

tables <- dbListTables(con)
cat('Tables found:\n')
print(tables)
cat('\n')

# ---------- helpers ----------
write_csv <- function(df, name) {
  path <- file.path(out_dir, paste0(name, '.csv'))
  write.csv(df, path, row.names = FALSE)
  cat('Wrote:', path, '\n')
}

run_query <- function(sql) {
  dbGetQuery(con, sql)
}

# ---------- 1. row counts ----------
row_counts <- do.call(rbind, lapply(tables, function(tbl) {
  data.frame(table_name = tbl,
             row_count = dbGetQuery(con, paste0('SELECT COUNT(*) AS n FROM ', tbl))$n)
}))
row_counts <- row_counts[order(-row_counts$row_count), ]
print(row_counts)
write_csv(row_counts, '01_row_counts')

# ---------- 2. document and line item structure ----------
doc_summary <- run_query("
SELECT
  COUNT(*) AS documents,
  COUNT(DISTINCT BUKRS) AS company_codes,
  MIN(BUDAT) AS min_posting_date,
  MAX(BUDAT) AS max_posting_date
FROM BKPF
")
print(doc_summary)
write_csv(doc_summary, '02_document_summary')

line_summary <- run_query("
SELECT
  COUNT(*) AS line_items,
  COUNT(DISTINCT BELNR) AS documents_with_lines,
  ROUND(1.0 * COUNT(*) / COUNT(DISTINCT BELNR), 2) AS avg_lines_per_document,
  SUM(DMBTR) AS total_local_amount
FROM BSEG
")
print(line_summary)
write_csv(line_summary, '03_line_item_summary')

# ---------- 3. documents by type ----------
docs_by_type <- run_query("
SELECT BLART, COUNT(*) AS documents
FROM BKPF
GROUP BY BLART
ORDER BY documents DESC
")
print(docs_by_type)
write_csv(docs_by_type, '04_documents_by_type')

# ---------- 4. monthly posting trend ----------
monthly_trend <- run_query("
SELECT
  substr(BUDAT, 1, 7) AS posting_month,
  COUNT(*) AS documents
FROM BKPF
GROUP BY substr(BUDAT, 1, 7)
ORDER BY posting_month
")
print(monthly_trend)
write_csv(monthly_trend, '05_monthly_posting_trend')

# ---------- 5. debit / credit balance check ----------
balance_check <- run_query("
SELECT
  b.BUKRS,
  b.BELNR,
  b.GJAHR,
  ROUND(SUM(CASE WHEN s.SHKZG = 'S' THEN s.DMBTR ELSE -s.DMBTR END), 2) AS signed_balance
FROM BKPF b
JOIN BSEG s
  ON b.MANDT = s.MANDT
 AND b.BUKRS = s.BUKRS
 AND b.BELNR = s.BELNR
 AND b.GJAHR = s.GJAHR
GROUP BY b.BUKRS, b.BELNR, b.GJAHR
HAVING ABS(signed_balance) > 0.01
ORDER BY ABS(signed_balance) DESC
LIMIT 20
")
print(balance_check)
write_csv(balance_check, '06_unbalanced_documents_sample')

# ---------- 6. AR/AP/GL open vs cleared ----------
open_cleared <- run_query("
SELECT 'AR_OPEN_BSID' AS bucket, COUNT(*) AS rows, ROUND(SUM(DMBTR), 2) AS amount FROM BSID
UNION ALL SELECT 'AR_CLEARED_BSAD', COUNT(*), ROUND(SUM(DMBTR), 2) FROM BSAD
UNION ALL SELECT 'AP_OPEN_BSIK', COUNT(*), ROUND(SUM(DMBTR), 2) FROM BSIK
UNION ALL SELECT 'AP_CLEARED_BSAK', COUNT(*), ROUND(SUM(DMBTR), 2) FROM BSAK
UNION ALL SELECT 'GL_OPEN_BSIS', COUNT(*), ROUND(SUM(DMBTR), 2) FROM BSIS
UNION ALL SELECT 'GL_CLEARED_BSAS', COUNT(*), ROUND(SUM(DMBTR), 2) FROM BSAS
")
print(open_cleared)
write_csv(open_cleared, '07_open_cleared_summary')

# ---------- 7. top customers / vendors / GL accounts ----------
top_customers <- run_query("
SELECT KUNNR, COUNT(*) AS open_items, ROUND(SUM(DMBTR), 2) AS open_amount
FROM BSID
GROUP BY KUNNR
ORDER BY open_amount DESC
LIMIT 20
")
print(top_customers)
write_csv(top_customers, '08_top_ar_customers_open')

top_vendors <- run_query("
SELECT LIFNR, COUNT(*) AS open_items, ROUND(SUM(DMBTR), 2) AS open_amount
FROM BSIK
GROUP BY LIFNR
ORDER BY open_amount DESC
LIMIT 20
")
print(top_vendors)
write_csv(top_vendors, '09_top_ap_vendors_open')

top_gl <- run_query("
SELECT HKONT, COUNT(*) AS line_items, ROUND(SUM(DMBTR), 2) AS amount
FROM BSEG
WHERE HKONT IS NOT NULL
GROUP BY HKONT
ORDER BY amount DESC
LIMIT 20
")
print(top_gl)
write_csv(top_gl, '10_top_gl_accounts')

# ---------- 8. simple aging buckets for open AR/AP ----------
ar_aging <- run_query("
SELECT
  CASE
    WHEN julianday('now') - julianday(ZFBDT) <= 30 THEN '000-030'
    WHEN julianday('now') - julianday(ZFBDT) <= 60 THEN '031-060'
    WHEN julianday('now') - julianday(ZFBDT) <= 90 THEN '061-090'
    ELSE '090+'
  END AS aging_bucket,
  COUNT(*) AS open_items,
  ROUND(SUM(DMBTR), 2) AS amount
FROM BSID
GROUP BY aging_bucket
ORDER BY aging_bucket
")
print(ar_aging)
write_csv(ar_aging, '11_ar_open_aging')

ap_aging <- run_query("
SELECT
  CASE
    WHEN julianday('now') - julianday(ZFBDT) <= 30 THEN '000-030'
    WHEN julianday('now') - julianday(ZFBDT) <= 60 THEN '031-060'
    WHEN julianday('now') - julianday(ZFBDT) <= 90 THEN '061-090'
    ELSE '090+'
  END AS aging_bucket,
  COUNT(*) AS open_items,
  ROUND(SUM(DMBTR), 2) AS amount
FROM BSIK
GROUP BY aging_bucket
ORDER BY aging_bucket
")
print(ap_aging)
write_csv(ap_aging, '12_ap_open_aging')

# ---------- 9. quick base-R plots ----------
plot_png <- function(filename, expr) {
  path <- file.path(out_dir, filename)
  png(path, width = 1000, height = 650)
  on.exit(dev.off(), add = TRUE)
  force(expr)
  cat('Wrote:', path, '\n')
}

plot_png('plot_01_documents_by_type.png', {
  barplot(docs_by_type$documents,
          names.arg = docs_by_type$BLART,
          main = 'Documents by SAP Document Type',
          xlab = 'BLART', ylab = 'Documents')
})

plot_png('plot_02_monthly_trend.png', {
  plot(seq_len(nrow(monthly_trend)), monthly_trend$documents,
       type = 'b', xaxt = 'n',
       main = 'Monthly Posting Trend',
       xlab = 'Posting Month', ylab = 'Documents')
  axis(1, at = seq_len(nrow(monthly_trend)), labels = monthly_trend$posting_month, las = 2)
})

plot_png('plot_03_open_cleared_amount.png', {
  barplot(open_cleared$amount,
          names.arg = open_cleared$bucket,
          las = 2,
          main = 'Open vs Cleared Amounts',
          ylab = 'Amount')
})

cat('\nEDA completed. Outputs written to:', out_dir, '\n')
