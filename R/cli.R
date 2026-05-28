#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

source("R/schema.R")
source("R/generate.R")

get_arg <- function(args, name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}

cmd <- commandArgs(trailingOnly = TRUE)

if (length(cmd) == 0) {
  stop(
    paste(
      "Usage:",
      "Rscript R/cli.R init-db --db=data/sap_fi.sqlite",
      "Rscript R/cli.R generate --db=data/sap_fi.sqlite --n-documents=5000 --seed=42",
      "Rscript R/cli.R list-tables --db=data/sap_fi.sqlite",
      "Rscript R/cli.R smoke-test --db=data/sap_fi.sqlite",
      sep = "\n"
    )
  )
}

verb <- cmd[1]
db_path <- get_arg(cmd, "db", "data/sap_fi.sqlite")

if (verb == "init-db") {
  create_db(db_path)
  cat("Initialized schema at", db_path, "\n")

} else if (verb == "generate") {
  if (!file.exists(db_path)) create_db(db_path)

  stats <- generate_everything(
    db_path = db_path,
    n_customers = as.integer(get_arg(cmd, "n-customers", "500")),
    n_vendors = as.integer(get_arg(cmd, "n-vendors", "350")),
    n_gl_accounts = as.integer(get_arg(cmd, "n-gl-accounts", "20")),
    n_documents = as.integer(get_arg(cmd, "n-documents", "5000")),
    seed = as.integer(get_arg(cmd, "seed", "42"))
  )

  print(stats)

} else if (verb == "list-tables") {
  if (!file.exists(db_path)) stop("DB not found: ", db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  print(DBI::dbListTables(con))

} else if (verb == "smoke-test") {
  if (!file.exists(db_path)) stop("DB not found: ", db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  checks <- c(
    "T001", "KNA1", "KNB1", "LFA1", "LFB1", "SKA1", "SKB1",
    "BKPF", "BSEG", "BSID", "BSAD", "BSIK", "BSAK", "BSIS", "BSAS", "ACDOCA"
  )

  existing <- DBI::dbListTables(con)
  missing <- setdiff(checks, existing)

  if (length(missing) > 0) {
    stop("Smoke test failed. Missing tables: ", paste(missing, collapse = ", "))
  }

  counts <- sapply(checks, function(tbl) {
    DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", tbl))$n
  })

  print(counts)

  if (counts[["BKPF"]] == 0 || counts[["BSEG"]] == 0) {
    stop("Smoke test failed: no FI documents generated")
  }

  cat("Smoke test passed\n")

} else {
  stop("Unknown command: ", verb)
}
