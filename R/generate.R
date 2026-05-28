# Synthetic data generators for SAP-style FI tables ----------------------------

pad_id <- function(prefix, n, width = 8) sprintf("%s%0*d", prefix, width, n)
rand_date <- function(start = as.Date("2024-01-01"), end = Sys.Date()) {
  start + sample.int(as.integer(end - start) + 1, 1) - 1
}
choice <- function(x, size = 1, prob = NULL) sample(x, size = size, replace = TRUE, prob = prob)

make_master_data <- function(n_customers, n_vendors, n_gl_accounts) {
  company_codes <- data.frame(
    MANDT = "800",
    BUKRS = c("1000", "2000"),
    BUTXT = c("Synthetic Poland Sp. z o.o.", "Synthetic Germany GmbH"),
    LAND1 = c("PL", "DE"),
    WAERS = c("PLN", "EUR"),
    stringsAsFactors = FALSE
  )

  countries <- c("PL", "DE", "CZ", "GB", "TR")
  customers <- data.frame(
    MANDT = "800",
    KUNNR = pad_id("C", seq_len(n_customers), 7),
    NAME1 = paste("Customer", seq_len(n_customers)),
    LAND1 = choice(countries, n_customers, prob = c(6, 2, rep(1, length(countries)-2))),
    ORT01 = choice(c("Warsaw", "London", "Berlin", "Prague", "Istanbul"), n_customers),
    PSTLZ = sprintf("%02d-%03d", sample(1:99, n_customers, TRUE), sample(1:999, n_customers, TRUE)),
    STRAS = paste("Synthetic Street", sample(1:250, n_customers, TRUE)),
    STCD1 = paste0("TAXC", sample(1000000:9999999, n_customers, TRUE)),
    KTOKD = choice(c("DEBI", "CPD"), n_customers, prob = c(0.95, 0.05)),
    ERDAT = as.character(Sys.Date() - sample(20:1500, n_customers, TRUE)),
    LOEVM = "",
    stringsAsFactors = FALSE
  )

  vendors <- data.frame(
    MANDT = "800",
    LIFNR = pad_id("V", seq_len(n_vendors), 7),
    NAME1 = paste("Vendor", seq_len(n_vendors)),
    LAND1 = choice(countries, n_vendors, prob = c(5, 2, rep(1, length(countries)-2))),
    ORT01 = choice(c("Warsaw", "London", "Berlin", "Prague", "Istanbul"), n_vendors),
    PSTLZ = sprintf("%02d-%03d", sample(1:99, n_vendors, TRUE), sample(1:999, n_vendors, TRUE)),
    STRAS = paste("Supplier Avenue", sample(1:250, n_vendors, TRUE)),
    STCD1 = paste0("TAXV", sample(1000000:9999999, n_vendors, TRUE)),
    KTOKK = choice(c("KRED", "CPD"), n_vendors, prob = c(0.95, 0.05)),
    ERDAT = as.character(Sys.Date() - sample(20:1500, n_vendors, TRUE)),
    LOEVM = "",
    stringsAsFactors = FALSE
  )

  base_gl <- data.frame(
    SAKNR = c("110000", "120000", "130000", "210000", "220000", "300000", "400000", "500000", "510000", "520000", "700000"),
    TXT50 = c("Trade receivables", "Bank account", "Cash on hand", "Trade payables", "Tax payable", "Equity", "Sales revenue", "Operating expense", "Payroll expense", "IT expense", "FX gain loss"),
    GLACCOUNT_TYPE = c("ASSET", "ASSET", "ASSET", "LIABILITY", "LIABILITY", "EQUITY", "REVENUE", "EXPENSE", "EXPENSE", "EXPENSE", "PNL"),
    XBILK = c("X", "X", "X", "X", "X", "X", "", "", "", "", ""),
    stringsAsFactors = FALSE
  )
  if (n_gl_accounts > nrow(base_gl)) {
    extra <- data.frame(
      SAKNR = as.character(seq(530000, by = 10, length.out = n_gl_accounts - nrow(base_gl))),
      TXT50 = paste("Synthetic expense", seq_len(n_gl_accounts - nrow(base_gl))),
      GLACCOUNT_TYPE = "EXPENSE",
      XBILK = "",
      stringsAsFactors = FALSE
    )
    base_gl <- rbind(base_gl, extra)
  }
  base_gl <- head(base_gl, n_gl_accounts)

  ska1 <- data.frame(MANDT = "800", KTOPL = "INT", base_gl, stringsAsFactors = FALSE)
  skb1 <- do.call(rbind, lapply(company_codes$BUKRS, function(bukrs) {
    data.frame(MANDT = "800", BUKRS = bukrs, SAKNR = base_gl$SAKNR,
               WAERS = company_codes$WAERS[company_codes$BUKRS == bukrs],
               XINTB = "", FSTAG = "G001", stringsAsFactors = FALSE)
  }))

  knb1 <- expand.grid(KUNNR = customers$KUNNR, BUKRS = company_codes$BUKRS, stringsAsFactors = FALSE)
  knb1 <- transform(knb1, MANDT = "800", AKONT = "110000", ZTERM = choice(c("0001", "0002", "0030"), nrow(knb1)), ZWELS = "T", FDGRV = "A1")
  knb1 <- knb1[, c("MANDT", "KUNNR", "BUKRS", "AKONT", "ZTERM", "ZWELS", "FDGRV")]

  lfb1 <- expand.grid(LIFNR = vendors$LIFNR, BUKRS = company_codes$BUKRS, stringsAsFactors = FALSE)
  lfb1 <- transform(lfb1, MANDT = "800", AKONT = "210000", ZTERM = choice(c("0001", "0002", "0030"), nrow(lfb1)), ZWELS = "T", FDGRV = "V1")
  lfb1 <- lfb1[, c("MANDT", "LIFNR", "BUKRS", "AKONT", "ZTERM", "ZWELS", "FDGRV")]

  list(T001 = company_codes, KNA1 = customers, KNB1 = knb1, LFA1 = vendors, LFB1 = lfb1, SKA1 = ska1, SKB1 = skb1)
}

make_documents <- function(master, n_documents) {
  bkpf <- list(); bseg <- list(); acdoca <- list()
  bsid <- list(); bsad <- list(); bsik <- list(); bsak <- list(); bsis <- list(); bsas <- list()
  belnr_seq <- 1000000000L
  line_id <- 1L

  add_doc <- function(kind) {
    bukrs <- choice(master$T001$BUKRS)
    waers <- master$T001$WAERS[master$T001$BUKRS == bukrs]
    budat <- rand_date()
    gjahr <- as.integer(format(budat, "%Y"))
    belnr <- as.character(belnr_seq + length(bkpf) + 1L)
    monat <- as.integer(format(budat, "%m"))
    amount <- round(exp(rnorm(1, log(1200), 0.9)), 2)
    cleared <- runif(1) < 0.62
    augbl <- if (cleared) as.character(as.integer(belnr) + 500000000L) else NA_character_
    augdt <- if (cleared) as.character(budat + sample(5:70, 1)) else NA_character_
    blart <- switch(kind, AR = "DR", AP = "KR", GL = "SA")

    bkpf[[length(bkpf) + 1L]] <<- data.frame(
      MANDT = "800", BUKRS = bukrs, BELNR = belnr, GJAHR = gjahr, BLART = blart,
      BLDAT = as.character(budat - sample(0:5, 1)), BUDAT = as.character(budat), MONAT = monat,
      CPUDT = as.character(Sys.Date()), CPUTM = format(Sys.time(), "%H%M%S"), WAERS = waers,
      KURSF = 1, XBLNR = paste0(kind, "-", belnr), BKTXT = paste("Synthetic", kind, "document"),
      USNAM = choice(c("BATCH_FI", "RFC_USER", "DEMO_USER")), TCODE = switch(kind, AR = "FB70", AP = "FB60", GL = "FB50"),
      stringsAsFactors = FALSE
    )

    if (kind == "AR") {
      kunnr <- choice(master$KNA1$KUNNR)
      lines <- data.frame(
        MANDT = "800", BUKRS = bukrs, BELNR = belnr, GJAHR = gjahr, BUZEI = 1:2,
        BSCHL = c("01", "50"), KOART = c("D", "S"), SHKZG = c("S", "H"),
        DMBTR = c(amount, amount), WRBTR = c(amount, amount), WAERS = waers,
        HKONT = c("110000", "400000"), KUNNR = c(kunnr, NA), LIFNR = NA,
        SGTXT = c("Customer invoice", "Revenue offset"), ZUONR = belnr,
        AUGBL = c(augbl, augbl), AUGDT = c(augdt, augdt), ZTERM = c("0030", NA),
        ZFBDT = as.character(budat), ZBD1T = c(30L, NA), PRCTR = choice(c("P100", "P200")),
        KOSTL = NA, MWSKZ = c("A1", "A1"), stringsAsFactors = FALSE
      )
      if (cleared) bsad[[length(bsad)+1L]] <<- lines[1, ] else bsid[[length(bsid)+1L]] <<- lines[1, ]
      if (cleared) bsas[[length(bsas)+1L]] <<- lines[2, ] else bsis[[length(bsis)+1L]] <<- lines[2, ]
    } else if (kind == "AP") {
      lifnr <- choice(master$LFA1$LIFNR)
      exp_acct <- choice(c("500000", "510000", "520000", setdiff(master$SKA1$SAKNR, c("110000", "210000", "400000"))))
      lines <- data.frame(
        MANDT = "800", BUKRS = bukrs, BELNR = belnr, GJAHR = gjahr, BUZEI = 1:2,
        BSCHL = c("40", "31"), KOART = c("S", "K"), SHKZG = c("S", "H"),
        DMBTR = c(amount, amount), WRBTR = c(amount, amount), WAERS = waers,
        HKONT = c(exp_acct, "210000"), KUNNR = NA, LIFNR = c(NA, lifnr),
        SGTXT = c("Expense posting", "Vendor invoice"), ZUONR = belnr,
        AUGBL = c(augbl, augbl), AUGDT = c(augdt, augdt), ZTERM = c(NA, "0030"),
        ZFBDT = as.character(budat), ZBD1T = c(NA, 30L), PRCTR = choice(c("P100", "P200")),
        KOSTL = choice(c("C100", "C200", "C300")), MWSKZ = c("V1", "V1"), stringsAsFactors = FALSE
      )
      if (cleared) bsak[[length(bsak)+1L]] <<- lines[2, ] else bsik[[length(bsik)+1L]] <<- lines[2, ]
      if (cleared) bsas[[length(bsas)+1L]] <<- lines[1, ] else bsis[[length(bsis)+1L]] <<- lines[1, ]
    } else {
      acct1 <- choice(c("120000", "130000", "500000", "700000"))
      acct2 <- choice(c("300000", "400000", "210000", "120000"))
      lines <- data.frame(
        MANDT = "800", BUKRS = bukrs, BELNR = belnr, GJAHR = gjahr, BUZEI = 1:2,
        BSCHL = c("40", "50"), KOART = c("S", "S"), SHKZG = c("S", "H"),
        DMBTR = c(amount, amount), WRBTR = c(amount, amount), WAERS = waers,
        HKONT = c(acct1, acct2), KUNNR = NA, LIFNR = NA,
        SGTXT = c("GL debit", "GL credit"), ZUONR = belnr,
        AUGBL = c(augbl, augbl), AUGDT = c(augdt, augdt), ZTERM = NA,
        ZFBDT = as.character(budat), ZBD1T = NA, PRCTR = choice(c("P100", "P200")),
        KOSTL = choice(c("C100", "C200", "C300")), MWSKZ = NA, stringsAsFactors = FALSE
      )
      if (cleared) bsas[[length(bsas)+1L]] <<- lines else bsis[[length(bsis)+1L]] <<- lines
    }

    bseg[[length(bseg) + 1L]] <<- lines
    acdoca[[length(acdoca) + 1L]] <<- transform(lines,
      RCLNT = MANDT, RLDNR = "0L", RBUKRS = BUKRS, DOCLN = BUZEI, RACCT = HKONT,
      DRCRK = SHKZG, HSL = ifelse(SHKZG == "S", DMBTR, -DMBTR), WSL = ifelse(SHKZG == "S", WRBTR, -WRBTR),
      RHCUR = WAERS, RWCUR = WAERS, BUDAT = as.character(budat), BLART = blart,
      RCNTR = KOSTL
    )[, c("RCLNT", "RLDNR", "RBUKRS", "GJAHR", "BELNR", "DOCLN", "RACCT", "KOART", "DRCRK", "HSL", "WSL", "RHCUR", "RWCUR", "BUDAT", "BLART", "SGTXT", "PRCTR", "RCNTR", "KUNNR", "LIFNR")]
  }

  kinds <- choice(c("AR", "AP", "GL"), n_documents, prob = c(0.35, 0.35, 0.30))
  for (k in kinds) add_doc(k)

  bind_or_empty <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
  list(BKPF = bind_or_empty(bkpf), BSEG = bind_or_empty(bseg), ACDOCA = bind_or_empty(acdoca),
       BSID = bind_or_empty(bsid), BSAD = bind_or_empty(bsad), BSIK = bind_or_empty(bsik), BSAK = bind_or_empty(bsak),
       BSIS = bind_or_empty(bsis), BSAS = bind_or_empty(bsas))
}

insert_data <- function(db_path, master, docs) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  for (nm in names(master)) DBI::dbWriteTable(con, nm, master[[nm]], append = TRUE)
  for (nm in names(docs)) if (nrow(docs[[nm]]) > 0) DBI::dbWriteTable(con, nm, docs[[nm]], append = TRUE)

  invisible(TRUE)
}

generate_everything <- function(db_path = "data/sap_fi.sqlite", n_customers = 500, n_vendors = 350,
                                n_gl_accounts = 20, n_documents = 5000, seed = 42) {
  set.seed(seed)
  master <- make_master_data(n_customers, n_vendors, n_gl_accounts)
  docs <- make_documents(master, n_documents)
  insert_data(db_path, master, docs)
  c(master_counts = length(master), customers = nrow(master$KNA1), vendors = nrow(master$LFA1),
    gl_accounts = nrow(master$SKA1), documents = nrow(docs$BKPF), line_items = nrow(docs$BSEG),
    ar_open = nrow(docs$BSID), ar_cleared = nrow(docs$BSAD), ap_open = nrow(docs$BSIK),
    ap_cleared = nrow(docs$BSAK), gl_open = nrow(docs$BSIS), gl_cleared = nrow(docs$BSAS))
}
