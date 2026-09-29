#!/usr/bin/env Rscript
# Command-line interface to the natospeeches package. Find this file with
#   system.file("scripts", "natospeeches.R", package = "natospeeches")
# Examples:
#   Rscript natospeeches.R --check
#   Rscript natospeeches.R --countries=FRA,DEU --roles=hos,mfa --since=2025-01-01
#   Rscript natospeeches.R --export=speeches.csv
suppressPackageStartupMessages(library(natospeeches))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "(=|$)"), args, value = TRUE)
  if (!length(hit)) return(default)
  val <- sub(paste0("^--", name, "=?"), "", hit[1])
  if (nzchar(val)) val else TRUE
}
split_opt <- function(x) if (is.null(x)) NULL else strsplit(x, ",", fixed = TRUE)[[1]]

if (isTRUE(opt("help", FALSE))) {
  cat("Usage: Rscript natospeeches.R [options]\n",
      "  --check                   test every source and write source_check.csv\n",
      "  --export=FILE             write the corpus to FILE (.csv or .rds)\n",
      "  --countries=ISO3,...      e.g. FRA,DEU (default: all)\n",
      "  --roles=hos,mod,mfa,chod  (default: all)\n",
      "  --ids=SOURCE_ID,...       e.g. GBR_mod\n",
      "  --since=YYYY-MM-DD        drop speeches dated earlier\n",
      "  --max-pages=N --max-articles=N --delay=SECONDS\n",
      "  --data-dir=DIR            (default: natospeech_data)\n",
      "  --sources=FILE            edited copy of the registry CSV\n",
      "  --contact=EMAIL           added to the User-Agent\n", sep = "")
  quit(status = 0)
}

cfg <- ns_config(data_dir = opt("data-dir", "natospeech_data"),
                 contact = opt("contact", Sys.getenv("NATOSPEECH_CONTACT", "")),
                 delay = as.numeric(opt("delay", 3)))
sources <- load_sources(opt("sources", default_registry()))

if (isTRUE(opt("check", FALSE))) {
  report <- check_sources(split_opt(opt("countries")), split_opt(opt("roles")), split_opt(opt("ids")),
                          cfg = cfg, sources = sources)
  dir.create(cfg$data_dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(cfg$data_dir, "source_check.csv")
  utils::write.csv(report, f, row.names = FALSE)
  print(table(report$verdict))
  message("Full report: ", f)
} else if (!is.null(opt("export"))) {
  df <- export_speeches(opt("export"), cfg)
  message(nrow(df), " speeches written to ", opt("export"))
} else {
  s <- collect_speeches(countries = split_opt(opt("countries")), roles = split_opt(opt("roles")),
                        ids = split_opt(opt("ids")), since = opt("since"),
                        max_pages = if (!is.null(opt("max-pages"))) as.integer(opt("max-pages")),
                        max_articles = as.numeric(opt("max-articles", Inf)),
                        cfg = cfg, sources = sources)
  print(s)
}
