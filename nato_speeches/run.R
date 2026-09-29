#!/usr/bin/env Rscript
# Command-line entry point. Run from the repository root, e.g.
#   Rscript nato_speeches/run.R --check
#   Rscript nato_speeches/run.R --countries=FRA,DEU --roles=hos,mfa --since=2025-01-01
#   Rscript nato_speeches/run.R --export=speeches.csv
args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "(=|$)"), args, value = TRUE)
  if (!length(hit)) return(default)
  val <- sub(paste0("^--", name, "=?"), "", hit[1])
  if (nzchar(val)) val else TRUE
}
split_opt <- function(x) if (is.null(x)) NULL else strsplit(x, ",", fixed = TRUE)[[1]]

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
options(natospeech.root = normalizePath(dirname(script)))
source(file.path(getOption("natospeech.root"), "load.R"))

cfg <- ns_config(data_dir = opt("data-dir", ns_path("data")),
                 delay = as.numeric(opt("delay", 3)),
                 obey_robots = !isTRUE(opt("ignore-robots", FALSE)))

if (isTRUE(opt("help", FALSE))) {
  cat("Options: --check | --export=FILE(.csv|.rds) | --countries=ISO3,... --roles=hos,mod,mfa,chod",
      "--ids=SOURCE_ID,... --since=YYYY-MM-DD --max-pages=N --max-articles=N --delay=SECONDS --data-dir=DIR\n")
} else if (isTRUE(opt("check", FALSE))) {
  report <- check_sources(split_opt(opt("countries")), split_opt(opt("roles")), split_opt(opt("ids")), cfg = cfg)
  f <- file.path(cfg$data_dir, "source_check.csv")
  dir.create(cfg$data_dir, showWarnings = FALSE, recursive = TRUE)
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
                        max_articles = as.numeric(opt("max-articles", Inf)), cfg = cfg)
  print(s)
}
