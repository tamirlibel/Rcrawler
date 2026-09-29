# Load the NATO speech collector into the current R session:
#   source("nato_speeches/load.R")
local({
  for (p in c("httr", "xml2", "jsonlite", "digest")) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Please install.packages('", p, "')")
  }
  # Non-ASCII month names and speech keywords need a UTF-8 locale.
  if (!isTRUE(l10n_info()$`UTF-8`)) {
    for (loc in c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8")) {
      if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
    }
  }
  this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
  root <- if (!is.null(this_file)) dirname(this_file) else getOption("natospeech.root", "nato_speeches")
  options(natospeech.root = root)
  for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
    sys.source(f, envir = globalenv(), keep.source = FALSE, toplevel.env = globalenv())
  }
})
