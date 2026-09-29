# Run from the repository root:  Rscript nato_speeches/tests/testthat.R
library(testthat)
root <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), ".."))
options(natospeech.root = root, natospeech.quiet = TRUE)
source(file.path(root, "load.R"))
test_dir(file.path(root, "tests", "testthat"), stop_on_failure = TRUE)
