NS_ROLES <- c(hos = "Head of state", mod = "Minister of Defence",
              mfa = "Minister of Foreign Affairs", chod = "Chief of Defence / armed forces leadership")
NS_TYPES <- c("html", "rss", "govuk_api", "none")
NS_COLUMNS <- c("source_id", "iso3", "country", "role", "office", "language", "type",
                "listing_url", "page_template", "max_pages", "link_regex", "speaker_regex",
                "filter", "js_required", "status", "notes",
                "title_xpath", "date_xpath", "body_xpath")

#' Source registry
#'
#' The registry lists, for each NATO member and role, where that office-holder's
#' speeches are published and how to recognise them. The bundled copy is at
#' `default_registry()`. To fix or extend it, copy it, edit the copy, and pass
#' the edited data to the `sources` argument of [collect_speeches()] and
#' [check_sources()].
#'
#' Columns:
#' \describe{
#'   \item{source_id}{unique id, `ISO3_role`.}
#'   \item{iso3, country}{member state.}
#'   \item{role}{`hos` head of state, `mod` minister of defence, `mfa` minister
#'     of foreign affairs, `chod` chief of defence.}
#'   \item{office, language}{official title and main language of the source.}
#'   \item{type}{`html` listing page, `rss` feed, `govuk_api` GOV.UK search API,
#'     or `none` (office does not exist).}
#'   \item{listing_url}{first page of the speech or news listing.}
#'   \item{page_template}{suffix for later pages, with `{page}` (page number)
#'     or `{offset}` (item offset; page size taken from `count=` in the URL,
#'     else 10).}
#'   \item{max_pages}{listing pages to read per run.}
#'   \item{link_regex}{optional regex (PCRE) that article URLs must match.}
#'   \item{speaker_regex}{optional regex that the title or opening text must
#'     match; separates office-holders who share a listing.}
#'   \item{filter}{`all` if the listing holds only speeches, `keywords` if a
#'     speech word must appear in the title or URL.}
#'   \item{js_required}{listing is built by JavaScript; see [chromote_fetcher()].}
#'   \item{status}{`unverified`, `verified` or `na`.}
#'   \item{notes}{free text.}
#'   \item{title_xpath, date_xpath, body_xpath}{optional per-site XPath
#'     overrides for extraction.}
#' }
#'
#' @param path CSV file in the registry format.
#' @return `load_sources()` returns a validated data.frame;
#'   `default_registry()` returns the path to the bundled CSV.
#' @export
#' @examples
#' src <- load_sources()
#' table(src$role)
#' src[src$iso3 == "FRA", c("source_id", "office", "listing_url")]
#'
#' # Edit a copy of the registry
#' my_registry <- file.path(tempdir(), "sources.csv")
#' file.copy(default_registry(), my_registry)
#' my_sources <- load_sources(my_registry)
load_sources <- function(path = default_registry()) {
  src <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                         colClasses = "character", encoding = "UTF-8")
  missing_cols <- setdiff(NS_COLUMNS, names(src))
  if (length(missing_cols)) stop("sources.csv is missing columns: ", paste(missing_cols, collapse = ", "))

  src$max_pages <- as.integer(src$max_pages %||% 1)
  src$max_pages[is.na(src$max_pages)] <- 1L
  src$js_required <- toupper(src$js_required) %in% "TRUE"
  src$filter[is.na(src$filter)] <- "keywords"
  src$status[is.na(src$status)] <- "unverified"

  problems <- c(
    if (anyDuplicated(src$source_id)) paste("duplicate source_id:", paste(src$source_id[duplicated(src$source_id)], collapse = ", ")),
    if (any(!src$role %in% names(NS_ROLES))) paste("unknown role in:", paste(src$source_id[!src$role %in% names(NS_ROLES)], collapse = ", ")),
    if (any(!src$type %in% NS_TYPES)) paste("unknown type in:", paste(src$source_id[!src$type %in% NS_TYPES], collapse = ", ")),
    if (any(!src$filter %in% c("all", "keywords"))) "filter must be 'all' or 'keywords'",
    if (any(src$type != "none" & is.na(src$listing_url))) paste("missing listing_url in:", paste(src$source_id[src$type != "none" & is.na(src$listing_url)], collapse = ", "))
  )
  bad_regex <- Filter(function(r) !is.na(r) && inherits(try(grepl(r, "", perl = TRUE), silent = TRUE), "try-error"),
                      c(src$link_regex, src$speaker_regex))
  if (length(bad_regex)) problems <- c(problems, paste("invalid regex:", paste(bad_regex, collapse = " ; ")))
  if (length(problems)) stop("Invalid sources.csv:\n- ", paste(problems, collapse = "\n- "))
  src
}

#' @rdname load_sources
#' @export
default_registry <- function() system.file("extdata", "sources.csv", package = "natospeeches", mustWork = TRUE)

#' Select registry rows
#'
#' Subsets the registry by country, role or source id. Rows with
#' `type = none` (offices that do not exist, e.g. Iceland has no armed forces)
#' are always dropped.
#'
#' @param sources registry from [load_sources()].
#' @param countries ISO3 codes, e.g. `c("FRA", "DEU")`. `NULL` for all.
#' @param roles any of `"hos"`, `"mod"`, `"mfa"`, `"chod"`. `NULL` for all.
#' @param ids source ids, e.g. `"GBR_mod"`. `NULL` for all.
#' @return the matching rows.
#' @export
#' @examples
#' select_sources(load_sources(), countries = c("POL", "LTU"), roles = "chod")
select_sources <- function(sources, countries = NULL, roles = NULL, ids = NULL) {
  keep <- sources$type != "none"
  if (length(countries)) keep <- keep & sources$iso3 %in% toupper(countries)
  if (length(roles)) keep <- keep & sources$role %in% tolower(roles)
  if (length(ids)) keep <- keep & sources$source_id %in% ids
  sources[keep, , drop = FALSE]
}

# Listing URL for page n (1-based). `{page}` is the page number, `{offset}`
# the item offset; page size is read from a `count=` query parameter, else 10.
listing_page_url <- function(source, page) {
  if (page == 1 || ns_blank(source$page_template)) return(source$listing_url)
  size <- suppressWarnings(as.integer(sub(".*[?&]count=([0-9]+).*", "\\1", source$listing_url)))
  if (is.na(size)) size <- 10L
  suffix <- gsub("{page}", page, source$page_template, fixed = TRUE)
  suffix <- gsub("{offset}", (page - 1) * size, suffix, fixed = TRUE)
  # A template beginning with ?, & or a path segment is appended to the listing URL.
  base <- source$listing_url
  if (startsWith(suffix, "?") && grepl("?", base, fixed = TRUE)) suffix <- sub("^\\?", "&", suffix)
  if (!startsWith(suffix, "?") && !startsWith(suffix, "&") && !endsWith(base, "/")) base <- paste0(base, "/")
  paste0(base, suffix)
}
