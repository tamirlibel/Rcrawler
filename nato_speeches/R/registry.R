NS_ROLES <- c(hos = "Head of state", mod = "Minister of Defence",
              mfa = "Minister of Foreign Affairs", chod = "Chief of Defence / armed forces leadership")
NS_TYPES <- c("html", "rss", "govuk_api", "none")
NS_COLUMNS <- c("source_id", "iso3", "country", "role", "office", "language", "type",
                "listing_url", "page_template", "max_pages", "link_regex", "speaker_regex",
                "filter", "js_required", "status", "notes",
                "title_xpath", "date_xpath", "body_xpath")

#' Load and validate the source registry (one row per country x role).
load_sources <- function(path = ns_path("config", "sources.csv")) {
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

#' Subset the registry by ISO3 country codes and/or roles. Sources marked
#' `type = none` (e.g. Iceland has no armed forces) are always dropped.
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
