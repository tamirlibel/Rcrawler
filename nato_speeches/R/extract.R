# XPath 1.0 test: does @attr contain `token` as a word (split on space, - and _)?
xp_token <- function(attr, tokens) {
  norm <- sprintf("concat(' ', translate(%s, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ-_', 'abcdefghijklmnopqrstuvwxyz  '), ' ')", attr)
  paste(sprintf("contains(%s, ' %s ')", norm, tokens), collapse = " or ")
}

NS_BOILERPLATE_XPATH <- paste(
  "//script", "//style", "//noscript", "//nav", "//footer", "//form", "//aside", "//iframe", "//svg",
  "//*[@role='navigation' or @role='contentinfo' or @role='banner']",
  sprintf("//*[%s]", xp_token("@class", c("cookie", "cookies", "share", "sharing", "social", "breadcrumb", "breadcrumbs",
                                          "menu", "navbar", "related", "newsletter", "sidebar", "skip", "pagination", "tags"))),
  sprintf("//*[%s]", xp_token("@id", c("cookie", "share", "social", "breadcrumb", "breadcrumbs", "menu",
                                       "related", "sidebar", "footer", "header"))),
  sep = " | ")

NS_BODY_CANDIDATES <- c(
  "//*[@itemprop='articleBody']", "//article", "//main", "//*[@role='main']",
  sprintf("//*[%s]", xp_token("@class", c("article", "content", "entry", "post", "text", "body", "speech")))
)

#' Extract a speech from a web page or PDF
#'
#' `extract_article()` pulls the title, date, language and main text out of
#' an HTML page. The title comes from `og:title`, then `<h1>`, then
#' `<title>`. The date comes from metadata (`article:published_time` and
#' similar, JSON-LD `datePublished`, `<time datetime>`), falling back to the
#' first date written in the text. The body is the page's main content
#' container with navigation, cookie banners, share buttons and similar
#' removed. Per-site XPath overrides in the registry (`title_xpath`,
#' `date_xpath`, `body_xpath`) take precedence.
#'
#' `extract_pdf()` does the same for a PDF (needs \pkg{pdftools}).
#'
#' @param html page source as a single string.
#' @param url the page URL, used to resolve relative links.
#' @param source a registry row (as a list) for XPath overrides and the
#'   fallback language; optional.
#' @return a list with `title`, `date` (`YYYY-MM-DD` or `NA`), `date_source`,
#'   `language`, `text` (paragraphs separated by blank lines) and `pdf_links`.
#' @export
#' @examples
#' html <- '<html lang="en"><head><title>Speech by the Minister</title>
#'   <meta property="article:published_time" content="2025-06-24T10:00:00Z">
#'   </head><body><nav>Home | News</nav><article><h1>Speech by the Minister</h1>
#'   <p>Colleagues, it is an honour to address you today.</p></article></body></html>'
#' str(extract_article(html, "https://example.org/speech"))
extract_article <- function(html, url, source = list()) {
  doc <- xml2::read_html(html)

  title <- first_text(doc, source$title_xpath) %||%
    meta_content(doc, c("og:title", "twitter:title", "dc.title")) %||%
    first_text(doc, "//h1") %||% first_text(doc, "//title")

  lang <- xml2::xml_attr(xml2::xml_find_first(doc, "/html"), "lang")
  if (is.na(lang)) lang <- source$language %||% NA_character_
  date <- extract_date(doc, source$date_xpath, lang)

  # Strip boilerplate only after title/date metadata has been read.
  xml2::xml_remove(xml2::xml_find_all(doc, NS_BOILERPLATE_XPATH))
  body_node <- if (!ns_blank(source$body_xpath)) xml2::xml_find_first(doc, source$body_xpath) else best_body_node(doc)
  text <- node_text(body_node)
  if (is.na(date$date)) {
    # Dates often sit in a byline outside the paragraphs, so search raw text.
    raw <- if (inherits(body_node, "xml_missing")) "" else ns_squish(xml2::xml_text(body_node))
    date <- date_from_text(substr(raw, 1, 1500), lang)
    if (is.na(date$date)) date <- date_from_text(substr(ns_squish(xml2::xml_text(doc)), 1, 3000), lang)
  }

  pdfs <- if (!inherits(body_node, "xml_missing")) {
    h <- xml2::xml_attr(xml2::xml_find_all(body_node, ".//a[contains(translate(@href,'PDF','pdf'),'.pdf')]"), "href")
    unique(xml2::url_absolute(h, url))
  } else character()

  list(title = ns_squish(title %||% NA_character_), date = date$date, date_source = date$source,
       language = lang %||% NA_character_, text = text, pdf_links = pdfs)
}

#' @rdname extract_article
#' @param raw the PDF file as a raw vector.
#' @param title title to use (e.g. the text of the link to the PDF); if
#'   missing, the PDF's metadata title or first line is used.
#' @param lang language code, used to read month names in dates.
#' @export
extract_pdf <- function(raw, title = NA, lang = NA_character_) {
  pages <- pdftools::pdf_text(raw)
  paras <- ns_squish(unlist(strsplit(paste(pages, collapse = "\n\n"), "\n[[:blank:]]*\n")))
  paras <- paras[nzchar(paras)]
  text <- if (length(paras)) paste(paras, collapse = "\n\n") else NA_character_
  info <- tryCatch(pdftools::pdf_info(raw), error = function(e) NULL)
  if (ns_blank(title)) title <- info$keys$Title %||% (if (length(paras)) substr(paras[1], 1, 200) else NA_character_)
  date <- date_from_text(substr(text %||% "", 1, 1500), lang)
  if (is.na(date$date) && !is.null(info$created) && !is.na(info$created)) {
    date <- list(date = format(as.Date(info$created)), source = "pdf_created")
  }
  list(title = ns_squish(title), date = date$date, date_source = date$source,
       language = lang, text = text, pdf_links = character())
}

first_text <- function(doc, xpath) {
  if (ns_blank(xpath)) return(NULL)
  n <- xml2::xml_find_first(doc, xpath)
  if (inherits(n, "xml_missing")) return(NULL)
  t <- ns_squish(xml2::xml_text(n))
  if (nzchar(t)) t else NULL
}

meta_content <- function(doc, names) {
  for (nm in names) {
    n <- xml2::xml_find_first(doc, sprintf(
      "//meta[translate(@property,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='%1$s' or translate(@name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='%1$s' or @itemprop='%1$s']", nm))
    v <- xml2::xml_attr(n, "content")
    if (!is.na(v) && nzchar(trimws(v))) return(trimws(v))
  }
  NULL
}

# Pick the candidate container with the most paragraph text; fall back to the
# element that directly holds the most paragraph text (a readability-style score).
best_body_node <- function(doc) {
  score <- function(n) sum(nchar(ns_squish(xml2::xml_text(xml2::xml_find_all(n, ".//p")))))
  cands <- unlist(lapply(NS_BODY_CANDIDATES, function(x) as.list(xml2::xml_find_all(doc, x))), recursive = FALSE)
  best <- NULL; best_score <- 0
  for (n in cands) {
    s <- score(n)
    if (s > best_score) { best <- n; best_score <- s }
  }
  if (!is.null(best) && best_score >= 200) return(best)
  ps <- xml2::xml_find_all(doc, "//p")
  if (!length(ps)) return(xml2::xml_find_first(doc, "//body"))
  key <- vapply(ps, function(p) xml2::xml_path(xml2::xml_parent(p)), character(1))
  lens <- tapply(nchar(ns_squish(xml2::xml_text(ps))), key, sum)
  xml2::xml_find_first(doc, names(lens)[which.max(lens)])
}

node_text <- function(node) {
  if (is.null(node) || inherits(node, "xml_missing")) return(NA_character_)
  blocks <- xml2::xml_find_all(node, ".//*[self::p or self::h2 or self::h3 or self::h4 or self::li or self::blockquote][not(ancestor::p) and not(ancestor::li) and not(ancestor::blockquote)]")
  txt <- ns_squish(xml2::xml_text(blocks))
  txt <- txt[nzchar(txt)]
  if (!length(txt)) txt <- ns_squish(xml2::xml_text(node))
  paste(txt[!duplicated(txt) | nchar(txt) > 80], collapse = "\n\n")
}

# ---- dates -----------------------------------------------------------------

extract_date <- function(doc, date_xpath = NULL, lang = NA_character_) {
  try_parse <- function(x, src) {
    d <- parse_date_string(x, lang)
    if (!is.na(d)) list(date = d, source = src) else NULL
  }
  if (!ns_blank(date_xpath)) {
    r <- try_parse(first_text(doc, date_xpath) %||% xml2::xml_attr(xml2::xml_find_first(doc, date_xpath), "datetime"), "xpath")
    if (!is.null(r)) return(r)
  }
  m <- meta_content(doc, c("article:published_time", "datepublished", "dc.date", "dc.date.issued",
                           "dcterms.created", "date", "pubdate", "publish-date", "og:updated_time"))
  r <- if (!is.null(m)) try_parse(m, "meta")
  if (!is.null(r)) return(r)
  for (js in xml2::xml_text(xml2::xml_find_all(doc, "//script[@type='application/ld+json']"))) {
    dp <- regmatches(js, regexpr('"datePublished"\\s*:\\s*"[^"]+"', js))
    if (length(dp)) { r <- try_parse(sub('.*:\\s*"([^"]+)"', "\\1", dp), "jsonld"); if (!is.null(r)) return(r) }
  }
  r <- try_parse(xml2::xml_attr(xml2::xml_find_first(doc, "//time[@datetime]"), "datetime"), "time")
  if (!is.null(r)) return(r)
  list(date = NA_character_, source = NA_character_)
}

date_from_text <- function(text, lang = NA_character_) {
  d <- parse_date_string(text, lang)
  list(date = d, source = if (is.na(d)) NA_character_ else "text")
}

# Month names (lower case, without trailing dots) -> month number. Covers the
# main NATO languages in the forms they take in dates (incl. genitive forms).
NS_MONTHS <- local({
  m <- list(
    `1` = c("january", "jan", "janvier", "janv", "januar", "j\u00e4nner", "enero", "ene", "gennaio", "gen", "janeiro", "januari", "stycze\u0144", "stycznia", "ledna", "leden", "ianuarie", "ian", "sije\u010dnja", "sije\u010danj", "januarja", "janu\u00e1ra", "janu\u00e1r", "ocak", "jaanuar", "janv\u0101ris", "janv\u0101r\u012b", "sausio", "jan\u00faar", "\u03b9\u03b1\u03bd\u03bf\u03c5\u03b1\u03c1\u03af\u03bf\u03c5", "\u044f\u043d\u0443\u0430\u0440\u0438", "\u0458\u0430\u043d\u0443\u0430\u0440\u0438", "janar", "tammikuuta"),
    `2` = c("february", "feb", "f\u00e9vrier", "f\u00e9vr", "februar", "febrero", "feb", "febbraio", "fevereiro", "fev", "februari", "luty", "lutego", "\u00fanora", "\u00fanor", "februarie", "velja\u010de", "velja\u010da", "februarja", "febru\u00e1ra", "febru\u00e1r", "\u015fubat", "subat", "veebruar", "febru\u0101ris", "febru\u0101r\u012b", "vasario", "febr\u00faar", "\u03c6\u03b5\u03b2\u03c1\u03bf\u03c5\u03b1\u03c1\u03af\u03bf\u03c5", "\u0444\u0435\u0432\u0440\u0443\u0430\u0440\u0438", "\u0444\u0435\u0432\u0440\u0443\u0430\u0440\u0438", "shkurt", "helmikuuta"),
    `3` = c("march", "mar", "mars", "m\u00e4rz", "marzo", "mar\u00e7o", "maart", "marca", "marzec", "b\u0159ezna", "b\u0159ezen", "martie", "o\u017eujka", "o\u017eujak", "marca", "marec", "marta", "m\u00e1rcius", "mart", "m\u00e4rts", "marts", "mart\u0101", "kovo", "mars", "\u03bc\u03b1\u03c1\u03c4\u03af\u03bf\u03c5", "\u043c\u0430\u0440\u0442", "\u043c\u0430\u0440\u0442", "mars", "maaliskuuta"),
    `4` = c("april", "apr", "avril", "abril", "aprile", "kwietnia", "kwiecie\u0144", "dubna", "duben", "aprilie", "travnja", "travanj", "aprila", "apr\u00edla", "\u00e1prilis", "nisan", "aprill", "apr\u012blis", "apr\u012bl\u012b", "baland\u017eio", "apr\u00edl", "\u03b1\u03c0\u03c1\u03b9\u03bb\u03af\u03bf\u03c5", "\u0430\u043f\u0440\u0438\u043b", "prill", "huhtikuuta"),
    `5` = c("may", "mai", "mayo", "maggio", "mag", "maio", "mei", "maja", "maj", "kv\u011btna", "kv\u011bten", "mai", "svibnja", "svibanj", "maja", "m\u00e1ja", "m\u00e1jus", "may\u0131s", "mayis", "mai", "maijs", "maij\u0101", "gegu\u017e\u0117s", "ma\u00ed", "\u03bc\u03b1\u0390\u03bf\u03c5", "\u03bc\u03b1\u03b9\u03bf\u03c5", "\u043c\u0430\u0439", "\u043c\u0430\u0458", "maj", "toukokuuta"),
    `6` = c("june", "jun", "juin", "juni", "junio", "giugno", "giu", "junho", "czerwca", "czerwiec", "\u010dervna", "\u010derven", "iunie", "lipnja", "lipanj", "junija", "j\u00fana", "j\u00fanius", "haziran", "juuni", "j\u016bnijs", "j\u016bnij\u0101", "bir\u017eelio", "j\u00fan\u00ed", "\u03b9\u03bf\u03c5\u03bd\u03af\u03bf\u03c5", "\u044e\u043d\u0438", "\u0458\u0443\u043d\u0438", "qershor", "kes\u00e4kuuta"),
    `7` = c("july", "jul", "juillet", "juil", "juli", "julio", "luglio", "lug", "julho", "lipca", "lipiec", "\u010dervence", "\u010dervenec", "iulie", "srpnja", "srpanj", "julija", "j\u00fala", "j\u00falius", "temmuz", "juuli", "j\u016blijs", "j\u016blij\u0101", "liepos", "j\u00fal\u00ed", "\u03b9\u03bf\u03c5\u03bb\u03af\u03bf\u03c5", "\u044e\u043b\u0438", "\u0458\u0443\u043b\u0438", "korrik", "hein\u00e4kuuta"),
    `8` = c("august", "augusti", "aug", "ao\u00fbt", "agosto", "ago", "augustus", "sierpnia", "sierpie\u0144", "srpna", "srpen", "august", "kolovoza", "kolovoz", "avgusta", "augusta", "augusztus", "a\u011fustos", "agustos", "august", "augusts", "august\u0101", "rugpj\u016b\u010dio", "\u00e1g\u00fast", "\u03b1\u03c5\u03b3\u03bf\u03cd\u03c3\u03c4\u03bf\u03c5", "\u0430\u0432\u0433\u0443\u0441\u0442", "\u0430\u0432\u0433\u0443\u0441\u0442", "gusht", "elokuuta"),
    `9` = c("september", "sep", "sept", "septembre", "septiembre", "settembre", "set", "setembro", "wrzesie\u0144", "wrze\u015bnia", "z\u00e1\u0159\u00ed", "septembrie", "rujna", "rujan", "septembra", "szeptember", "eyl\u00fcl", "eylul", "septembris", "septembr\u012b", "rugs\u0117jo", "september", "\u03c3\u03b5\u03c0\u03c4\u03b5\u03bc\u03b2\u03c1\u03af\u03bf\u03c5", "\u0441\u0435\u043f\u0442\u0435\u043c\u0432\u0440\u0438", "\u0441\u0435\u043f\u0442\u0435\u043c\u0432\u0440\u0438", "shtator", "syyskuuta"),
    `10` = c("october", "oct", "octobre", "oktober", "octubre", "ottobre", "ott", "outubro", "out", "oktober", "pa\u017adziernika", "pa\u017adziernik", "\u0159\u00edjna", "\u0159\u00edjen", "octombrie", "oktobra", "okt\u00f3bra", "okt\u00f3ber", "ekim", "oktoober", "oktobris", "oktobr\u012b", "spalio", "okt\u00f3ber", "\u03bf\u03ba\u03c4\u03c9\u03b2\u03c1\u03af\u03bf\u03c5", "\u043e\u043a\u0442\u043e\u043c\u0432\u0440\u0438", "\u043e\u043a\u0442\u043e\u043c\u0432\u0440\u0438", "tetor", "lokakuuta"),
    `11` = c("november", "nov", "novembre", "noviembre", "novembro", "listopada", "listopad", "listopadu", "noiembrie", "studenoga", "studeni", "novembra", "novembra", "november", "kas\u0131m", "kasim", "novembris", "novembr\u012b", "lapkri\u010dio", "n\u00f3vember", "\u03bd\u03bf\u03b5\u03bc\u03b2\u03c1\u03af\u03bf\u03c5", "\u043d\u043e\u0435\u043c\u0432\u0440\u0438", "\u043d\u043e\u0435\u043c\u0432\u0440\u0438", "n\u00ebntor", "marraskuuta"),
    `12` = c("december", "dec", "d\u00e9cembre", "d\u00e9c", "dezember", "diciembre", "dic", "dicembre", "dezembro", "dez", "grudnia", "grudzie\u0144", "prosince", "prosinec", "decembrie", "prosinca", "prosinac", "decembra", "decembra", "december", "aral\u0131k", "aralik", "detsember", "decembris", "decembr\u012b", "gruod\u017eio", "desember", "\u03b4\u03b5\u03ba\u03b5\u03bc\u03b2\u03c1\u03af\u03bf\u03c5", "\u0434\u0435\u043a\u0435\u043c\u0432\u0440\u0438", "\u0434\u0435\u043a\u0435\u043c\u0432\u0440\u0438", "dhjetor", "joulukuuta"))
  # "listopad(a)" is November in Polish/Czech but October in Croatian; see parse_date_string().
  v <- unlist(lapply(names(m), function(k) stats::setNames(rep(as.integer(k), length(m[[k]])), m[[k]])))
  v[!duplicated(names(v))]
})

#' Parse a date written in any NATO language
#'
#' Finds the first recognisable date in a string: ISO 8601 timestamps,
#' numeric day-first dates (`03.10.2025`, `03/10/2025`), Hungarian
#' year-first dates, and dates with month names in the languages of all
#' NATO members (e.g. "3 October 2025", "October 3, 2025", "3. Oktober 2025",
#' "3 de octubre de 2025", "3 Ekim 2025", Greek and Cyrillic months).
#'
#' "listopad" means November in Polish and Czech but October in Croatian;
#' pass `lang = "hr"` for Croatian text.
#'
#' @param x a string.
#' @param lang optional language code of the text.
#' @return `"YYYY-MM-DD"`, or `NA` if no valid date is found.
#' @export
#' @examples
#' parse_date_string("Speech delivered on 3 octobre 2025 in Paris")
#' parse_date_string("October 3, 2025")
#' parse_date_string("30. listopada 2025.", lang = "hr")
parse_date_string <- function(x, lang = NA_character_) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_character_)
  x <- tolower(ns_squish(x))
  ok <- function(y, m, d) {
    y <- as.integer(y); m <- as.integer(m); d <- as.integer(d)
    if (any(is.na(c(y, m, d))) || y < 1990 || y > 2100) return(NA_character_)
    out <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, m, d)), error = function(e) NA)
    if (is.na(out)) NA_character_ else format(out)
  }
  # ISO 8601 (also the start of timestamps).
  m <- regmatches(x, regexec("(\\d{4})-(\\d{1,2})-(\\d{1,2})", x))[[1]]
  if (length(m)) return(ok(m[2], m[3], m[4]))
  # Hungarian style: "2025. oktober 3." / "2025.10.03."
  m <- regmatches(x, regexec("(\\d{4})\\.\\s*(\\d{1,2})\\.\\s*(\\d{1,2})\\.?", x))[[1]]
  if (length(m)) return(ok(m[2], m[3], m[4]))
  mon_rx <- paste0("(", paste(names(NS_MONTHS)[order(-nchar(names(NS_MONTHS)))], collapse = "|"), ")(?!\\p{L})")
  mon <- function(tok) if (startsWith(tok, "listopad") && isTRUE(startsWith(tolower(lang), "hr"))) 10L else NS_MONTHS[[tok]]
  # 3 October 2025 / 3. Oktober 2025 / 3 de octubre de 2025 (also Cyrillic, Greek)
  m <- regmatches(x, regexec(paste0("(\\d{1,2})\\.?\\s*(?:de\\s+|th\\s+|st\\s+|nd\\s+|rd\\s+)?", mon_rx, "\\.?,?\\s*(?:de\\s+)?(\\d{4})"), x, perl = TRUE))[[1]]
  if (length(m)) return(ok(m[4], mon(m[3]), m[2]))
  # October 3, 2025
  m <- regmatches(x, regexec(paste0(mon_rx, "\\.?\\s+(\\d{1,2})(?:st|nd|rd|th)?,?\\s+(\\d{4})"), x, perl = TRUE))[[1]]
  if (length(m)) return(ok(m[4], mon(m[2]), m[3]))
  # 2025. oktober 3.
  m <- regmatches(x, regexec(paste0("(\\d{4})\\.?\\s+", mon_rx, "\\s+(\\d{1,2})"), x, perl = TRUE))[[1]]
  if (length(m)) return(ok(m[2], mon(m[3]), m[4]))
  # 03.10.2025 / 03/10/2025 (day first, as used by European sites).
  m <- regmatches(x, regexec("(\\d{1,2})[./](\\d{1,2})[./](\\d{4})", x))[[1]]
  if (length(m)) return(ok(m[4], m[3], m[2]))
  NA_character_
}
