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

#' Extract title, date, language, and body text from a speech page.
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
    `1` = c("january", "jan", "janvier", "janv", "januar", "jänner", "enero", "ene", "gennaio", "gen", "janeiro", "januari", "styczeń", "stycznia", "ledna", "leden", "ianuarie", "ian", "siječnja", "siječanj", "januarja", "januára", "január", "ocak", "jaanuar", "janvāris", "janvārī", "sausio", "janúar", "ιανουαρίου", "януари", "јануари", "janar", "tammikuuta"),
    `2` = c("february", "feb", "février", "févr", "februar", "febrero", "feb", "febbraio", "fevereiro", "fev", "februari", "luty", "lutego", "února", "únor", "februarie", "veljače", "veljača", "februarja", "februára", "február", "şubat", "subat", "veebruar", "februāris", "februārī", "vasario", "febrúar", "φεβρουαρίου", "февруари", "февруари", "shkurt", "helmikuuta"),
    `3` = c("march", "mar", "mars", "märz", "marzo", "março", "maart", "marca", "marzec", "března", "březen", "martie", "ožujka", "ožujak", "marca", "marec", "marta", "március", "mart", "märts", "marts", "martā", "kovo", "mars", "μαρτίου", "март", "март", "mars", "maaliskuuta"),
    `4` = c("april", "apr", "avril", "abril", "aprile", "kwietnia", "kwiecień", "dubna", "duben", "aprilie", "travnja", "travanj", "aprila", "apríla", "április", "nisan", "aprill", "aprīlis", "aprīlī", "balandžio", "apríl", "απριλίου", "април", "prill", "huhtikuuta"),
    `5` = c("may", "mai", "mayo", "maggio", "mag", "maio", "mei", "maja", "maj", "května", "květen", "mai", "svibnja", "svibanj", "maja", "mája", "május", "mayıs", "mayis", "mai", "maijs", "maijā", "gegužės", "maí", "μαΐου", "μαιου", "май", "мај", "maj", "toukokuuta"),
    `6` = c("june", "jun", "juin", "juni", "junio", "giugno", "giu", "junho", "czerwca", "czerwiec", "června", "červen", "iunie", "lipnja", "lipanj", "junija", "júna", "június", "haziran", "juuni", "jūnijs", "jūnijā", "birželio", "júní", "ιουνίου", "юни", "јуни", "qershor", "kesäkuuta"),
    `7` = c("july", "jul", "juillet", "juil", "juli", "julio", "luglio", "lug", "julho", "lipca", "lipiec", "července", "červenec", "iulie", "srpnja", "srpanj", "julija", "júla", "július", "temmuz", "juuli", "jūlijs", "jūlijā", "liepos", "júlí", "ιουλίου", "юли", "јули", "korrik", "heinäkuuta"),
    `8` = c("august", "augusti", "aug", "août", "agosto", "ago", "augustus", "sierpnia", "sierpień", "srpna", "srpen", "august", "kolovoza", "kolovoz", "avgusta", "augusta", "augusztus", "ağustos", "agustos", "august", "augusts", "augustā", "rugpjūčio", "ágúst", "αυγούστου", "август", "август", "gusht", "elokuuta"),
    `9` = c("september", "sep", "sept", "septembre", "septiembre", "settembre", "set", "setembro", "wrzesień", "września", "září", "septembrie", "rujna", "rujan", "septembra", "szeptember", "eylül", "eylul", "septembris", "septembrī", "rugsėjo", "september", "σεπτεμβρίου", "септември", "септември", "shtator", "syyskuuta"),
    `10` = c("october", "oct", "octobre", "oktober", "octubre", "ottobre", "ott", "outubro", "out", "oktober", "października", "październik", "října", "říjen", "octombrie", "oktobra", "októbra", "október", "ekim", "oktoober", "oktobris", "oktobrī", "spalio", "október", "οκτωβρίου", "октомври", "октомври", "tetor", "lokakuuta"),
    `11` = c("november", "nov", "novembre", "noviembre", "novembro", "listopada", "listopad", "listopadu", "noiembrie", "studenoga", "studeni", "novembra", "novembra", "november", "kasım", "kasim", "novembris", "novembrī", "lapkričio", "nóvember", "νοεμβρίου", "ноември", "ноември", "nëntor", "marraskuuta"),
    `12` = c("december", "dec", "décembre", "déc", "dezember", "diciembre", "dic", "dicembre", "dezembro", "dez", "grudnia", "grudzień", "prosince", "prosinec", "decembrie", "prosinca", "prosinac", "decembra", "decembra", "december", "aralık", "aralik", "detsember", "decembris", "decembrī", "gruodžio", "desember", "δεκεμβρίου", "декември", "декември", "dhjetor", "joulukuuta"))
  # "listopad(a)" is November in Polish/Czech but October in Croatian; see parse_date_string().
  v <- unlist(lapply(names(m), function(k) setNames(rep(as.integer(k), length(m[[k]])), m[[k]])))
  v[!duplicated(names(v))]
})

#' Parse the first recognisable date in a string. Returns "YYYY-MM-DD" or NA.
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
  # Hungarian style 2025. október 3. / 2025.10.03.
  m <- regmatches(x, regexec("(\\d{4})\\.\\s*(\\d{1,2})\\.\\s*(\\d{1,2})\\.?", x))[[1]]
  if (length(m)) return(ok(m[2], m[3], m[4]))
  mon_rx <- paste0("(", paste(names(NS_MONTHS)[order(-nchar(names(NS_MONTHS)))], collapse = "|"), ")(?!\\p{L})")
  mon <- function(tok) if (startsWith(tok, "listopad") && isTRUE(startsWith(tolower(lang), "hr"))) 10L else NS_MONTHS[[tok]]
  # 3 October 2025 / 3. Oktober 2025 / 3 de octubre de 2025 / 3 октомври 2025
  m <- regmatches(x, regexec(paste0("(\\d{1,2})\\.?\\s*(?:de\\s+|th\\s+|st\\s+|nd\\s+|rd\\s+)?", mon_rx, "\\.?,?\\s*(?:de\\s+)?(\\d{4})"), x, perl = TRUE))[[1]]
  if (length(m)) return(ok(m[4], mon(m[3]), m[2]))
  # October 3, 2025
  m <- regmatches(x, regexec(paste0(mon_rx, "\\.?\\s+(\\d{1,2})(?:st|nd|rd|th)?,?\\s+(\\d{4})"), x, perl = TRUE))[[1]]
  if (length(m)) return(ok(m[4], mon(m[2]), m[3]))
  # 2025. október 3.
  m <- regmatches(x, regexec(paste0("(\\d{4})\\.?\\s+", mon_rx, "\\s+(\\d{1,2})"), x, perl = TRUE))[[1]]
  if (length(m)) return(ok(m[2], mon(m[3]), m[4]))
  # 03.10.2025 / 03/10/2025 (day first, as used by European sites).
  m <- regmatches(x, regexec("(\\d{1,2})[./](\\d{1,2})[./](\\d{4})", x))[[1]]
  if (length(m)) return(ok(m[4], m[3], m[2]))
  NA_character_
}
