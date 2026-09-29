# Words that mark a page as a speech, per language. Matched against the title
# and URL slug (slugs are usually transliterated, hence the ASCII variants).
NS_SPEECH_WORDS <- c(
  en = "speech|remarks|address|keynote|lecture|toast|eulogy|state of the union",
  fr = "discours|allocution|intervention|vœux|voeux|hommage",
  de = "rede|ansprache|grußwort|grusswort|festrede|weihnachtsansprache",
  nl = "toespraak|speech|troonrede",
  it = "discorso|intervento|messaggio|saluto",
  es = "discurso|intervención|intervencion|mensaje",
  pt = "discurso|intervenção|intervencao|mensagem",
  da = "tale|nytårstale|nytarstale",
  no = "tale|nyttårstale|nyttarstale",
  sv = "tal|jultal|anförande|anforande",
  is = "ræða|raeda|ávarp|avarp",
  fi = "puhe|juhlapuhe",
  pl = "przemówienie|przemowienie|wystąpienie|wystapienie|orędzie|oredzie",
  cs = "projev|proslov",
  sk = "prejav|príhovor|prihovor",
  sl = "govor|nagovor|pozdravni",
  hr = "govor|obraćanje|obracanje|pozdravni",
  sr = "govor|obraćanje|obracanje",
  hu = "beszéd|beszed|felszólalás|felszolalas|köszöntő|koszonto",
  ro = "discurs|discursul|alocuțiune|alocutiune|mesaj",
  bg = "реч|слово|обръщение|изказване|rech|obrashtenie",
  mk = "говор|обраќање|govor|obrakjanje",
  el = "ομιλία|ομιλια|χαιρετισμός|διάγγελμα|omilia|xairetismos",
  tr = "konuşma|konusma|hitap|mesaj",
  et = "kõne|kone|tervitus",
  lv = "runā|runa|uzruna",
  lt = "kalba|sveikinimas",
  sq = "fjalim|fjalimi|përshëndetje"
)

NS_SPEECH_RX <- enc2utf8(paste0("(?i)(?<![\\p{L}])(", paste(unique(unlist(strsplit(NS_SPEECH_WORDS, "|", fixed = TRUE))), collapse = "|"), ")(?![\\p{L}])"))

#' Does a page look like a speech? Sources whose listing contains only speeches
#' (`filter = all`) accept everything; otherwise a speech word must appear in the
#' title, the link text, or the URL.
is_speech <- function(source, title = NA, url = NA, link_text = NA) {
  if (identical(source$filter, "all")) return(TRUE)
  slug <- gsub("[-_/+.]", " ", utils::URLdecode(url_path(url %||% "")))
  hay <- enc2utf8(paste(title %||% "", link_text %||% "", slug))
  grepl(NS_SPEECH_RX, hay, perl = TRUE)
}

#' For listings shared by several office-holders (e.g. a defence ministry site
#' carrying both the minister's and the chief of defence's speeches), require
#' `speaker_regex` to match the title or the opening of the text.
speaker_matches <- function(source, title = NA, text = NA) {
  if (ns_blank(source$speaker_regex)) return(TRUE)
  hay <- enc2utf8(paste(title %||% "", substr(text %||% "", 1, 1500)))
  grepl(source$speaker_regex, hay, perl = TRUE, ignore.case = TRUE)
}
