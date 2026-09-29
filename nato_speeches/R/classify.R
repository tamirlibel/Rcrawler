# Words that mark a page as a speech, per language. Matched against the title
# and URL slug (slugs are usually transliterated, hence the ASCII variants).
NS_SPEECH_WORDS <- c(
  en = "speech|remarks|address|keynote|lecture|toast|eulogy|state of the union",
  fr = "discours|allocution|intervention|v\u0153ux|voeux|hommage",
  de = "rede|ansprache|gru\u00dfwort|grusswort|festrede|weihnachtsansprache",
  nl = "toespraak|speech|troonrede",
  it = "discorso|intervento|messaggio|saluto",
  es = "discurso|intervenci\u00f3n|intervencion|mensaje",
  pt = "discurso|interven\u00e7\u00e3o|intervencao|mensagem",
  da = "tale|nyt\u00e5rstale|nytarstale",
  no = "tale|nytt\u00e5rstale|nyttarstale",
  sv = "tal|jultal|anf\u00f6rande|anforande",
  is = "r\u00e6\u00f0a|raeda|\u00e1varp|avarp",
  fi = "puhe|juhlapuhe",
  pl = "przem\u00f3wienie|przemowienie|wyst\u0105pienie|wystapienie|or\u0119dzie|oredzie",
  cs = "projev|proslov",
  sk = "prejav|pr\u00edhovor|prihovor",
  sl = "govor|nagovor|pozdravni",
  hr = "govor|obra\u0107anje|obracanje|pozdravni",
  sr = "govor|obra\u0107anje|obracanje",
  hu = "besz\u00e9d|beszed|felsz\u00f3lal\u00e1s|felszolalas|k\u00f6sz\u00f6nt\u0151|koszonto",
  ro = "discurs|discursul|alocu\u021biune|alocutiune|mesaj",
  bg = "\u0440\u0435\u0447|\u0441\u043b\u043e\u0432\u043e|\u043e\u0431\u0440\u044a\u0449\u0435\u043d\u0438\u0435|\u0438\u0437\u043a\u0430\u0437\u0432\u0430\u043d\u0435|rech|obrashtenie",
  mk = "\u0433\u043e\u0432\u043e\u0440|\u043e\u0431\u0440\u0430\u045c\u0430\u045a\u0435|govor|obrakjanje",
  el = "\u03bf\u03bc\u03b9\u03bb\u03af\u03b1|\u03bf\u03bc\u03b9\u03bb\u03b9\u03b1|\u03c7\u03b1\u03b9\u03c1\u03b5\u03c4\u03b9\u03c3\u03bc\u03cc\u03c2|\u03b4\u03b9\u03ac\u03b3\u03b3\u03b5\u03bb\u03bc\u03b1|omilia|xairetismos",
  tr = "konu\u015fma|konusma|hitap|mesaj",
  et = "k\u00f5ne|kone|tervitus",
  lv = "run\u0101|runa|uzruna",
  lt = "kalba|sveikinimas",
  sq = "fjalim|fjalimi|p\u00ebrsh\u00ebndetje"
)

NS_SPEECH_RX <- enc2utf8(paste0("(?i)(?<![\\p{L}])(", paste(unique(unlist(strsplit(NS_SPEECH_WORDS, "|", fixed = TRUE))), collapse = "|"), ")(?![\\p{L}])"))

# Does a page look like a speech? Sources whose listing contains only speeches
# (`filter = all`) accept everything; otherwise a speech word must appear in the
# title, the link text, or the URL.
is_speech <- function(source, title = NA, url = NA, link_text = NA) {
  if (identical(source$filter, "all")) return(TRUE)
  slug <- gsub("[-_/+.]", " ", utils::URLdecode(url_path(url %||% "")))
  hay <- enc2utf8(paste(title %||% "", link_text %||% "", slug))
  grepl(NS_SPEECH_RX, hay, perl = TRUE)
}

# For listings shared by several office-holders (e.g. a defence ministry site
# carrying both the minister's and the chief of defence's speeches), require
# `speaker_regex` to match the title or the opening of the text.
speaker_matches <- function(source, title = NA, text = NA) {
  if (ns_blank(source$speaker_regex)) return(TRUE)
  hay <- enc2utf8(paste(title %||% "", substr(text %||% "", 1, 1500)))
  grepl(source$speaker_regex, hay, perl = TRUE, ignore.case = TRUE)
}
