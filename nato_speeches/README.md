# natospeeches: speeches by NATO leaders

Collects speeches by four office-holders in each of the 32 NATO member states:

| role   | office                                            |
|--------|---------------------------------------------------|
| `hos`  | Head of state (president, monarch, or Canada's Governor General) |
| `mod`  | Minister of Defence                               |
| `mfa`  | Minister of Foreign Affairs                       |
| `chod` | Chief of Defence / top military commander         |

Iceland has no defence minister and no armed forces, so `ISL_mod` and `ISL_chod` are
listed with `type = none` and skipped.

## Installation

```r
# install.packages("remotes")
remotes::install_github("tamirlibel/Rcrawler", subdir = "nato_speeches",
                        ref = "claude/nato-speech-scraper")
install.packages("pdftools")    # optional: extract speeches published as PDFs
install.packages("chromote")    # optional: sites built by JavaScript
```

Or from a local clone: `R CMD INSTALL nato_speeches`.

## Quick start

```r
library(natospeeches)

cfg <- ns_config(data_dir = "natospeech_data", contact = "you@university.edu")

# 1. Check which registry entries currently work (run this first)
report <- check_sources(cfg = cfg)
table(report$verdict)
report[report$verdict != "ok", ]

# 2. Collect (re-run any time; only new speeches are fetched)
collect_speeches(countries = c("FRA", "DEU"), roles = c("hos", "mfa"),
                 since = "2025-01-01", cfg = cfg)

# 3. Use the corpus
speeches <- read_speeches(cfg)
export_speeches("speeches.csv", cfg)    # or .rds
```

Command line (the script ships with the package):

```sh
S=$(Rscript -e 'cat(system.file("scripts", "natospeeches.R", package = "natospeeches"))')
Rscript $S --check
Rscript $S --countries=GBR,USA --roles=mod,chod --since=2025-01-01
Rscript $S --export=speeches.csv
Rscript $S --help
```

## Fixing or extending the registry

```r
file.copy(default_registry(), "sources.csv")
# edit sources.csv (e.g. in Excel or RStudio), then:
my_sources <- load_sources("sources.csv")          # validates the file
check_sources(ids = "HUN_mod", sources = my_sources, cfg = cfg)
collect_speeches(sources = my_sources, cfg = cfg)
```

The columns are documented in `?load_sources`.

## How it works

1. **Registry** (`config/sources.csv`): one row per country × role, holding the listing
   page URL, pagination template, optional `link_regex` (which links are articles),
   optional `speaker_regex` (separates office-holders who share a listing, e.g. minister
   and chief of defence on a defence-ministry site), and `filter`: `all` if the listing
   holds only speeches, `keywords` if it's a general news page.
2. **Discovery**: fetches listing pages (HTML, RSS, or the GOV.UK search API) and keeps
   article-like links from the same site.
3. **Classification**: on `keywords` sources, a title/URL must contain a speech word in
   one of 28 languages ("speech", "Rede", "discours", "przemówienie", "ομιλία", …).
4. **Extraction** (`?extract_article`): title (og:title → h1 → `<title>`), date (meta tags → JSON-LD →
   `<time>` → dates in text, with month names in all NATO languages), body text (main
   content container after stripping navigation, cookie banners, share widgets, etc.).
   Per-source XPath overrides go in `title_xpath`, `date_xpath`, `body_xpath`.
   PDF speeches are extracted with pdftools when it is installed.
5. **Storage** in `data_dir`:
   `speeches.jsonl` (the corpus), `visited.csv` (processed URLs, so re-runs fetch only
   new pages), `raw/` (gzipped HTML for re-parsing), `runs/` (per-run summaries).

It is polite by default: obeys robots.txt, waits 3 s between requests to the same host,
and backs off on 429/5xx errors.

## Record fields

`id, source_id, iso3, country, role, office, url, title, date, date_source, language,
format, text, n_words, text_hash, pdf_links, retrieved_at`. See `?read_speeches`.

`date_source` records where the date came from (`meta`, `jsonld`, `time`, `xpath`,
`listing`, `text`, `pdf_created`); `text` and `pdf_created` dates are the least reliable.

## Known limitations

- **Registry URLs have not been tested against the live sites** (`status = unverified`).
  They were compiled without internet access to the sites, so some will point to
  moved pages. Run `check_sources()`, then fix rows whose verdict is not `ok`. After
  fixing a row, change `status` to `verified`.
- Sources flagged `js_required` (currently the three canada.ca entries) are skipped
  unless you run them with a browser:
  `collect_speeches(ids = "CAN_mod", include_js = TRUE, cfg = ns_config(fetcher = chromote_fetcher()))`.
  `chromote_fetcher()` has not been tested against live sites.
- PDFs linked *from* a speech page (e.g. "download full text") are recorded in
  `pdf_links` but not downloaded; PDFs linked directly from a listing are.
- Office-holders change. Check sources whose URL contains the incumbent's name
  (e.g. `DEU_hos`, `SVN_hos`) after elections.
- Keyword filtering on general news pages catches most speeches but can include
  statements or messages and miss untitled speeches. Review with `n_words`/`title`.

## Tests

```sh
R CMD build nato_speeches && R CMD check natospeeches_0.1.0.tar.gz
```

Tests run offline, using HTML/PDF fixtures and a mock website.
