# NATO speech collector

Collects speeches by four office-holders in each of the 32 NATO member states:

| role   | office                                            |
|--------|---------------------------------------------------|
| `hos`  | Head of state (president, monarch, or Canada's Governor General) |
| `mod`  | Minister of Defence                               |
| `mfa`  | Minister of Foreign Affairs                       |
| `chod` | Chief of Defence / top military commander         |

Iceland has no defence minister and no armed forces, so `ISL_mod` and `ISL_chod` are
listed with `type = none` and skipped.

It is written in plain R (`httr`, `xml2`, `jsonlite`, `digest`), and lives in the
Rcrawler repository but does not require the package to be installed.

## Quick start

```r
install.packages(c("httr", "xml2", "jsonlite", "digest"))
source("nato_speeches/load.R")

Sys.setenv(NATOSPEECH_CONTACT = "you@university.edu")   # goes into the User-Agent

# 1. Check which registry entries currently work (run this first)
report <- check_sources()
table(report$verdict)

# 2. Collect
collect_speeches(countries = c("FRA", "DEU"), roles = c("hos", "mfa"), since = "2025-01-01")

# 3. Use the corpus
speeches <- read_speeches()
export_speeches("speeches.csv")        # or .rds
```

Or from the shell, from the repository root:

```sh
Rscript nato_speeches/run.R --check
Rscript nato_speeches/run.R --countries=GBR,USA --roles=mod,chod --since=2025-01-01
Rscript nato_speeches/run.R --export=speeches.csv
```

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
4. **Extraction**: title (og:title → h1 → `<title>`), date (meta tags → JSON-LD →
   `<time>` → dates in text, with month names in all NATO languages), body text (main
   content container after stripping navigation, cookie banners, share widgets, etc.).
   Per-source XPath overrides go in `title_xpath`, `date_xpath`, `body_xpath`.
5. **Storage** in `nato_speeches/data/` (git-ignored):
   `speeches.jsonl` (the corpus), `visited.csv` (processed URLs, so re-runs fetch only
   new pages), `raw/` (gzipped HTML for re-parsing), `runs/` (per-run summaries).

It is polite by default: obeys robots.txt, waits 3 s between requests to the same host,
and backs off on 429/5xx errors.

## Record fields

`id, source_id, iso3, country, role, office, url, title, date, date_source, language,
text, n_words, text_hash, pdf_links, retrieved_at`

`date_source` records where the date came from (`meta`, `jsonld`, `time`, `text`,
`listing`, `xpath`); `text`-derived dates are the least reliable.

## Known limitations

- **Registry URLs have not been tested against the live sites** (`status = unverified`).
  They were compiled without internet access to the sites, so some will point to
  moved pages. Run `check_sources()`, then fix rows whose verdict is not `ok`. After
  fixing a row, change `status` to `verified`.
- Sources flagged `js_required` (currently canada.ca search results) are skipped
  unless you pass `include_js = TRUE` together with a JavaScript-capable `fetcher`
  in `ns_config()`. A fetcher is `function(url, cfg)` returning
  `list(url, status, content_type, body)`, e.g. built on `chromote`.
- PDF-only speeches are not extracted; PDF links found in a speech page are kept in
  `pdf_links`.
- Office-holders change. Check sources whose URL contains the incumbent's name
  (e.g. `DEU_hos`, `SVN_hos`) after elections.
- Keyword filtering on general news pages catches most speeches but can include
  statements or messages and miss untitled speeches. Review with `n_words`/`title`.

## Tests

```sh
Rscript nato_speeches/tests/testthat.R
```

Tests are offline; they use HTML fixtures and a mock website.
