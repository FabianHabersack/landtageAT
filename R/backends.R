.state_catalog <- tibble::tribble(
  ~state, ~state_name, ~entry_url, ~secondary_url,
  "bgld", "Burgenland", "https://www.bgld-landtag.at/landtagssitzungen/protokolle/xxiii-gp-protokolle", "https://www.bgld-landtag.at/der-landtag/archiv/xxi-gp-protokolle",
  "ktn", "K\u00e4rnten", "https://www.ktn.gv.at/Politik/Landtag/Stenographische-Protokolle", NA_character_,
  "noe", "Nieder\u00f6sterreich", "https://noe-landtag.gv.at/sitzungen", NA_character_,
  "ooe", "Ober\u00f6sterreich", "https://www.land-oberoesterreich.gv.at/12182.htm", "https://e-gov.ooe.gv.at/at.gv.ooe.ogd2-citi/#/overview",
  "sbg", "Salzburg", "https://www.salzburg.gv.at/pol/landtag/parlamentarische-materialien", NA_character_,
  "stm", "Steiermark", "https://www.landtag.steiermark.at/cms/ziel/181952035", "https://www.landtag.steiermark.at/cms/ziel/122780475",
  "tir", "Tirol", "https://www.tirol.gv.at/landtag", "https://lte.tirol.gv.at/public/index.xhtml",
  "vbg", "Vorarlberg", "https://vorarlberg.at/web/landtag/-/protokolle", NA_character_,
  "wie", "Wien", "https://www.wien.gv.at/mdb/ltg", NA_character_
)

.backend_feature_matrix <- tibble::tribble(
  ~state, ~sessions, ~protocols, ~protocol_text, ~members, ~speech_level, ~notes,
  "bgld", TRUE, TRUE, TRUE, FALSE, FALSE, "HTML + PDF discovery with protocol-focused filtering.",
  "ktn", TRUE, TRUE, TRUE, FALSE, FALSE, "Endpoint can intermittently return 503; backend degrades gracefully.",
  "noe", TRUE, TRUE, TRUE, FALSE, FALSE, "Session page discovery with one-level follow links.",
  "ooe", TRUE, TRUE, TRUE, FALSE, FALSE, "Landtag page discovery + OGD catalog pointer.",
  "sbg", TRUE, TRUE, TRUE, FALSE, FALSE, "Parliamentary materials page with one-level follow links.",
  "stm", TRUE, TRUE, TRUE, FALSE, FALSE, "Material pages with direct stenographic PDF links.",
  "tir", TRUE, TRUE, TRUE, FALSE, FALSE, "General portal + LTE system discovery.",
  "vbg", TRUE, TRUE, TRUE, FALSE, FALSE, "Protocol page with focused filtering.",
  "wie", TRUE, TRUE, TRUE, FALSE, FALSE, "Recursive document-tree crawling (depth-limited, default 1)."
)

.state_backend_config <- tibble::tribble(
  ~state, ~include_pattern, ~exclude_pattern, ~crawl_depth_default, ~follow_level1,
  "bgld", "protok|steno|sitz|landtagssitzung|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, FALSE,
  "ktn", "protok|steno|sitz|landtag|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, FALSE,
  "noe", "sitz|protok|steno|plenar|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "ooe", "protok|steno|landtag|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "sbg", "landtag|material|protok|steno|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "stm", "protok|steno|sitz|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, FALSE,
  "tir", "landtag|lte|protok|steno|sitz|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "vbg", "protok|steno|sitz|landtag|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "wie", "ltg|protok|steno|sitz|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 1L, FALSE
)

normalize_state <- function(state) {
  state <- tolower(state)
  aliases <- c(
    bgld = "bgld",
    ktn = "ktn",
    noe = "noe",
    ooe = "ooe",
    sbg = "sbg",
    stm = "stm",
    tir = "tir",
    vbg = "vbg",
    wie = "wie",
    burgenland = "bgld",
    "k\u00e4rnten" = "ktn",
    kaernten = "ktn",
    "nieder\u00f6sterreich" = "noe",
    niederoesterreich = "noe",
    "ober\u00f6sterreich" = "ooe",
    oberoesterreich = "ooe",
    salzburg = "sbg",
    steiermark = "stm",
    tirol = "tir",
    vorarlberg = "vbg",
    wien = "wie",
    vienna = "wie"
  )
  out <- unname(aliases[state])
  if (is.na(out)) {
    cli::cli_abort("Unsupported state `{state}`. Run `list_states()` for supported values.")
  }
  out
}

state_name <- function(code) {
  .state_catalog$state_name[match(code, .state_catalog$state)]
}

state_entry_urls <- function(code) {
  row <- .state_catalog[.state_catalog$state == code, ]
  unique(stats::na.omit(c(row$entry_url, row$secondary_url)))
}

state_backend <- function(code) {
  cfg <- .state_backend_config[.state_backend_config$state == code, ]
  if (nrow(cfg) != 1) cli::cli_abort("Missing backend configuration for `{code}`.")
  cfg
}
