.state_catalog <- tibble::tribble(
  ~state, ~state_name, ~entry_url, ~secondary_url,
  "bgld", "Burgenland", "https://www.bgld-landtag.at/landtagssitzungen/protokolle/xxiii-gp-protokolle", "https://www.bgld-landtag.at/der-landtag/archiv/xxi-gp-protokolle",
  "ktn", "Kaernten", "https://www.ktn.gv.at/Politik/Landtag/Stenographische-Protokolle", NA_character_,
  "noe", "Niederoesterreich", "https://noe-landtag.gv.at/sitzungen", NA_character_,
  "ooe", "Oberoesterreich", "https://www.land-oberoesterreich.gv.at/12182.htm", "https://e-gov.ooe.gv.at/at.gv.ooe.ogd2-citi/#/overview",
  "sbg", "Salzburg", "https://www.salzburg.gv.at/pol/landtag/parlamentarische-materialien", NA_character_,
  "stm", "Steiermark", "https://www.landtag.steiermark.at/cms/ziel/181952035", "https://www.landtag.steiermark.at/cms/ziel/122780475",
  "tir", "Tirol", "https://www.tirol.gv.at/landtag", "https://lte.tirol.gv.at/public/index.xhtml",
  "vbg", "Vorarlberg", "https://vorarlberg.at/web/landtag/-/protokolle", NA_character_,
  "wie", "Wien", "https://www.wien.gv.at/mdb/ltg", NA_character_
)

.state_extra_entry_urls <- list(
  bgld = c(
    "https://www.bgld-landtag.at/der-landtag/archiv/xx-gp-protokolle",
    "https://www.bgld-landtag.at/der-landtag/archiv/xix-gp-protokolle"
  ),
  stm = c(
    "https://www.landtag.steiermark.at/cms/ziel/155295847",
    "https://www.landtag.steiermark.at/cms/ziel/138181194"
  ),
  wie = sprintf("https://www.wien.gv.at/mdb/ltg/%s/index.htm", 1998:as.integer(format(Sys.Date(), "%Y")))
)

.backend_feature_matrix <- tibble::tribble(
  ~state, ~sessions, ~protocols, ~protocol_text, ~members, ~speech_level, ~notes,
  "bgld", TRUE, TRUE, TRUE, FALSE, FALSE, "HTML + PDF discovery across current and archive GP pages.",
  "ktn", TRUE, TRUE, TRUE, FALSE, FALSE, "Endpoint can intermittently return 503; backend degrades gracefully.",
  "noe", TRUE, TRUE, TRUE, FALSE, FALSE, "Session page discovery with legislative-period subpages.",
  "ooe", TRUE, TRUE, TRUE, FALSE, FALSE, "Landtag page discovery + OGD catalog pointer.",
  "sbg", TRUE, TRUE, TRUE, FALSE, FALSE, "Parliamentary materials page and service search endpoint support.",
  "stm", TRUE, TRUE, TRUE, FALSE, FALSE, "Coverage includes multiple Gesetzgebungsperioden pages.",
  "tir", TRUE, TRUE, TRUE, FALSE, FALSE, "General portal + LTE system discovery.",
  "vbg", TRUE, TRUE, TRUE, FALSE, FALSE, "Protocol page plus period overview pages.",
  "wie", TRUE, TRUE, TRUE, FALSE, FALSE, "Year-indexed MDB source crawled from 1998 to present year."
)

.state_backend_config <- tibble::tribble(
  ~state, ~include_pattern, ~exclude_pattern, ~crawl_depth_default, ~follow_level1,
  "bgld", "protok|steno|sitz|landtagssitzung|gp|archiv|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "ktn", "protok|steno|sitz|landtag|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, FALSE,
  "noe", "sitz|protok|steno|plenar|gesetzgeb|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "ooe", "protok|steno|landtag|plenar|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "sbg", "landtag|material|protok|steno|periode|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "stm", "protok|steno|sitz|gesetzgebungsperiode|periode|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "tir", "landtag|lte|protok|steno|sitz|gesetzgebungsperiode|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "vbg", "protok|steno|sitz|landtagsperiode|periode|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 0L, TRUE,
  "wie", "ltg|protok|steno|sitz|\\.pdf($|\\?)", "impressum|datenschutz|kontakt|javascript:", 1L, FALSE
)



.state_source_period_map <- list(
  stm = c(
    "https://www.landtag.steiermark.at/cms/ziel/181952035" = "19",
    "https://www.landtag.steiermark.at/cms/ziel/155295847" = "18",
    "https://www.landtag.steiermark.at/cms/ziel/138181194" = "17"
  ),
  bgld = c(
    "https://www.bgld-landtag.at/landtagssitzungen/protokolle/xxiii-gp-protokolle" = "XXIII",
    "https://www.bgld-landtag.at/der-landtag/archiv/xxi-gp-protokolle" = "XXI",
    "https://www.bgld-landtag.at/der-landtag/archiv/xx-gp-protokolle" = "XX",
    "https://www.bgld-landtag.at/der-landtag/archiv/xix-gp-protokolle" = "XIX"
  )
)

normalize_state <- function(state) {
  state <- tolower(state)
  aliases <- c(
    bgld = "bgld", ktn = "ktn", noe = "noe", ooe = "ooe", sbg = "sbg", stm = "stm", tir = "tir", vbg = "vbg", wie = "wie",
    burgenland = "bgld", kaernten = "ktn", kaernten = "ktn", niederoesterreich = "noe", oberoesterreich = "ooe",
    salzburg = "sbg", steiermark = "stm", tirol = "tir", vorarlberg = "vbg", wien = "wie", vienna = "wie"
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
  extras <- .state_extra_entry_urls[[code]]
  unique(stats::na.omit(c(row$entry_url, row$secondary_url, extras)))
}

state_backend <- function(code) {
  cfg <- .state_backend_config[.state_backend_config$state == code, ]
  if (nrow(cfg) != 1) cli::cli_abort("Missing backend configuration for `{code}`.")
  cfg
}


source_period_hint <- function(state_code, source_url) {
  m <- .state_source_period_map[[state_code]]
  if (is.null(m)) return(NA_character_)
  out <- unname(m[source_url])
  ifelse(length(out) == 0, NA_character_, out)
}
