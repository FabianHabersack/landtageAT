# landtageAT

`landtageAT` provides a unified R interface to parliamentary data from Austria's nine state parliaments (Landtage), while using state-specific backend logic under the hood.

## What v0.2.0 supports

- listing states and backend feature coverage,
- discovering plenary session/protocol links with a harmonized schema,
- downloading protocol files,
- extracting text from PDF/HTML protocols,
- provenance fields (`source_url`, `backend`, `scraped_at`) on outputs,
- graceful degradation when a state endpoint is unavailable (e.g., temporary 503).

## Quick start

```r
library(landtageAT)

list_states()
landtage_supported_features()

# Discover protocols (state-specific backend behavior)
stm <- list_protocols("steiermark", limit = 20)
wie <- list_protocols("wien", limit = 20) # recursive crawl backend

# Download documents
files <- download_protocols(stm, destdir = "data/raw")

# Extract text from a local or remote protocol
txt <- extract_protocol_text(files$file_path[[1]])
```

## Notes on heterogeneity

State systems differ strongly in structure and data depth. `landtageAT` standardizes the user-facing interface, but does not pretend every state exposes the same level of detail. Member-level and speech-level extraction remain planned extension points and are explicitly marked as such.

## Official discovery entry points used

- Burgenland: https://www.bgld-landtag.at/landtagssitzungen/protokolle/xxiii-gp-protokolle
- Kärnten: https://www.ktn.gv.at/Politik/Landtag/Stenographische-Protokolle
- Niederösterreich: https://noe-landtag.gv.at/sitzungen
- Oberösterreich: https://www.land-oberoesterreich.gv.at/12182.htm
- Salzburg: https://www.salzburg.gv.at/pol/landtag/parlamentarische-materialien
- Steiermark: https://www.landtag.steiermark.at/cms/ziel/181952035
- Tirol: https://www.tirol.gv.at/landtag
- Vorarlberg: https://vorarlberg.at/web/landtag/-/protokolle
- Wien: https://www.wien.gv.at/mdb/ltg
