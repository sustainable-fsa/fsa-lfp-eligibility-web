# fsa-lfp-eligibility-web.R
#
# Archive the USDA FSA "Weekly LFP Program Eligibility Maps and Table" page:
# https://www.fsa.usda.gov/resources/programs/livestock-forage-disaster-program-lfp/maps
#
# Each week FSA publishes per-pasture-type eligibility map PDFs and a
# "Counties & Pasture Types Eligible" XLSX table for the current program year
# at NEW dated URLs; the page only links the current week. This script:
#   1. scrapes the maps page and resolves every linked asset (direct links
#      and /documents/ landing pages) to its /sites/default/files/ URL,
#   2. exits early if every asset is already archived (cheap daily no-op),
#   3. downloads new assets and appends them to the S3 archive
#      (s3://sustainable-fsa/fsa-lfp-eligibility-web/data-raw/),
#   4. parses ALL archived XLSX tables and harmonizes them into the same
#      schema as the sustainable-fsa/fsa-lfp-eligibility FOIA archive,
#   5. writes consolidated outputs (deduped-to-latest CSV + parquet, and an
#      all-snapshots parquet preserving every weekly version),
#   6. renders the interactive dashboard and publishes everything to S3 /
#      CloudFront (https://data.sustainable-fsa.com/fsa-lfp-eligibility-web/).
#
# Set DRY_RUN=true to skip all S3/CloudFront writes (reads still occur).
# Set FORCE_REBUILD=true to regenerate and republish the consolidated outputs
# even when the maps page shows nothing new (e.g. after harmonization fixes).
#
# Packages (provided by mt-climate-office/actions/setup-geospatial in CI):
# pak::pak(c("tidyverse", "magrittr", "arrow", "readxl", "xml2", "curl",
#            "digest", "jsonlite", "processx", "tigris", "quarto", "sf"))

library(magrittr)
library(tidyverse)

source("R/s3-archive.R")

## A. Setup ----

s3_preflight()

s3_bucket <- Sys.getenv("S3_BUCKET", unset = "sustainable-fsa")
s3_prefix <- Sys.getenv("S3_PREFIX", unset = "fsa-lfp-eligibility-web")
cloudfront_base <- Sys.getenv("CLOUDFRONT_BASE",
                              unset = "https://data.sustainable-fsa.com")
dry_run <- tolower(Sys.getenv("DRY_RUN", unset = "false")) == "true"
force_rebuild <- tolower(Sys.getenv("FORCE_REBUILD", unset = "false")) == "true"

fsa_base <- "https://www.fsa.usda.gov"
maps_url <- paste0(fsa_base,
                   "/resources/programs/livestock-forage-disaster-program-lfp/maps")

# Akamai in front of fsa.usda.gov stalls or resets requests from non-browser
# User-Agents (including polite identifying ones), so every request uses a
# mainstream browser string.
ua <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
             "AppleWebKit/537.36 (KHTML, like Gecko) ",
             "Chrome/126.0.0.0 Safari/537.36")

fetch <- function(url) {
  res <- curl::curl_fetch_memory(url, handle = curl::new_handle(useragent = ua))
  if (res$status_code != 200L)
    stop("Fetch failed (HTTP ", res$status_code, "): ", url)
  res
}

## B. Scrape the maps page and resolve every asset ----

maps_page <- fetch(maps_url)

hrefs <-
  maps_page$content %>%
  rawToChar() %>%
  xml2::read_html() %>%
  xml2::xml_find_all(".//a[@href]") %>%
  xml2::xml_attr("href") %>%
  unique()

# Landing pages (/documents/<slug>) each contain exactly one link to the
# actual file under /sites/default/files/; assert that so upstream page
# restructuring fails loudly instead of silently corrupting the archive.
resolve_landing <- function(url) {
  Sys.sleep(0.2)
  files <-
    fetch(url)$content %>%
    rawToChar() %>%
    xml2::read_html() %>%
    xml2::xml_find_all(".//a[@href]") %>%
    xml2::xml_attr("href") %>%
    stringr::str_subset(stringr::fixed("/sites/default/files/")) %>%
    unique()
  if (length(files) != 1)
    stop("Expected exactly 1 file link on landing page ", url,
         " but found ", length(files))
  xml2::url_absolute(files, fsa_base)
}

program_year_of <- function(x) {
  dplyr::if_else(
    stringr::str_detect(x, "fy.2009.2011|Fiscal Year 2009"),
    "2009-2011",
    stringr::str_extract(x, "(19|20)\\d{2}")
  )
}

message("Discovering assets linked from ", maps_url)

discovered <-
  tibble::tibble(href = hrefs) %>%
  dplyr::filter(
    stringr::str_detect(href, stringr::fixed("/sites/default/files/")) |
      stringr::str_detect(href, "^(https://www\\.fsa\\.usda\\.gov)?/documents/")
  ) %>%
  dplyr::mutate(
    url = xml2::url_absolute(href, fsa_base),
    kind = dplyr::if_else(
      stringr::str_detect(href, stringr::fixed("/sites/default/files/")),
      "direct", "landing")
  ) %>%
  dplyr::mutate(
    file_url = dplyr::if_else(kind == "direct", url, NA_character_)
  )

discovered$file_url[discovered$kind == "landing"] <-
  purrr::map_chr(discovered$url[discovered$kind == "landing"], resolve_landing)

discovered <-
  discovered %>%
  dplyr::mutate(
    basename = utils::URLdecode(basename(file_url)),
    # program year from the landing slug (its 4-digit year precedes the
    # mm-dd-yy datestamp tail) or from the legacy direct-link basename
    program_year = program_year_of(
      dplyr::if_else(kind == "landing", basename(url), basename)),
    dest = file.path("data-raw", program_year, basename),
    key = file.path(s3_prefix, "data-raw", program_year, basename)
  ) %>%
  dplyr::distinct(key, .keep_all = TRUE)

if (any(is.na(discovered$program_year)))
  stop("Could not derive a program year for: ",
       paste(discovered$url[is.na(discovered$program_year)], collapse = ", "))

message("Discovered ", nrow(discovered), " assets (",
        sum(discovered$kind == "direct"), " direct, ",
        sum(discovered$kind == "landing"), " via landing pages)")

## C. Freshness gate: exit early when nothing is new ----

inventory <- s3_list_keys(s3_bucket, paste0(s3_prefix, "/data-raw"))

new_assets <- dplyr::filter(discovered, !(key %in% inventory$Key))

if (nrow(new_assets) == 0 && !force_rebuild) {
  gate_skip(paste0("All ", nrow(discovered), " assets on the FSA LFP maps ",
                   "page are already archived; nothing to do."))
  quit(save = "no", status = 0)
}

message(nrow(new_assets), " new assets to archive",
        if (force_rebuild) " (FORCE_REBUILD)" else "")

## D. Pull the xlsx corpus, download new assets, append to S3 ----

dir.create("data-raw", showWarnings = FALSE)

# The consolidated outputs are regenerated from ALL archived tables, so pull
# the (small) xlsx corpus down; the (large) PDF corpus stays remote.
s3_pull(s3_bucket, paste0(s3_prefix, "/data-raw"), "data-raw",
        excludes = "*", includes = c("*.xlsx", "*.xls"))

if (nrow(new_assets) > 0) {

purrr::walk(unique(dirname(new_assets$dest)),
            dir.create, recursive = TRUE, showWarnings = FALSE)

# Akamai resets multiplexed HTTP/2 streams under load, so download over
# plain per-host connections (libcurl caps these at 6) and retry stragglers;
# resume = TRUE continues honest partial transfers. A response with a bad
# HTTP status leaves an error page on disk, which resume would corrupt --
# delete those before retrying.
pending <- new_assets
for (attempt in 1:5) {
  downloads <-
    curl::multi_download(
      urls = pending$file_url,
      destfiles = pending$dest,
      resume = TRUE,
      useragent = ua,
      progress = FALSE,
      multiplex = FALSE
    )
  ok <- is.na(downloads$error) &
    downloads$status_code %in% c(200L, 206L, 416L)
  bad_status <- !(downloads$status_code %in% c(200L, 206L, 416L)) &
    !is.na(downloads$status_code)
  unlink(pending$dest[bad_status])
  pending <- pending[!ok, ]
  if (nrow(pending) == 0) break
  message(nrow(pending), " downloads failed on attempt ", attempt,
          "; retrying")
  Sys.sleep(10 * attempt)
}

if (nrow(pending) > 0) {
  print(dplyr::select(pending, file_url, dest))
  stop("FILE DOWNLOAD ERRORS")
}

# Never archive a block/error page served with HTTP 200: every new file must
# carry the magic bytes of its claimed type (PDF: "%PDF"; xlsx: ZIP "PK").
magic_ok <- purrr::map_lgl(new_assets$dest, function(path) {
  sig <- readBin(path, "raw", n = 4)
  if (grepl("\\.pdf$", path, ignore.case = TRUE))
    identical(sig, charToRaw("%PDF"))
  else if (grepl("\\.xlsx?$", path, ignore.case = TRUE))
    identical(sig[1:2], as.raw(c(0x50, 0x4b)))
  else TRUE
})
if (!all(magic_ok)) {
  print(new_assets$dest[!magic_ok])
  stop("DOWNLOADED FILES FAILED CONTENT VALIDATION")
}

# Per-run provenance: the maps page HTML and the full discovery table (with
# sha256 for newly archived files). Written only on runs that archive
# something, so daily no-ops leave no trace.
log_dir <- file.path("data-raw", "log", format(Sys.Date()))
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
writeBin(maps_page$content, file.path(log_dir, "maps-page.html"))

discovered %>%
  dplyr::left_join(
    dplyr::select(inventory, key = Key, archived_size = Size),
    by = "key") %>%
  dplyr::mutate(
    new = key %in% new_assets$key,
    size = dplyr::if_else(new, file.size(dest), archived_size),
    sha256 = dplyr::if_else(
      new,
      purrr::map_chr(dest, ~ digest::digest(.x, algo = "sha256", file = TRUE)),
      NA_character_)
  ) %>%
  dplyr::select(kind, url, file_url, key, program_year, new, size, sha256) %>%
  jsonlite::write_json(file.path(log_dir, "links.json"),
                       pretty = TRUE, auto_unbox = TRUE)

}  # end if (nrow(new_assets) > 0)

if (!dry_run) {
  s3_push(s3_bucket, paste0(s3_prefix, "/data-raw"), "data-raw",
          delete = FALSE)
  s3_verify_subset(s3_bucket, paste0(s3_prefix, "/data-raw"), "data-raw")
}

## E. Parse ALL archived XLSX tables and harmonize ----
#
# Four schema families:
#   F1: fy-2009-2011 workbook (one sheet per year 2008-2011; State "01-AL",
#       County, Year, D2/D3A/D3B/D4 qualifying/ending dates as yyyymmdd)
#   F2: 2012-2023 (FSA_ST_CODE, FSA_CNTY_CODE, PROGRAM_YEAR, PASTURE_TYPE,
#       D2_START_DATE...D4B_END, FACTOR, START, END, MONTHS, ...)
#   F3: 2024-2025 (space-named columns, FSA CODE instead of ST/CNTY codes)
#   F4: 2026+ weekly (PROGRAM YEAR, PASTURE TYPE, DATE OF QUALIFYING DROUGHT,
#       FSA STATE, FSA COUNTY NAME, PAYMENT FACTOR -- no county code)

# Add as-NA any column the harmonization references that no file supplied,
# so the coalesce chains below survive schema drift.
ensure_cols <- function(df, cols) {
  df[setdiff(cols, names(df))] <- NA_character_
  df
}

# Dates arrive as m/d/y strings, ISO strings, yyyymmdd strings, or Excel
# serial numbers depending on the file vintage.
parse_lfp_date <- function(x) {
  suppressWarnings(dplyr::case_when(
    is.na(x) ~ lubridate::NA_Date_,
    stringr::str_detect(x, "/") ~ lubridate::mdy(x),
    stringr::str_detect(x, "^\\d{4}-\\d{2}-\\d{2}") ~ lubridate::as_date(x),
    stringr::str_detect(x, "^\\d{8}$") ~ lubridate::ymd(x),
    stringr::str_detect(x, "^\\d+(\\.\\d+)?$") ~
      lubridate::as_date(as.numeric(x), origin = "1899-12-30"),
    .default = lubridate::as_date(x)
  ))
}

extract_datestamp <- function(x) {
  d8 <- stringr::str_extract(x, "\\d{8}")
  mdY <- stringr::str_extract(x, "\\d{2}-\\d{2}-\\d{4}")
  mdy <- stringr::str_extract(x, "\\d{2}-\\d{2}-\\d{2}(?!\\d)")
  suppressWarnings(dplyr::case_when(
    !is.na(d8) ~ lubridate::ymd(d8),
    !is.na(mdY) ~ lubridate::mdy(mdY),
    !is.na(mdy) ~ lubridate::mdy(mdy),
    .default = lubridate::NA_Date_
  ))
}

read_lfp_xlsx <- function(path) {
  readxl::excel_sheets(path) %>%
    purrr::map(function(sheet) {
      df <- suppressMessages(
        readxl::read_excel(path, sheet = sheet, col_types = "text"))
      if (nrow(df) == 0 || ncol(df) == 0) return(NULL)
      dplyr::rename_with(df, ~ gsub("_", " ", .x))
    }) %>%
    purrr::compact() %>%
    dplyr::bind_rows()
}

# Collapse a county name to a comparison key: transliterate, uppercase, and
# strip punctuation/whitespace, so "St.Francis", "ST FRANCIS", and
# "St. Francis" all match.
normalize_name <- function(x) {
  x %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("[^A-Z0-9]", "")
}

canonical_pasture_types <- c(
  "Annual Crabgrass",
  "Annual Ryegrass",
  "Cool Season Improved Pasture",
  "Forage Sorghum",
  "Full Season Improved Mixed Pasture",
  "Full Season Improved Pasture",
  "Improved Pasture",
  "Long Season Small Grains",
  "Native Pasture",
  "Rangeland",
  "Short Season Fall/Winter Small Grains",
  "Short Season Small Grains",
  "Short Season Small Grains (1)",
  "Short Season Spring Small Grains",
  "Small Grains",
  "Warm Season Improved Pasture"
)

message("Parsing archived XLSX tables")

lfp_raw <-
  list.files("data-raw", pattern = "\\.xlsx?$",
             recursive = TRUE, full.names = TRUE) %>%
  purrr::discard(~ stringr::str_starts(.x, "data-raw/log/")) %>%
  {
    tibble::tibble(
      file = stringr::str_remove(., "^data-raw/"),
      file_datestamp = extract_datestamp(basename(.)),
      data = purrr::map(., read_lfp_xlsx)
    )
  } %>%
  tidyr::unnest(data) %>%
  ensure_cols(c(
    # F1 columns
    "State", "County", "Year", "Pasture Type",
    "D2 Qualifying Date", "D2 Qualiying Date", "D2 Ending Date",
    "D3A Qualifying Date", "D3A Ending Date",
    "D3B Qualifying Date", "D3B Ending Date",
    "D4 Qualifying Date", "D4 Ending Date",
    # F2/F3/F4 columns (the 2025 vintage uses FSA STATE CODE / FSA COUNTY
    # CODE / FSA COUNTY instead of FSA ST CODE / FSA CNTY CODE / FSA COUNTY
    # NAME)
    "FSA ST CODE", "FSA CNTY CODE", "FSA STATE", "FSA COUNTY NAME",
    "FSA STATE CODE", "FSA COUNTY CODE", "FSA COUNTY",
    "FSA State/County CODE", "FSA CODE",
    "PROGRAM YEAR", "PASTURE TYPE",
    "D2 START DATE", "D2 END", "D3A START DATE", "D3A END",
    "D3B START DATE", "D3B END", "D4A START DATE", "D4A END",
    "D4B START DATE", "D4B END",
    "DATE OF QUALIFYING DROUGHT", "DROUGHT FACTOR", "FACTOR",
    "START", "END", "MONTHS",
    "PAYMENT FACTOR", "Eligible Payment Months", "LOWEST"
  ))

fsa_lfp_eligibility <-
  lfp_raw %>%
  dplyr::mutate(
    # Fold the F1 (fy-2009-2011) workbook into the F2+ column space; the
    # "Qualiying" variant is a typo in some of its sheets.
    `PROGRAM YEAR` = dplyr::coalesce(`PROGRAM YEAR`, Year),
    `PASTURE TYPE` = dplyr::coalesce(`PASTURE TYPE`, `Pasture Type`),
    `FSA STATE` = dplyr::coalesce(`FSA STATE`, State),
    `FSA COUNTY NAME` = dplyr::coalesce(`FSA COUNTY NAME`, County),
    `D2 START DATE` = dplyr::coalesce(`D2 START DATE`,
                                      `D2 Qualifying Date`,
                                      `D2 Qualiying Date`),
    `D2 END` = dplyr::coalesce(`D2 END`, `D2 Ending Date`),
    `D3A START DATE` = dplyr::coalesce(`D3A START DATE`, `D3A Qualifying Date`),
    `D3A END` = dplyr::coalesce(`D3A END`, `D3A Ending Date`),
    `D3B START DATE` = dplyr::coalesce(`D3B START DATE`, `D3B Qualifying Date`),
    `D3B END` = dplyr::coalesce(`D3B END`, `D3B Ending Date`),
    `D4A START DATE` = dplyr::coalesce(`D4A START DATE`, `D4 Qualifying Date`),
    `D4A END` = dplyr::coalesce(`D4A END`, `D4 Ending Date`)
  ) %>%
  dplyr::mutate(
    `FSA ST CODE` = dplyr::coalesce(`FSA ST CODE`, `FSA STATE CODE`),
    `FSA CNTY CODE` = dplyr::coalesce(`FSA CNTY CODE`, `FSA COUNTY CODE`),
    `FSA COUNTY NAME` = dplyr::coalesce(`FSA COUNTY NAME`, `FSA COUNTY`),
    `FSA ST CODE` = ifelse(is.na(`FSA ST CODE`),
                           stringr::str_trunc(`FSA STATE`, 2,
                                              side = "right", ellipsis = ""),
                           `FSA ST CODE`),
    `FSA CNTY CODE` = ifelse(is.na(`FSA CNTY CODE`),
                             stringr::str_trunc(`FSA State/County CODE`, 3,
                                                side = "left", ellipsis = ""),
                             `FSA CNTY CODE`),
    `FSA CNTY CODE` = ifelse(is.na(`FSA CNTY CODE`),
                             stringr::str_trunc(`FSA CODE`, 3,
                                                side = "left", ellipsis = ""),
                             `FSA CNTY CODE`),
    `DROUGHT FACTOR` = ifelse(is.na(`DROUGHT FACTOR`), FACTOR,
                              `DROUGHT FACTOR`),
    `PAYMENT FACTOR` = ifelse(is.na(`PAYMENT FACTOR`),
                              `Eligible Payment Months`, `PAYMENT FACTOR`),
    `PAYMENT FACTOR` = ifelse(is.na(`PAYMENT FACTOR`), LOWEST,
                              `PAYMENT FACTOR`),
    # Some vintages (e.g. 2025) store codes as numbers, which read as
    # unpadded text ("1", "11"); NA passes through str_pad untouched.
    `FSA ST CODE` = stringr::str_pad(`FSA ST CODE`, 2, pad = "0"),
    `FSA CNTY CODE` = stringr::str_pad(`FSA CNTY CODE`, 3, pad = "0")
  ) %>%
  dplyr::select(
    file,
    file_datestamp,
    `FSA State Code` = `FSA ST CODE`,
    `FSA County Code` = `FSA CNTY CODE`,
    `FSA County Name` = `FSA COUNTY NAME`,
    `Program Year` = `PROGRAM YEAR`,
    `Pasture Type` = `PASTURE TYPE`,
    `D2 START DATE`, `D2 END`,
    `D3A START DATE`, `D3A END`,
    `D3B START DATE`, `D3B END`,
    `D4A START DATE`, `D4A END`,
    `D4B START DATE`, `D4B END`,
    `Date of Qualifying Drought` = `DATE OF QUALIFYING DROUGHT`,
    `Drought Factor` = `DROUGHT FACTOR`,
    `Grazing Period Start Date` = START,
    `Grazing Period End Date` = END,
    `Maximum Eligible Payment Months` = MONTHS,
    `Payment Factor` = `PAYMENT FACTOR`
  ) %>%
  dplyr::mutate(
    `Disaster Type` = "Drought",
    dplyr::across(
      c(`D2 START DATE`:`D4B END`,
        `Date of Qualifying Drought`,
        `Grazing Period Start Date`,
        `Grazing Period End Date`),
      parse_lfp_date),
    dplyr::across(
      c(`Program Year`, `Drought Factor`,
        `Maximum Eligible Payment Months`, `Payment Factor`),
      as.integer),
    # suppressWarnings: fct_collapse warns when a mapped source level (e.g.
    # Rangeland, a Fire pasture type) is absent from this drought-only data;
    # real drift is caught by the unexpected-levels stop() below
    `Pasture Type` =
      `Pasture Type` %>%
      stringr::str_to_title() %>%
      factor() %>%
      {
        suppressWarnings(forcats::fct_collapse(
          .,
          `Annual Crabgrass` = "Annual Crabgrass",
          `Annual Ryegrass` = "Annual Ryegrass",
          `Cool Season Improved Pasture` = "Cool Season Improved",
          `Forage Sorghum` = "Forage Sorghum",
          `Full Season Improved Mixed Pasture` = "Full Season Improve Mixed",
          `Full Season Improved Pasture` = "Full Season Improved",
          `Improved Pasture` = "Improved Pasture",
          `Long Season Small Grains` = "Long Season Small Grains",
          `Native Pasture` = "Native Pasture",
          `Rangeland` = "Rangeland",
          `Short Season Fall/Winter Small Grains` = "Shrt Ssn Fall_wtr Sml Grn",
          `Short Season Small Grains` = "Short Season Small Grains",
          `Short Season Small Grains (1)` = "Shrt Season Small Grain 1",
          `Short Season Spring Small Grains` = "Short Ssn Spring Sml Grn",
          `Small Grains` = "Small Grains",
          `Warm Season Improved Pasture` = "Warm Season Improved"
        ))
      } %>%
      forcats::fct_expand(canonical_pasture_types) %>%
      forcats::fct_relevel(canonical_pasture_types)
  )

unexpected_types <-
  setdiff(levels(fsa_lfp_eligibility$`Pasture Type`), canonical_pasture_types)
if (length(unexpected_types) > 0)
  stop("Unexpected pasture types (extend the fct_collapse map): ",
       paste(unexpected_types, collapse = "; "))

# Authoritative FIPS county roster (current + 2014 vintages, so both sides
# of county renames like Shannon/Oglala Lakota are present). Used for the
# name-based code resolution below and the final FIPS name join.
fips_counties <-
  dplyr::bind_rows(
    tigris::counties(cb = TRUE) %>%
      sf::st_drop_geometry(),
    tigris::counties(cb = TRUE, year = 2014) %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(
        tigris::states(cb = TRUE, year = 2014) %>%
          sf::st_drop_geometry() %>%
          dplyr::transmute(STATEFP, STATE_NAME = NAME)
      ) %>%
      dplyr::arrange(STATEFP, COUNTYFP)
  ) %>%
  dplyr::transmute(`FIPS State Code` = STATEFP,
                   `FIPS County Code` = COUNTYFP,
                   `FIPS County Name` = NAME,
                   `FIPS State Name` = STATE_NAME) %>%
  tibble::as_tibble() %>%
  dplyr::distinct() %>%
  dplyr::arrange(`FIPS State Code`, `FIPS County Code`)

# The fy-2009-2011 workbook (F1) and the current-year weekly tables (F4)
# carry no county codes, and their county names are messy (squashed
# "LosAngeles", abbreviated "W BATON ROUGE", typo'd "CULPEPPER"). Resolve
# codes by normalized name against (1) the archive's own coded rows, then
# (2) the FIPS roster -- FSA and FIPS codes agree except for the handful of
# cases the reconciliation below fixes, and what matters downstream is the
# FIPS identity.
county_name_fixes <- c(
  "W BATON ROUGE" = "WEST BATON ROUGE",
  "CULPEPPER" = "CULPEPER",
  "NORTH ST LOUIS" = "ST LOUIS",   # FSA splits St. Louis County, MN
  "SOUTH ST LOUIS" = "ST LOUIS",
  "ST.JOHNTHEBA" = "ST JOHN THE BAPTIST"
)

lookup_archive <-
  fsa_lfp_eligibility %>%
  dplyr::filter(!is.na(`FSA County Code`)) %>%
  dplyr::mutate(name_norm = normalize_name(`FSA County Name`)) %>%
  dplyr::arrange(dplyr::desc(file_datestamp)) %>%
  dplyr::distinct(`FSA State Code`, name_norm, .keep_all = TRUE) %>%
  dplyr::select(`FSA State Code`, name_norm,
                lookup_county_code = `FSA County Code`)

lookup_fips <-
  fips_counties %>%
  dplyr::transmute(
    # translate FIPS state codes to their FSA equivalents for the join
    `FSA State Code` = dplyr::case_match(`FIPS State Code`,
                                         "60" ~ "03", # American Samoa
                                         "66" ~ "14", # Guam
                                         "72" ~ "43", # Puerto Rico
                                         "78" ~ "52", # US Virgin Islands
                                         .default = `FIPS State Code`),
    name_norm = normalize_name(`FIPS County Name`),
    lookup_county_code = `FIPS County Code`
  ) %>%
  # where a state has a county and an independent city of the same name
  # (e.g. Franklin, VA), prefer the county (lower code)
  dplyr::arrange(`FSA State Code`, lookup_county_code) %>%
  dplyr::distinct(`FSA State Code`, name_norm, .keep_all = TRUE)

county_lookup <-
  dplyr::bind_rows(lookup_archive, lookup_fips) %>%
  dplyr::distinct(`FSA State Code`, name_norm, .keep_all = TRUE)

# FSA's exports print "???" for the names of FSA-specific administrative
# units (identified in sustainable-fsa/fsa-counties-dd22, FSA's own county
# layer). When such a row also lacks codes (the 2026+ weekly format), the
# unit is recoverable from the state alone -- except Minnesota, which has
# three distinct split-county units (East/West Otter Tail, East/West Polk,
# North/South St. Louis); those fall through to the unidentifiable drop
# below.
mystery_units <- tibble::tribble(
  ~`FSA State Code`, ~mystery_county_code,
  "12", "025", # "Dade,Monroe" unit, FL
  "19", "156", # West Pottawattamie, IA
  "23", "002", # Houlton unit of Aroostook, ME (002/004 both map to FIPS 003)
  "29", "193", # Ste. Genevieve, MO
  "32", "035"  # Southeast Nye, NV
)

fsa_lfp_eligibility <-
  fsa_lfp_eligibility %>%
  dplyr::left_join(mystery_units, by = "FSA State Code") %>%
  dplyr::mutate(
    `FSA County Code` = dplyr::if_else(
      is.na(`FSA County Code`) & `FSA County Name` %in% "???",
      mystery_county_code,
      `FSA County Code`)
  ) %>%
  dplyr::select(-mystery_county_code)

unidentifiable <-
  fsa_lfp_eligibility %>%
  dplyr::filter(is.na(`FSA County Code`),
                `FSA County Name` %in% c("???", NA))
if (nrow(unidentifiable) > 0) {
  message("Dropping ", nrow(unidentifiable),
          " rows with unidentifiable county names")
  fsa_lfp_eligibility <-
    dplyr::anti_join(fsa_lfp_eligibility, unidentifiable,
                     by = names(unidentifiable))
}

fsa_lfp_eligibility <-
  fsa_lfp_eligibility %>%
  dplyr::mutate(
    name_norm = stringr::str_to_upper(`FSA County Name`) %>%
      dplyr::recode(!!!county_name_fixes) %>%
      normalize_name()
  ) %>%
  dplyr::left_join(county_lookup,
                   by = c("FSA State Code", "name_norm")) %>%
  dplyr::mutate(
    `FSA County Code` = dplyr::coalesce(`FSA County Code`,
                                        lookup_county_code)
  ) %>%
  dplyr::select(-name_norm, -lookup_county_code)

unresolved <-
  fsa_lfp_eligibility %>%
  dplyr::filter(is.na(`FSA County Code`)) %>%
  dplyr::distinct(`FSA State Code`, `FSA County Name`)
if (nrow(unresolved) > 0) {
  print(unresolved, n = Inf)
  stop("Could not resolve FSA county codes for ", nrow(unresolved),
       " (state, county name) pairs; extend county_name_fixes or the lookup.")
}

## FSA -> FIPS reconciliation (identical to fsa-lfp-eligibility) ----

fsa_lfp_eligibility <-
  fsa_lfp_eligibility %>%
  dplyr::mutate(
    # Recode weird FIPS codes
    `FIPS State Code` = dplyr::case_match(`FSA State Code`,
                                          "03" ~ "60", # American Samoa
                                          "14" ~ "66", # Guam
                                          "43" ~ "72", # Puerto Rico
                                          "52" ~ "78", # US Virgin Islands
                                          .default = `FSA State Code`
    ),
    `FIPS County Code` = dplyr::case_when(
      `FIPS State Code` == "66" & `FSA County Code` == "001" ~ "010", # Guam
      `FIPS State Code` == "78" & `FSA County Code` == "001" ~ "010", # St. Croix, USVI
      `FIPS State Code` == "78" & `FSA County Code` == "003" ~ "020", # St. John, USVI
      `FIPS State Code` == "78" & `FSA County Code` == "005" ~ "030", # St. Thomas, USVI
      `FIPS State Code` == "23" & `FSA County Code` %in% c("002", "004") ~ "003", # Houlton & Fort Kent FSA units to Aroostook, ME
      `FIPS State Code` == "46" & `FSA County Code` == "113" & `Program Year` > 2015 ~ "102", # Shannon, SD to Oglala Lakota, SD
      `FIPS State Code` == "32" & `FSA County Code` == "035" ~ "023", # Nye, NV
      `FIPS State Code` == "29" & `FSA County Code` == "193" ~ "186", # Ste. Genevieve, Missouri
      `FIPS State Code` == "27" & `FSA County Code` == "138" ~ "137", # St. Louis, MN
      `FIPS State Code` == "27" & `FSA County Code` == "120" ~ "119", # Polk, MN
      `FIPS State Code` == "27" & `FSA County Code` == "112" ~ "111", # Otter Tail, MN
      `FIPS State Code` == "19" & `FSA County Code` == "156" ~ "155", # Pottawattamie, IA
      `FIPS State Code` == "12" & `FSA County Code` == "025" ~ "086", # Dade, FL to Miami-Dade, FL
      .default = `FSA County Code`
    ),
    `FSA County Name` = stringr::str_to_upper(`FSA County Name`)
  ) %>%
  dplyr::left_join(
    fips_counties,
    relationship = "many-to-many"
  )

# Every record must resolve to a real FIPS county; a silent drop here is how
# bad codes disappear unnoticed, so fail loudly instead.
fips_unmatched <-
  fsa_lfp_eligibility %>%
  dplyr::filter(is.na(`FIPS County Name`)) %>%
  dplyr::distinct(`FSA State Code`, `FSA County Code`, `FSA County Name`)
if (nrow(fips_unmatched) > 0) {
  print(fips_unmatched, n = Inf)
  stop(nrow(fips_unmatched), " (state, county) codes failed the FIPS join; ",
       "extend the FSA -> FIPS reconciliation.")
}

fsa_lfp_eligibility <-
  fsa_lfp_eligibility %>%
  dplyr::select(
    `FIPS State Code`,
    `FIPS County Code`,
    `FIPS State Name`,
    `FIPS County Name`,
    file,
    file_datestamp,
    `FSA State Code`,
    `FSA County Code`,
    `FSA County Name`,
    `Program Year`,
    `Pasture Type`,
    `Disaster Type`,
    `D2 START DATE`, `D2 END`,
    `D3A START DATE`, `D3A END`,
    `D3B START DATE`, `D3B END`,
    `D4A START DATE`, `D4A END`,
    `D4B START DATE`, `D4B END`,
    `Date of Qualifying Drought`,
    `Drought Factor`,
    `Grazing Period Start Date`,
    `Grazing Period End Date`,
    `Maximum Eligible Payment Months`,
    `Payment Factor`
  )

## F. Consolidated outputs ----

# Every weekly version of every record, for studying how eligibility for a
# program year accrued through the season.
snapshots <-
  fsa_lfp_eligibility %>%
  dplyr::arrange(dplyr::desc(`Program Year`),
                 dplyr::desc(file_datestamp),
                 `FIPS State Code`,
                 `FIPS County Code`,
                 `Pasture Type`)

# Latest published version of each record: directly comparable to the
# sustainable-fsa/fsa-lfp-eligibility FOIA archive.
current <-
  snapshots %>%
  dplyr::arrange(`FIPS State Code`,
                 `FIPS County Code`,
                 dplyr::desc(`Program Year`),
                 dplyr::desc(file_datestamp),
                 `Pasture Type`,
                 `Disaster Type`) %>%
  dplyr::distinct(
    `FIPS State Code`,
    `FIPS County Code`,
    `Program Year`,
    `Pasture Type`,
    `Disaster Type`,
    .keep_all = TRUE
  ) %>%
  dplyr::arrange(dplyr::desc(`Program Year`),
                 `FIPS State Code`,
                 `FIPS County Code`,
                 `FSA County Name`,
                 `Disaster Type`,
                 `Pasture Type`)

message("Writing consolidated outputs (",
        nrow(current), " current rows; ",
        nrow(snapshots), " snapshot rows)")

readr::write_excel_csv(current, "fsa-lfp-eligibility-web.csv")

arrow::write_parquet(current,
                     "fsa-lfp-eligibility-web.parquet",
                     version = "latest",
                     compression = "zstd",
                     use_dictionary = TRUE)

arrow::write_parquet(snapshots,
                     "fsa-lfp-eligibility-web-snapshots.parquet",
                     version = "latest",
                     compression = "zstd",
                     use_dictionary = TRUE)

## G. Render the interactive dashboard ----

quarto::quarto_render("fsa-lfp-eligibility-web.qmd")

## H. Manifest of the raw archive, from the verified remote listing ----
## I. Publish consolidated outputs to S3 and invalidate CloudFront ----

if (!dry_run) {
  s3_list_keys(s3_bucket, paste0(s3_prefix, "/data-raw")) %>%
    dplyr::transmute(
      path = stringr::str_remove(Key, paste0("^", s3_prefix, "/")),
      size = Size,
      mtime = format(lubridate::as_datetime(LastModified),
                     "%Y-%m-%d %H:%M:%S")
    ) %>%
    dplyr::arrange(path) %>%
    jsonlite::write_json("manifest.json", pretty = TRUE, auto_unbox = TRUE)

  s3_put(s3_bucket, paste0(s3_prefix, "/fsa-lfp-eligibility-web.csv"),
         "fsa-lfp-eligibility-web.csv",
         content_type = "text/csv", cache_control = "max-age=3600")
  s3_put(s3_bucket, paste0(s3_prefix, "/fsa-lfp-eligibility-web.parquet"),
         "fsa-lfp-eligibility-web.parquet",
         content_type = "application/vnd.apache.parquet",
         cache_control = "max-age=3600")
  s3_put(s3_bucket,
         paste0(s3_prefix, "/fsa-lfp-eligibility-web-snapshots.parquet"),
         "fsa-lfp-eligibility-web-snapshots.parquet",
         content_type = "application/vnd.apache.parquet",
         cache_control = "max-age=3600")
  s3_put(s3_bucket, paste0(s3_prefix, "/fsa-lfp-eligibility-web.html"),
         "fsa-lfp-eligibility-web.html",
         content_type = "text/html", cache_control = "max-age=3600")
  s3_put(s3_bucket,
         paste0(s3_prefix, "/assets/fsa-lfp-eligibility-web-simple.csv"),
         "assets/fsa-lfp-eligibility-web-simple.csv",
         content_type = "text/csv", cache_control = "max-age=3600")
  s3_put(s3_bucket, paste0(s3_prefix, "/manifest.json"),
         "manifest.json",
         content_type = "application/json", cache_control = "max-age=3600")

  s3_write_manifest(s3_bucket, s3_prefix, base = cloudfront_base)

  cf_invalidate(
    paths = paste0("/", s3_prefix, "/",
                   c("fsa-lfp-eligibility-web.csv",
                     "fsa-lfp-eligibility-web.parquet",
                     "fsa-lfp-eligibility-web-snapshots.parquet",
                     "fsa-lfp-eligibility-web.html",
                     "assets/fsa-lfp-eligibility-web-simple.csv",
                     "manifest.json",
                     "_manifest.txt"))
  )
}

message("Done.")
