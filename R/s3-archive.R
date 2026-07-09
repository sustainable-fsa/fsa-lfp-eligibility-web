# s3-archive.R — shared S3 archive helpers for sustainable-fsa data repos.
# Vendored copy; canonical source lives in the migration notes. Generalized
# from fsa-payment-files.R. All functions shell out to the AWS CLI v2 via
# processx and stop() loudly on failure — no silent tryCatch.
#
# Auth: under GitHub Actions (OIDC via aws-actions/configure-aws-credentials),
# ambient env credentials are used. Locally, AWS_PROFILE defaults to "mco".
#
# This copy extends the fsa-disasters vendoring with:
#   - s3_list_keys() also returns LastModified
#   - s3_pull() accepts excludes/includes filters

s3_profile_args <- function() {
  profile <- Sys.getenv("AWS_PROFILE",
                        unset = if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) "" else "mco")
  if (nzchar(profile)) c("--profile", profile) else character(0)
}

s3_run <- function(args, echo = TRUE) {
  processx::run("aws", c(args, s3_profile_args()), echo = echo)
}

s3_preflight <- function() {
  sts <- processx::run("aws",
                       c("sts", "get-caller-identity", s3_profile_args()),
                       error_on_status = FALSE)
  if (sts$status != 0)
    stop("AWS credentials unavailable. Locally: aws sso login --profile ",
         Sys.getenv("AWS_PROFILE", unset = "mco"),
         "; in CI check the OIDC role assumption step.")
  invisible(TRUE)
}

# List all keys under a prefix. Returns a tibble with Key, Size, and
# LastModified (zero rows if the prefix is empty). CLI v2 auto-paginates.
s3_list_keys <- function(bucket, prefix) {
  out <- s3_run(c("s3api", "list-objects-v2",
                  "--bucket", bucket,
                  "--prefix", paste0(prefix, "/"),
                  "--output", "json"),
                echo = FALSE)$stdout
  parsed <- jsonlite::fromJSON(out)
  if (is.null(parsed$Contents))
    return(tibble::tibble(Key = character(0), Size = numeric(0),
                          LastModified = character(0)))
  tibble::as_tibble(parsed$Contents)[, c("Key", "Size", "LastModified")]
}

# Pull prior archive state down before an incremental run (pattern P2).
# excludes/includes are passed through to `aws s3 sync` in that order, so
# e.g. excludes = "*", includes = "*.xlsx" pulls only the xlsx corpus.
s3_pull <- function(bucket, prefix, local_dir = prefix,
                    excludes = NULL, includes = NULL) {
  s3_run(c("s3", "sync",
           paste0("s3://", bucket, "/", prefix, "/"),
           paste0(local_dir, "/"),
           unlist(lapply(excludes, function(x) c("--exclude", x))),
           unlist(lapply(includes, function(x) c("--include", x))),
           "--no-progress"))
  invisible(TRUE)
}

# Push a local staging dir to its prefix.
#   delete = TRUE  -> exact mirror (patterns P1/P2: local dir holds the FULL archive)
#   delete = FALSE -> append-only (pattern P3: local dir holds only new files)
s3_push <- function(bucket, prefix, local_dir = prefix, delete = TRUE,
                    excludes = c("*.DS_Store", ".Rproj.user/*", ".Rhistory",
                                 "_manifest.txt")) {
  s3_run(c("s3", "sync",
           paste0(local_dir, "/"),
           paste0("s3://", bucket, "/", prefix, "/"),
           if (delete) "--delete",
           unlist(lapply(excludes, function(x) c("--exclude", x))),
           "--no-progress"))
  invisible(TRUE)
}

s3_put <- function(bucket, key, file,
                   content_type = "application/octet-stream",
                   cache_control = "max-age=86400") {
  s3_run(c("s3", "cp", file, paste0("s3://", bucket, "/", key),
           "--content-type", content_type,
           "--cache-control", cache_control))
  invisible(TRUE)
}

# Exact-match verification for mirrored prefixes (P1/P2): every local file
# must exist remotely with the same size, and (if delete-mirroring) no
# unexpected remote keys. `allow_extra` lists remote keys that are permitted
# beyond the local set (e.g. "<prefix>/_manifest.txt").
s3_verify <- function(bucket, prefix, local_dir = prefix,
                      allow_extra = paste0(prefix, "/_manifest.txt"),
                      expect_exact = TRUE) {
  remote <- s3_list_keys(bucket, prefix)
  local <- tibble::tibble(
    path = list.files(local_dir, recursive = TRUE, full.names = TRUE)
  )
  local$Key  <- file.path(prefix, sub(paste0("^", local_dir, "/"), "", local$path))
  local$Size <- file.size(local$path)

  missing <- setdiff(local$Key, remote$Key)
  extra   <- setdiff(remote$Key, c(local$Key, allow_extra))
  merged  <- merge(local, remote, by = "Key", suffixes = c(".local", ".s3"))
  mismatch <- merged[merged$Size.local != merged$Size.s3, ]

  problems <- c(
    if (length(missing) > 0) paste0(length(missing), " missing remotely"),
    if (expect_exact && length(extra) > 0) paste0(length(extra), " unexpected remote keys"),
    if (nrow(mismatch) > 0) paste0(nrow(mismatch), " size mismatches")
  )
  if (length(problems) > 0) {
    print(list(missing = missing, extra = extra, mismatch = mismatch$Key))
    stop("S3 verification FAILED for ", prefix, ": ",
         paste(problems, collapse = "; "))
  }
  message("S3 verified: ", nrow(local), " files match under s3://",
          bucket, "/", prefix, "/")
  invisible(TRUE)
}

# Membership verification for append-only prefixes (P3): every uploaded file
# must be present remotely at the right size; extra remote keys are expected.
s3_verify_subset <- function(bucket, prefix, local_dir, subdir_prefix = prefix) {
  s3_verify(bucket, prefix, local_dir,
            allow_extra = character(0), expect_exact = FALSE)
}

# URL-encode an S3 key for the CloudFront manifest. Order matters: encode
# literal "%" first, then spaces. "=" and "/" stay literal — DuckDB's
# hive_partitioning detection parses key=value from the raw URL path.
s3_encode_key <- function(x) {
  x <- gsub("%", "%25", x, fixed = TRUE)
  x <- gsub(" ", "%20", x, fixed = TRUE)
  x <- gsub("#", "%23", x, fixed = TRUE)
  x <- gsub("?", "%3F", x, fixed = TRUE)
  gsub("+", "%2B", x, fixed = TRUE)
}

# Regenerate the prefix's _manifest.txt from the VERIFIED remote listing.
# Skips internal keys (underscore-prefixed path segments, e.g. _cache/).
s3_write_manifest <- function(bucket, prefix,
                              base = "https://data.sustainable-fsa.com") {
  keys <- s3_list_keys(bucket, prefix)$Key
  keys <- keys[!grepl("(^|/)_", sub(paste0("^", prefix, "/"), "", keys))]
  manifest_file <- file.path(tempdir(), "_manifest.txt")
  writeLines(paste0(base, "/", s3_encode_key(sort(keys))), manifest_file)
  s3_put(bucket, paste0(prefix, "/_manifest.txt"), manifest_file,
         content_type = "text/plain", cache_control = "max-age=86400")
  invisible(TRUE)
}

# Invalidate only mutated paths (immutable new weekly files never need it).
cf_invalidate <- function(paths,
                          distribution = "E1BNL6ONVN84RI") {
  if (length(paths) == 0) return(invisible(FALSE))
  s3_run(c("cloudfront", "create-invalidation",
           "--distribution-id", distribution,
           "--paths", paths))
  invisible(TRUE)
}

# HTTP existence probe for cross-archive freshness gates. HEAD via curl;
# retries transient failures, TRUE only on a final 200. The tryCatch guards
# the probe itself, not archive work — a FALSE gates a skip, never a write.
url_exists <- function(url, tries = 3L, pause = 5) {
  for (i in seq_len(tries)) {
    res <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(nobody = TRUE)),
      error = function(e) NULL)
    if (!is.null(res) && res$status_code == 200L) return(TRUE)
    if (i < tries) Sys.sleep(pause)
  }
  FALSE
}

# Freshness-gate skip message. Dispatched runs are expected to find fresh
# upstream data, so a skip there surfaces as a warning annotation; scheduled
# and push runs no-op quietly with a notice.
gate_skip <- function(msg) {
  if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
    level <- if (identical(Sys.getenv("GITHUB_EVENT_NAME"), "workflow_dispatch"))
      "warning" else "notice"
    cat(sprintf("::%s::%s\n", level, msg))
  }
  message(msg)
}
