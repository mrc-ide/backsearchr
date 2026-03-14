#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
}

usage <- function() {
  cat(
    paste(
      "backsearchr-task <mode> [options]",
      "",
      "Modes:",
      "  extract",
      "  compare",
      "  extract-and-compare",
      "",
      "Common options:",
      "  --outdir PATH                    Output directory (default: /work/data/out)",
      "",
      "Extract options:",
      "  --method cermine|grobid          Extraction backend (default: cermine)",
      "  --indir PATH                     Directory with PDFs",
      "  --output PATH                    Extracted refs CSV path (default: <outdir>/extracted_refs.csv)",
      "  --grobid-jar PATH                Optional; default from BACKSEARCHR_GROBID_JAR",
      "  --grobid-home PATH               Optional; default from BACKSEARCHR_GROBID_HOME",
      "",
      "Compare options:",
      "  --provided PATH                  Provided refs CSV",
      "  --extracted PATH                 Extracted refs CSV",
      "",
      "Extract-and-compare options:",
      "  --provided PATH                  Provided refs CSV",
      "  --method cermine|grobid          Extraction backend",
      "  --indir PATH                     Directory with PDFs",
      "  --output PATH                    Optional extracted refs CSV output path",
      "",
      "Examples:",
      "  backsearchr-task extract --method cermine --indir /work/data/in/pdfs",
      "  backsearchr-task extract --method grobid --indir /work/data/in/pdfs",
      "  backsearchr-task compare --provided /work/data/in/provided_refs.csv --extracted /work/data/out/extracted_refs.csv",
      "  backsearchr-task extract-and-compare --method grobid --indir /work/data/in/pdfs --provided /work/data/in/provided_refs.csv",
      sep = "\n"
    )
  )
}

parse_opts <- function(args) {
  opts <- list()
  i <- 1L
  while (i <= length(args)) {
    token <- args[[i]]
    if (!startsWith(token, "--")) {
      stop("Unexpected positional argument: ", token, call. = FALSE)
    }
    token <- substring(token, 3L)
    if (grepl("=", token, fixed = TRUE)) {
      key <- sub("=.*$", "", token)
      value <- sub("^[^=]*=", "", token)
      opts[[key]] <- value
      i <- i + 1L
      next
    }
    key <- token
    if (i == length(args) || startsWith(args[[i + 1L]], "--")) {
      opts[[key]] <- TRUE
      i <- i + 1L
      next
    }
    opts[[key]] <- args[[i + 1L]]
    i <- i + 2L
  }
  opts
}

opt_get <- function(opts, key, default = NULL) {
  if (!is.null(opts[[key]])) opts[[key]] else default
}

require_opt <- function(opts, key) {
  value <- opt_get(opts, key)
  if (is.null(value) || isTRUE(value) || !nzchar(value)) {
    stop("Missing required option --", key, call. = FALSE)
  }
  value
}

must_exist_dir <- function(path, label) {
  if (!dir.exists(path)) {
    stop(label, " directory does not exist: ", path, call. = FALSE)
  }
}

must_exist_file <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " file does not exist: ", path, call. = FALSE)
  }
}

write_csv_atomic <- function(df, out_path) {
  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  tmp <- tempfile(pattern = "tmp-backsearchr-", fileext = ".csv", tmpdir = out_dir)
  utils::write.csv(df, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, out_path)) {
    ok <- file.copy(tmp, out_path, overwrite = TRUE)
    unlink(tmp)
    if (!ok) {
      stop("Failed to write output CSV: ", out_path, call. = FALSE)
    }
  }
}

read_csv_checked <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

run_extract <- function(opts, outdir_default = "/work/data/out") {
  method <- tolower(opt_get(opts, "method", "cermine"))
  if (!method %in% c("cermine", "grobid")) {
    stop("Unsupported --method: ", method, ". Use cermine or grobid.", call. = FALSE)
  }

  indir <- require_opt(opts, "indir")
  must_exist_dir(indir, "Input")

  outdir <- opt_get(opts, "outdir", outdir_default)
  output_path <- opt_get(opts, "output", file.path(outdir, "extracted_refs.csv"))

  message("Running extraction with method: ", method)
  args <- list(indir = indir, method = method)

  if (method == "grobid") {
    grobid_jar <- opt_get(opts, "grobid-jar", Sys.getenv("BACKSEARCHR_GROBID_JAR", unset = ""))
    grobid_home <- opt_get(opts, "grobid-home", Sys.getenv("BACKSEARCHR_GROBID_HOME", unset = ""))
    args$grobid_jar <- if (nzchar(grobid_jar)) grobid_jar else NULL
    args$grobid_home <- if (nzchar(grobid_home)) grobid_home else NULL
    message("GROBID jar: ", args$grobid_jar %||% "<package default>")
    message("GROBID home: ", args$grobid_home %||% "<package default>")
  }

  extracted <- do.call(backsearchr::extract_references, args)
  write_csv_atomic(extracted, output_path)
  message("Wrote extracted references (", nrow(extracted), " rows): ", output_path)
  output_path
}

run_compare <- function(opts, outdir_default = "/work/data/out") {
  provided_path <- require_opt(opts, "provided")
  extracted_path <- require_opt(opts, "extracted")
  outdir <- opt_get(opts, "outdir", outdir_default)

  must_exist_file(provided_path, "Provided refs")
  must_exist_file(extracted_path, "Extracted refs")
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }

  provided <- read_csv_checked(provided_path)
  extracted <- read_csv_checked(extracted_path)

  out <- backsearchr::compare_reference_lists(
    provided_refs = provided,
    extracted_refs = extracted
  )
  if (is.null(out)) {
    stop("Comparison failed because required columns were missing.", call. = FALSE)
  }

  files <- list(
    matched = file.path(outdir, "matched.csv"),
    not_matched = file.path(outdir, "not_matched.csv"),
    doi_matched = file.path(outdir, "doi_matched.csv"),
    titles_matched = file.path(outdir, "titles_matched.csv")
  )
  write_csv_atomic(out$matched, files$matched)
  write_csv_atomic(out$not_matched, files$not_matched)
  write_csv_atomic(out$doi_matched, files$doi_matched)
  write_csv_atomic(out$titles_matched, files$titles_matched)

  message("Wrote comparison outputs to ", outdir)
  message("  matched rows: ", nrow(out$matched))
  message("  not_matched rows: ", nrow(out$not_matched))
  files
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0 || args[[1]] %in% c("-h", "--help", "help")) {
    usage()
    quit(status = 0L)
  }

  mode <- args[[1]]
  opts <- parse_opts(args[-1])
  outdir_default <- "/work/data/out"

  if (mode == "extract") {
    run_extract(opts, outdir_default = outdir_default)
    quit(status = 0L)
  }

  if (mode == "compare") {
    run_compare(opts, outdir_default = outdir_default)
    quit(status = 0L)
  }

  if (mode == "extract-and-compare") {
    extracted_output <- run_extract(opts, outdir_default = outdir_default)
    opts$extracted <- extracted_output
    run_compare(opts, outdir_default = outdir_default)
    quit(status = 0L)
  }

  stop(
    "Unsupported mode: ", mode,
    ". Use one of: extract, compare, extract-and-compare.",
    call. = FALSE
  )
}

tryCatch(
  main(),
  error = function(e) {
    message("Error: ", conditionMessage(e))
    message("")
    usage()
    quit(status = 1L)
  }
)
