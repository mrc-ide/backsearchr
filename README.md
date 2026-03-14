
# backsearchr

<!-- badges: start -->
<!-- badges: end -->

The goal of backsearchr is to automate backward citation searching. This is a common step in literature reviews -  researchers look through the papers cited in a review to check that
they have considered (included or excluded) all relevant papers.

## Installation
You can install the development version of backsearchr like so:

``` r
remotes::install_github("mrc-ide/backsearchr")
```

## Run with Podman

Use Podman Compose for a reproducible extraction/comparison workflow without
local R/Java setup.

### Prerequisites

- Podman
- Podman Compose (`podman compose`)

### Expected folders

```text
data/
  in/
    pdfs/                 # input PDFs
    provided_refs.csv     # references from your main list
  out/                    # outputs written here
```

Create mount folders once:

```bash
mkdir -p data/in/pdfs data/out
```

Build the image:

```bash
podman compose build
```

The supported local image tag is `localhost/backsearchr:local`. You can verify
that Podman sees it with:

```bash
podman images | grep backsearchr
```

By default the compose setup builds and runs as `linux/amd64`. This keeps
GROBID working on both x86_64 hosts and ARM64 hosts running emulation.
If you need to override that, set `BACKSEARCHR_PLATFORM`:

```bash
BACKSEARCHR_PLATFORM=linux/amd64 podman compose build
```

Extract references with CERMINE:

```bash
podman compose run --rm backsearchr \
  extract \
  --method cermine \
  --indir /work/data/in/pdfs \
  --output /work/data/out/extracted_refs.csv
```

Extract references with GROBID:

```bash
podman compose run --rm backsearchr \
  extract \
  --method grobid \
  --indir /work/data/in/pdfs \
  --output /work/data/out/extracted_refs.csv
```

Compare extracted vs provided references:

```bash
podman compose run --rm backsearchr \
  compare \
  --provided /work/data/in/provided_refs.csv \
  --extracted /work/data/out/extracted_refs.csv \
  --outdir /work/data/out
```

Run end-to-end in one command:

```bash
podman compose run --rm backsearchr \
  extract-and-compare \
  --method grobid \
  --indir /work/data/in/pdfs \
  --provided /work/data/in/provided_refs.csv \
  --outdir /work/data/out
```

Standard outputs in `data/out`:

- `extracted_refs.csv`
- `matched.csv`
- `not_matched.csv`
- `doi_matched.csv`
- `titles_matched.csv`

### Troubleshooting

- Permission denied on output files:
  - Ensure `data/out` is writable on the host.
- Platform and architecture:
  - The default compose platform is `linux/amd64` because GROBID's bundled
    Linux native library is x86_64-only.
  - On ARM64 hosts this usually runs via emulation.
  - If you override `BACKSEARCHR_PLATFORM`, GROBID may fail on non-x86_64
    Linux targets.
- GROBID path override:
  - Set `BACKSEARCHR_GROBID_JAR` and/or `BACKSEARCHR_GROBID_HOME` when running
    compose if you want to use custom assets.
- No PDFs found:
  - Confirm files are under `data/in/pdfs` and `--indir` points to
    `/work/data/in/pdfs`.

## Details

There are two steps to backward citation searching:

1. Given a list of papers, extract the references from each paper. For details, go to the section [Extracting References](#extracting-references). 
2. Given a list of references (call it the main list) and another list of papers that are to be checked for inclusion (call it the check list), identify the papers in the main list that are not in the check list. For details, go to the section [Comparing References](#comparing-references).
    
## Extracting References

There are multiple ways to extract references from a paper, two which we have considered here are:
1. Using an API e.g., rscopus.
2. Using a pdf parser. We support [cermine](https://github.com/CeON/CERMINE) and [GROBID](https://github.com/kermitt2/grobid) for this purpose. The advantages of this method are:
   - parsers can be run locally and do not require an API key. 
   - there is no need to worry about rate limits.
   - there is no need too worry about scopus subscription.

If neither of the above methods suits you, you can choose any other method to compile a list of references as the comparison functionality only needs a list, and is agnostic to how this list is obtained.

## Comparing References

This comparison is non-trivial for several reasons. First, using automatically compiled lists, we cannot reply on all fields used for comparison being present. Second, there might be inconsistences between fiels due to OCR erros, formatting differences, or spurious spaces. We use the following rules to compare papers:
    - If the doi of a paper in the main list is an exact match to the doi of a paper in the check list, then the papers are considered to be the same. However, doi is often missing, especially from older papers. We then use the second rule.
    - If the title of a paper in the main list is an exact match to the title of a paper in the check list, then the papers are considered to be the same. The comaprison is case-insensitive and punctuation is removed.
    - Finally, we revert to comparison of meta-data. If the first author, year of publication, and journal of a paper in the main list are an exact match to the corresponding fields in the check list, then the papers are flagged as being potentially similar. As before, the comparison is case-insensitive and punctuation is removed

``` r
library(backsearchr)
## basic example code
```
