
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

## Details

There are two steps to backward citation searching:

1. Given a list of papers, extract the references from each paper. For details, go to the section [Extracting References](#extracting-references). 
2. Given a list of references (call it the main list) and another list of papers that are to be checked for inclusion (call it the check list), identify the papers in the main list that are not in the check list. For details, go to the section [Comparing References](#comparing-references).
    
## Extracting References

There are multiple ways to extract references from a paper, two which we have considered here are:
1. Using an API e.g., rscopus.
2. Using a pdf parser. We will use [cermine](https://github.com/CeON/CERMINE) for this purpose. The advantages of using this method are:
   - cermine can be run locally and does not require an API key. 
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
s 
