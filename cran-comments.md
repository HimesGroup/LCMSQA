## (Update) Reviewer's comments on the new submission

* Please always write package names, software names and API (application
  programming interface) names in single quotes in title and description. Note
  that package names are case sensitive
  - We checked case-sensitivity of package names and added single quotes

* Please always explain all acronyms in the description text.
  - We spelled out fully LC/MS in the text (liquid chromatography/mass spectrometry)

* Please add small executable examples in your Rd-files
  - Our reviewer confirmed that case examples not need for this package; we
    included the package vignette to illustrate how to use the shiny app.

## Test environments

* Ubuntu 22.04 LTS: R 4.2.2
* GitHub Actions (Ubuntu-lastest): devel, release, oldrel-1
* GitHub Actions (Windows-lastest): release
* GitHub Actions (macOS-lastest): release
* Win-builder: devel, release, oldrelease

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

* Possibly misspelled words in DESCRIPTION: BPC, LCMSQA, XIC, and chromatogram
  - BPC, XIC, and chromatogram: technical terms in the field of metabolomics
  - LCMSQA: the name of package

