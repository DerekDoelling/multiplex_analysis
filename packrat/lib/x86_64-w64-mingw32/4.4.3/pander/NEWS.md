# pander 0.6.7

Development version on GitHub.

# pander 0.6.6

This is a maintenance release without major updates,
  https://github.com/Rapporter/pander/compare/v0.6.5...v0.6.6

# pander 0.6.5 (2022-03-17)

This is a maintenance release without major updates,
  https://github.com/Rapporter/pander/compare/v0.6.4...v0.6.5

Fixes:
* check for condition has length > 1
* emphasize rows when no rows passed (#354)

# pander 0.6.4 (2021-06-08)

This is a maintenance release without major updates,
  https://github.com/Rapporter/pander/compare/v0.6.3...v0.6.4

Changes:
* switch from `futile.logger` to `logger` in `evals`

Fixes:
* quote path to Pandoc binary (#331)
* auto-detect path to Pandoc in RStudio. (#332)
* character encoding issues on Windows (#280, #296)
* table column with fix (#321)
* list indent fix (#323)

# pander 0.6.3 (2018-10-06)

This is a maintenance release without major updates,
  https://github.com/Rapporter/pander/compare/v0.6.2...v0.6.3

New fetures:
* Jira table format support

Fixes:
* keeping data.table keys as rownames (#314)
* pander.date passing ... (#328)
* hyphenation with most recent release of korPus -> sylly

# pander 0.6.2 (2018-07-07)

Maintainence release fixing having 1+ conditions in if statements.

# pander 0.6.1 (2017-08-04)

This is rather a maintenance release without major updates, but still 100+ commits since last version:
  https://github.com/Rapporter/pander/compare/v0.6.0...v0.6.1

New features:
* placement of coefficients in `pander.lm` is now optional (#232)
* cutoff overrides for the significance stars (#255)
* row and columns names in `pandoc.table` (#281)

Fixes:
* dplyr/tibble support (#234)
* overriding p values with stars and fix significance stars markdown (#230)
* don't update original markdown file when calling `Pandoc.brew` (#173)
* column alignment of tables without a header (#233)
* matrix coercion issues, eg date accidentally converted to integer (#237)
* formatting of multiple columns instead of the first one
* `pander.mtable` fix after `memisc` update (#246)
* `keep.trailing.zeros` regression bug (#259)
* `pandoc.formula` with caption (#274)
* show estimes of `htest` (#277)
* `rms` update 0.5 fixes on `reVector` to `reListclean`

New classes supported by the `pander` generic S3 method:
* ets (#231)
* data.table (#241)

Classes no longer supported by the `pander` generic S3 method:
* due to the major updates in the `memisc` package, for at least temporarily, we have removed `mtable` support

# pander 0.6.0 (2015-10-27)

This is a major release with 200+ new commits since last version:
  https://github.com/Rapporter/pander/compare/v0.5.2...v0.6.0

New features:
* added logging for evals using futile.logger (#124)
* added support for digits/round as a vector (#146)
* added support for emphasize.verbatim (#221)
* vignettes for pander, pandoc.table, knitr integration and evals

Fixes:
* correct rendering for empty objects (#180)
* correct emphasizing for rows (#176)
* missing column and row titles in CrossTable (#163)
* removal of calls to deprecated functions (#172)
* correct rendering for horizontal mtables (#174)
* correct splitting of tables based on style (#164)
* correct rendering for rmarkdown tables with | (#186)
* pandoc.list with zero element (#191)
* assigning NULL to a list element in (pander|evals)Options
* missing blank line in one-row multiline tables without header (#224)
* adding p-value to htest tables (#227)
* support for ggplot2 1.1.0 (pander and the unify.graphs)

New classes supported by the `pander` generic S3 method:
* summary.table
* randomForest
* tabular
* summary.lme
* manova/summary.manova
* gtable
* nls/summary.nls
* arima
* survreg/summary.survreg
* ols
* rms
* Glm
* cph
* lrm
* summary.rms
* polr/summary.polr

Other changes:
* Pandoc.convert will not add a footer to the document by dafault (#173)
* deprecating pander.return, pander.option and evals.option functions, use pander_return, panderOptions and evalsOptions instead (#229)

pander 0.5.2 (2015-04-19)
----------------------------------------------------------------

New fetures:
* optionally add significance stars for P values (#94)
* specify full path of the pandoc binary (#127)
* define custom "missing value" text in pander outputs (#131)
* more general CrossTable renderer (started at #135)
* LaTeX-style column-alignment (#149)

Fixes:
* Rcpp support for Unicode characters (#144)
* S3 method for a bunch of (lm, glm) summary objects
* disable "knitr.auto.asis" option for recursive calls (#129)
* evals hook trigger
* collapse multiple captions instead of rendering multiple captions (#130)
* preserve digits, decimal mark etc. settings for splitted tables (#133)
* narrower columns without a header (#136)
* read default params of set.alignment from panderOptions (#137)
* render header for 1D tables (#138)
* emphasize cells with missing/empty values (#156)
* total.r and total.c support for CrossTable (#211)

Other changes:
* pander.return and pander.option will be deprecated in the next release (#171)

Full changelog: https://github.com/Rapporter/pander/compare/v0.5.1...v0.5.2

# pander 0.5.1 (2014-10-29)

* refix S3 method exports in NAMESPACE
* revert to non-Rcpp solution with Unicode characters

Full changelog: https://github.com/Rapporter/pander/compare/v0.5.0...v0.5.1

# pander 0.5.0 (2014-10-27)

This is a major release with 200+ new commits since last version:
  https://github.com/Rapporter/pander/compare/v0.3.8...v0.5.0

Special thanks goes to Roman Tsegelskyi, who worked hard on various
parts of the package in Google Summer of Code 2014.

New features:
* no need to specify the `asis` option in `knitr` chunks any more when calling `pander`
* show Pandoc command on error for easier debugging in document conversion
* supporting tables with more than 3 dimensions by converting those to 2D with `ftable`
* optionally keep line breaks inside of cells (keep.line.breaks)
* optionally remove any markup from the table (plain.ascii)
* optionally do not highlight rownames (emphasize.rownames)
* flexible column width of tables
* `Pandoc` reference class updates on enabling the new features of the package

New classes supported by the `pander` generic S3 method:
* CrossTable
* ts
* formula
* coxph
* zoo
* lme
* aovlist
* sessionInfo
* mtable
* survfit
* stat.table
* smooth.spline
* clogit
* rlm
* function
* microbenchmark

Updates:
* dual licensing the package with AGPLv3 and OSL based on user request
* defaults to `cairo` graphics device if available
* removed the `pandoc` alias for `pander` to avoid conflict with `knitr` pkg
* replayPlot workaround for distributed R instances
* partial Rcpp rewrite of pandoc.table for increased performance

Fixes:
* avoid stacking of markup characters in helper functions
* remove dummy image files on Windows (#57)
* `simple` and `rmarkdown` tables cannot be split (#60)
* fix Windows path issues when looking for Pandoc
* `p` function does not fail any more on NULL
* POSIXlt typo

# pander 0.3.8 (2013-08-29)

* dropped custom font for decreased package size (#39)
* defaults to self contained HTML file (#39)
* remove file extension for `vignetteEngine` (#44)
* pander goes stargazer (#46)
* new option for big.mark
* fix: rounding 2D tables (#45)
* fix: pass optional parameters from S3 method to `pandoc.table`
* fix: suppress empty rownames
* further minor fixes: caption, par, text, pander.booelan

Full changelog: https://github.com/Rapporter/pander/compare/v0.3.7...v0.3.8

# pander 0.3.7 (2013-05-27)

* new options for default cell alignment (#41)
* new optional HTML class to manually exclude headers from TOC (#40)

Full changelog: https://github.com/Rapporter/pander/compare/v0.3.6...v0.3.7

# pander 0.3.6 (2013-05-15)

* added Travis-CI for continuous integration tests
* fixed header and footer line of simple and multiline tables
* updated `set.alignment` to also work outside of `Pandoc.brew`
* minor bugfixes for in pandoc helpers, emphasize and S3 methods (#33 #34 among others)

Full changelog: https://github.com/Rapporter/pander/compare/v0.3.5...v0.3.6

# pander 0.3.5 (2013-04-10)

* new functions to highlight parts of a table
* let both US/UK spelling of "center"/"centre" work as alignment parameter for tables (#31)
* fix `pandoc.table` issue with tables holding only one number (#32)
* fix `redraw.recodedplot` compatibility issue with R 3.0.0
* fixing alignment of cells with helper outside of `Pandoc.brew`
* minor documentation improvements

Full changelog: https://github.com/Rapporter/pander/compare/v0.3.4...v0.3.5

# pander 0.3.4 (2013-04-05)

* new options: table caption prefix and split caption messages (#28)
* fix for a rather unnoticeable but rapport-related critical bug that resulted in an invisibly returned wrong chunk type (concatenated image URL to the next paragraph)

Full changelog: https://github.com/Rapporter/pander/compare/v0.3.3...v0.3.4

# pander 0.3.3 (2013-03-27)

* new option: keep or ditch trailing zeros (#27)
* minor bugfixes (#22 #23 #26)

Full changelog: https://github.com/Rapporter/pander/compare/v0.3.2...v0.3.3

# pander 0.3.2 (2013-03-20)

* new table style: rmarkdown thanks to @onesandzeroes

Full changelog: https://github.com/Rapporter/pander/compare/v0.3.1...v0.3.2

# pander 0.3.1 (2013-03-10)

* further tweaks/fixes with ggplot2's new theme system
* table alignment defaults to center
* more informative error messages for `evals` about plotting
* support for CJK chars (#18)
* minor bugfixes (#7 #12 and #16)

Full changelog: https://github.com/Rapporter/pander/compare/v0.3...v0.3.1

# pander 0.3 (2012-10-24)

* using ggplot2's new theme system (0.9.2)
* removed accented chars from examples to let ASCII live long
* added captions to most table-like S3 methods
* new `evals` graph.name option for chunk ID
* new functions to easily toggle caching
* a few minor bugfixes
* new demo document for brewing about Olympics

Full changelog: https://github.com/Rapporter/pander/compare/v0.2...v0.3

# pander 0.2   (2012-08-17)

Initial CRAN release.
