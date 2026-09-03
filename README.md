# pwr4exp Shiny App

An interactive R Shiny web application for statistical power analysis of research experiments. This app is built on top of the [pwr4exp](https://cran.r-project.org/web/packages/pwr4exp/index.html) R package and provides a graphical interface for calculating statistical power using linear mixed models.

## Live App

The app is deployed at: [https://wangkai.shinyapps.io/power_analyses_app/](https://wangkai.shinyapps.io/power_analyses_app/)

## Running Locally

### Prerequisites

- R (>= 4.4)
- `renv`

Restore the exact package versions used by the project:

```r
install.packages("renv")
renv::restore()
```

For a manual installation without `renv`, install the following packages:

```r
install.packages(c(
  "shiny", "pwr4exp", "bslib", "rhandsontable",
  "data.table", "readxl", "nlme"
))
```

### Launch the App

```r
shiny::runApp("app.R")
```

Or from within the repository directory:

```r
shiny::runApp()
```

## Citation

To cite `pwr4exp` in publications use:

Wang K, Arshad U, Niu M (2025). Invited review: Enhancing quality of dairy cattle research through adequate power analysis. *Journal of Dairy Science*, **108**(9), 8981–9003. <https://doi.org/10.3168/jds.2024-25793>

## Overview

The app supports power analysis for a variety of standard experimental designs and offers flexibility for more complex study designs. It calculates power for both F-tests and t-tests (including pairwise comparisons, polynomial contrasts, and custom contrasts).

## Supported Experimental Designs

| Design | Description |
|--------|-------------|
| **Completely Randomized Design (CRD)** | Treatments randomly assigned to experimental units; supports single or multiple treatment factors |
| **Randomized Complete Block Design (RCBD)** | Experimental units grouped into blocks to control for known sources of variability |
| **Latin Square Design (LSD)** | Two blocking factors (rows and columns) to control two sources of variability; supports replicated squares with options to reuse row or column blocks |
| **Split-Plot Design (SPD)** | Two levels of experimental units (main plots and sub-plots); supports multiple main-plot and sub-plot factors |
| **General Design** | Upload a data file or create an editable table to define a custom design using `lme4`/`nlme` model syntax |

## Features

- **Four-step workflow**: Design setup, Model assumptions, Test settings, and Results & export
- **Flexible factor specification**: Define up to eight treatment factors with any number of levels
- **Custom treatment names**: Replace generated factor and level labels with study-specific names that carry through the model preview, interaction and t-test selectors, assumption tables, results, and CSV exports
- **Interaction effects**: Optionally include interaction terms between factors
- **Means input**: Enter treatment means via an interactive table (cell means for full-factorial models, marginal means for additive models)
- **Variance input**: Specify variance components for each random effect (block, row, column, main-plot, error)
- **F-test power**: Choose Type I, II, or III sums of squares and set the significance level
- **t-test power**: Pairwise, treatment vs. control (`trt.vs.ctrl`), polynomial (`poly`), or custom contrast vectors; supports one-sided and two-sided alternatives; optional Bonferroni adjustment
- **General Design mode**: Upload a data file (CSV, XLSX, XLS, TSV, TXT) or create and paste into an editable table, then specify any `lme4::lmer`-compatible formula along with optional `nlme` residual correlation structures
- **Download results**: Export all power analysis results to a file

## Usage

### Step 1 – Design setup

Choose an experimental design and provide its replication or layout settings. For standard designs, define the treatment factors, factor names, levels, and level names in the **Treatment structure** section. For a custom design, upload a data file or create an editable design table.

- **CRD / RCBD / LSD**: Set the number of treatment factors and their levels. For RCBD, enter the number of blocks. For LSD, set the number of replicated squares and the block-reuse option (`row`, `col`, or `none`).
- **Split-Plot Design**: Set the number of main-plot factors and sub-plot factors, their levels, and the number of main-plot replicates per treatment.
- **General Design**: Upload a data file or create an editable design table and assign each column as a factor or numeric variable.

For standard designs, use **Treatment names** to replace generated labels such as `trt1` or `facA1` with the factor and level names used in your study. Enter one comma-separated level name for every configured level. These names appear consistently throughout the workflow and in exports; the underlying statistical model is unchanged.

### Step 2 – Model assumptions

Review the generated analysis model or enter a custom model formula. Then provide plausible expected responses and variance estimates for random effects and residual error.

For designs with multiple factors, choose whether to include interaction terms and select the desired interaction combinations.

For a General Design, specify the model formula (e.g., `~ fA + fB + (1|block)`) and an optional residual correlation structure (e.g., `corAR1(value=0.6, form=~fA|fC)`).

### Step 3 – Test settings

Choose the effects or comparisons to test and define the testing criteria:

- Choose the **Type of test**: F-test, t-test, or both.
- **F-test**: Select the type of sum of squares (Type I, II, or III) and set the significance level.
- **t-test**: Select the factor of interest, an optional conditioning variable, a contrast method (pairwise, `trt.vs.ctrl`, `poly`, or a custom contrast vector), the alternative hypothesis direction, the significance level, and whether to apply Bonferroni adjustment.

### Step 4 – Results & export

Run the power analysis, review the estimated power, and download a record of the results.

## Dependencies

| Package | Purpose |
|---------|---------|
| [shiny](https://shiny.posit.co/) | Web application framework |
| [pwr4exp](https://cran.r-project.org/web/packages/pwr4exp/index.html) | Power analysis for experimental designs via linear mixed models |
| [bslib](https://rstudio.github.io/bslib/) | Bootstrap theming (`cosmo` preset) |
| [rhandsontable](https://jrowen.github.io/rhandsontable/) | Interactive editable tables for means and variances |
| [readxl](https://readxl.tidyverse.org/) | Read Excel files in General Design mode |
| [data.table](https://rdatatable.gitlab.io/data.table/) | Fast data manipulation |
| [nlme](https://cran.r-project.org/package=nlme) | Residual correlation structures for General Design mode |

## Known issues

General Design currently has open validation and ordering issues for some
residual correlation structures. See
[Known correlation-structure issues](docs/known-correlation-issues.md) for
affected types, temporary workarounds, and the planned fixes.

## Development and tests

Run the regression suite from the repository root:

```r
Rscript tests/testthat.R
Rscript tests/smoke_app.R
# Optional browser-based tests (requires Chrome):
Rscript tests/e2e.R
```

Power-calculation expectations are taken from the documented examples in the
official [pwr4exp vignette](https://an-ethz.github.io/pwr4exp/articles/pwr4exp.html).
The suite also covers upload handling, validation failures, bounded factor
generation, combined F/t calculations, and residual-correlation errors.
Browser-based tests are kept separate from the deployment checks; `tests/e2e.R`
launches the app in headless Chrome, clicks **Power Calculation**, and verifies
the rendered omnibus and contrast tables.

## References

- pwr4exp package documentation: [https://cran.r-project.org/web/packages/pwr4exp/index.html](https://cran.r-project.org/web/packages/pwr4exp/index.html)
- pwr4exp vignette: [https://an-ethz.github.io/pwr4exp/articles/pwr4exp.html](https://an-ethz.github.io/pwr4exp/articles/pwr4exp.html)
- pwr4exp source code: [https://github.com/an-ethz/pwr4exp](https://github.com/an-ethz/pwr4exp)
