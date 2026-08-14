# A General Tool for Living Meta-Analysis

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21938551.svg)](https://doi.org/10.5281/zenodo.21938551)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

A Shiny web application for interactive, reproducible meta-analysis that can be
kept up to date as new studies appear ("living" meta-analysis). It supports
aggregated and multilevel effect-size models, frequentist (`metafor`) and
Bayesian (`bayesmeta`, `brms`) estimation, meta-regression, correlated
within-paper sampling errors (CHE), Bayes-factor robustness checks, descriptive
summaries, and downloadable reproducible R scripts for every analysis.

## Run it

Directly from GitHub, in an R session:

```r
# install.packages("shiny")
shiny::runGitHub("living-meta-analysis-app", "davidallbritton")
```

Or clone and run locally:

```bash
git clone https://github.com/davidallbritton/living-meta-analysis-app.git
```

```r
shiny::runApp("living-meta-analysis-app")
```

The app loads a default dataset on startup and lets you upload your own data or
choose from curated data files (see `data/curated/README.md` for the required
file format). The Bayesian multilevel models compile Stan code via `brms` on
first use; subsequent fits reuse the compiled templates.

## Lineage and citation

This app is the successor to, and a substantial extension of, the tool described
in:

> Allbritton, D., Gómez, P., Angele, B., Vasilev, M. R., & Perea, M. (2024).
> Breathing Life Into Meta-Analytic Methods. *Journal of Cognition, 7*(1), 61.
> https://doi.org/10.5334/joc.389

If you use this software, please cite it using the metadata in
[`CITATION.cff`](CITATION.cff), or by DOI:

> Allbritton, D. (2026). *A General Tool for Living Meta-Analysis* (Version 1.3)
> [Computer software]. https://doi.org/10.5281/zenodo.21938551

That is the concept DOI: it always resolves to the most recent release. To cite
version 1.3 specifically, use https://doi.org/10.5281/zenodo.21938552.

## License

Released under the [MIT License](LICENSE).
