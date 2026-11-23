# Curated data files

This folder holds the curated collection of ready-to-load data files offered to
users in the app's "Study criteria" tab (next to the file-upload control), as a
maintained source of updated data for living meta-analyses.

## Adding a data file

1. Put the `.csv` file in this folder.  It must have the same structure as an
   uploaded data file (see the "Explanation" tab in the app, or
   `www/dataFileTemplate.csv`), including the `Begin./End.Selection.Factors`
   and `Begin./End.Selection.Numerics` marker columns.  Use UTF-8 encoding if
   it contains non-ASCII characters.
2. Add a row for it in `curated_index.csv`.  A file is only offered in the app
   if it is listed there, so you can stage a file without exposing it.

## curated_index.csv columns

- `filename` — the file's name in this folder
- `label` — the name shown to users in the dropdown (also displayed as the
  "current data file" name after loading)
- `date` — when this version of the data was last updated
- `description` — one or two sentences shown under the dropdown and in the
  "Explanation" tab
- `citation` — source citation (may be left empty)
- `sharing` — optional: the name of a sharing table in this folder (a .csv
  with columns `ID` and `unit`, one line per membership), loaded together with
  the data file.  Effect sizes that share a unit get correlated sampling
  errors in the multilevel models; all other pairs are independent.  Leave it
  empty, or leave the column out, for the flat structure (every pair from the
  same paper correlated).
- `selections` — optional: the name of a selections file in this folder (a
  .json saved with "Save current selections" in the app), applied when the data
  file is loaded, for example the selections of a published analysis.  Leave it
  empty, or leave the column out, to start with every study included.
- `precomputed` — optional: the name of a folder in this folder holding
  precomputed results for the data file, as saved from the app's "Saved Plots
  and Models" tab, under these names (any of them may be left out):
  `models.RDS` (bayesmeta models), `plots.RDS` (Bayes-factor robustness plots),
  `bmr.RDS` (Bayesian meta-regression models) and `bml.RDS` (Bayesian multilevel
  models).  A result is used whenever the data and settings match the ones it
  was computed with, so the slow Bayesian fits for, say, a published analysis
  appear at once.  The files are read only when first needed and released when
  another data file is loaded.  Keep in each file only the results for this data
  file: a download from "Saved Plots and Models" contains everything cached in
  that session.

Rows appear in the dropdown in the order they are listed here, so put the most
recently updated files first.

A local copy can offer further files that are not part of the release: list them
in `curated_index_local.csv` (same columns), which the app reads after
`curated_index.csv`, and keep that file and its data files out of version control.
If a file is listed in both, the row in `curated_index_local.csv` replaces the one
in `curated_index.csv`, so a local copy can, for example, give a released file a
sharing table.
