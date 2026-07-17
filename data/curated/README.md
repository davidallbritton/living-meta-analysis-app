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

Rows appear in the dropdown in the order they are listed here, so put the most
recently updated files first.
