# living-meta-analysis-app — guidance for Claude Code sessions

This repository **is** the Shiny app "A General Tool for Living Meta-Analysis"
(v1.3). It is the primary, public development repository — the app source lives
at the repository root (which is why `shiny::runGitHub("living-meta-analysis-app",
"davidallbritton")` works). Work directly here; there is no separate mirror.

## Architecture

- `global.R` loads packages, `source()`s the three helper files, loads the default
  data (`data/2023updatedData.Rda` → `df`), and loads the **process-wide seed caches**
  (`defaultModelsSeed`, `defaultPlotsSeed`, `defaultBmrSeed`, `defaultBmlSeed`) plus the
  brms compiled-template store `bmlTemplates`.
- `server.R` sources, with `local = TRUE` (all share one environment):
  `codeGenerator.R` (+ `metaRegression_code_generator.R`, `descriptives_code_generator.R`),
  `descriptives_server.R`, `selections_server.R`, `bayesianMultilevel_server.R`,
  `metaRegression_server.R`, `bayesianMetaRegression_server.R`.
- Helper files (pure functions, also concatenated into the user-downloadable
  "HelperFunctions.R" bundle by `download_HelperFunctions` in server.R — keep them free of
  side effects): `HelperFunctions.R`, `metaRegressionFunctions.R`,
  `bayesianMultilevelFunctions.R`.

## Data flow invariants

- `MA()` (eventReactive on `recalculateButton`) filters by the sidebar criteria, then
  builds **both** datasets: the returned AGGREGATED table (per the 2-way ID/Papers radio,
  via `MAd::agg`, cor = .5) and the UNaggregated multilevel table exposed as `MAml()`
  (every effect size its own row; `study` labels made unique for metafor).
- Standard analyses (rma, bayesmeta `bma()`, `bmr()`, robustness) use `MA()`;
  multilevel analyses (`fmaMl()` = CHE via `fitMultilevelCHE`, brms `bmlModel()`/
  `bmlRegModel()`) use `MAml()`.  Both families have their own tabs and are always
  available — never re-introduce mode-gating between them.
- `MA()` also stores `myrvs$bayesSnapshot` (priors + moderator at recalculation time).
  **All Bayesian pipelines read settings ONLY from this snapshot**, so settings changes
  do nothing until "(Re)Calculate" — except the robustness Yes/No toggle, which is
  deliberately live.  It also stores `myrvs$moderatorVariesWithinPapers` (Papers mode
  only) backing `moderatorBlockedByPapersAgg()` — the guard against moderators that vary
  within papers being blended by Papers aggregation.

## Hard-won gotchas (violating these caused real bugs)

1. **Same-value writes to `reactiveValues` do NOT invalidate dependents.**  The Bayesian
   confirm-modal observers therefore set their trigger to FALSE *before* showing the
   dialog, so confirmation is a real FALSE→TRUE transition.  Keep that pattern.
2. **Cache lookups compare numeric priors by value** (`samePriorValue`) because saved
   caches contain both integer `0L` and double `0` for the same setting; `identical()`
   alone silently misses and forces slow refits.  MA data frames are still compared with
   `identical()` after factor→character conversion.
3. **brms compile cost**: prior constants are passed as Stan *data* (`stanvars`), so one
   compiled template per model structure (in `bmlTemplates`, per process) serves every
   prior value and dataset via `update()`.  The moderator column is always renamed
   `moderator` so one template serves any moderator.  Never put prior values into the
   prior strings, or every fit recompiles.
4. **`renderPlot(height = ...)` needs `plotOutput(..., height = "auto")`** (or an
   explicit matching container height); otherwise the image overflows the default 400px
   container and overlaps content below.  Plots inside `renderUI` get implicit fixed
   containers — use named outputs instead.
5. **Per-session vs per-process memory**: the seed caches are loaded once in `global.R`
   and assigned into each session's `myrvs` (copy-on-write shares the big objects).
   Don't move `readRDS` back into the server function.
6. **Generated-code parity**: any change to models, guards, or settings must be mirrored
   in `codeGenerator.R` / `metaRegression_code_generator.R` (the downloaded script
   contains BOTH families behind `run_aggregated` / `run_multilevel` /
   `calculate_bma` / `calculate_brms` switches) and, if new functions are involved, in
   the 3-file HelperFunctions download bundle.

## Conventions

- Version header `# v.1.3 2026.07.19` in the main .R files — bump all of them together,
  plus the Explanation tab's "This updated version" text, when releasing.  Keep
  `CITATION.cff` / `.zenodo.json` version fields in step with releases.
- The automated test suite is maintained in the author's private working repository and
  is not part of this published tree.  Boot-check with `shiny::runApp` before finishing.
- Deployment is via rsconnect to shinyapps.io; `rsconnect/` is gitignored (deployment
  records, regenerated locally on deploy — no secrets).  brms on the hosted server is a
  memory issue (2 GB min; 8 GB comfortable): chains run sequentially on shinyapps (cores
  auto-switch via `R_CONFIG_ACTIVE` in bmlSettings) and fits degrade to an explanatory
  message on failure.  Production strategy: ship precomputed fits as
  `data/defaultPrecalculatedBmlModels.RDS` (fit locally, download from Saved Plots and
  Models) so cached requests need no server-side Stan.

## Roadmap: open items by difficulty

Statistically/numerically subtle items (attempt with care, or warn first):
- **Bayes factors + robustness check for the multilevel models** via bridgesampling
  (needs proper priors ✓, many more draws, numerically fragile).
- **Deep Stan-on-server diagnosis** if shinyapps.io deploys fail non-obviously (the bml
  tabs already degrade to an explanatory message on fit failure).

Routine items any session can do: text/UI tweaks, extending descriptives/selections,
using the data file's optional `r` column instead of the fixed 0.5 correlation in
`agg()`/`vcalc()` (small, well-scoped).

Note: the Bayesian CHE analogue (correlated within-paper sampling errors in the brms
multilevel model) is now **implemented**.
