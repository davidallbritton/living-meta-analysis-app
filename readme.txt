Fix this in the next release (after 1.52)

Why loading the old file caused trouble. You began from the October settings file, which was saved when the data looked different. Since then the data was rebuilt, with Kim et al. recoded and the Sedlmeier & Gigerenzer five-week rows added. Restoring the old filter states against the new data left two filters quietly excluding rows:

The sample-size slider. In October the smallest intervention group in the data had 10 participants, so the saved slider ran from 10 to 610. “Everything” at the time, stored as numbers. The five-week rows describe only the participants who came back, and two of those groups have 5 and 7 people. A slider starting at 10 shuts them out.
The intervention-condition-name checklist. Kim et al.’s second arm, CP-FT (formula-based training), used to be coded as a control, so its name sat in the control-condition-name column. The recoding made it a training, so its name now sits in the intervention-condition-name column. The old file never had it ticked there, because it didn’t exist there in October. So the app leaves out both CP-FT rows.

Neither is a choice you made; both are leftovers. That’s why the app shows 99 rather than the planned 103.

Why our own check didn’t catch it. run_analysis.Rmd doesn’t run the app’s filters. Its apply_selections() reads only the four selections that are meant to exclude something, on the assumption that every other filter is set to everything. That assumption failed here, so the script would analyze 103 rows while the app analyzes 99. That matters for the Method, which says the selections file lets the app reproduce the analysis. I found it only because the descriptives spreadsheet reports its own count.

The empty descriptives settings. This one is my best guess, not something I’ve confirmed. In Shiny apps, a tab’s controls often don’t exist until the tab is first opened. If the settings were saved before the Descriptives tab was opened, its choices would be saved as empty, which is what the file shows. The spreadsheet, downloaded a few minutes later, has the full set of tables, so the choices were in place by then. The R code was downloaded in between and has no descriptives section either, which fits.

What fixes it. In the app:

Drag the sample-size slider out to the full range, 5 to 610.
Tick “CP-FT (conditional probability formula-based training)” in the intervention-condition-name list.
Check it reads 103 effect sizes from 26 papers.
Open the Descriptives tab.
Save the settings file.
Download the R code and descriptives.

I’ll then rerun the check that tests every filter, not just the four, against the masked data.

To stop this happening again, I’d add a guard to apply_selections(). It would apply the app’s other filters too, or at least stop with a message if any of them removes a row. A leftover like these would then halt the analysis instead of slipping through.