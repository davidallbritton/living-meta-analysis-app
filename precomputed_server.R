#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.6 2026.11.23
#
################### Precomputed results that come with the data #####################
#
# The session's four caches (myrvs$previousModels, previousPlots, previousBmrModels,
# previousBmlModels; downloadable in "Saved Plots and Models") start empty.  The
# data loaded in the session may come with a set of precomputed results (see
# precomputedFiles() in global.R): the built-in data's set, a curated entry's
# "precomputed" folder, or a configured copy's default set.  That set is attached
# while its data is loaded:
#   * useCache(type) is called just before every lookup in a cache (and before the
#     cache is downloaded or added to): the first time, it puts the set's entries
#     for that cache at the front of the session's list, reading them from disk if
#     no session has done so yet (precomputedEntries(), shared by the R process);
#   * loading another data file detaches the set: its entries leave the session's
#     lists, and the R process drops them once no session has the set attached.
# Lookups match a cached result by its data and settings, so a set's results are
# only ever used for the data they were computed from.
#
# The lookups run inside isolate(), so adding the entries there invalidates nothing
# that is running.  "Delete Saved ..." empties a cache, set entries included.
#
# This file is sourced (local = T) from server.R.
######################################################################################

cacheListName <- c(models = "previousModels", plots = "previousPlots",
                   bmr = "previousBmrModels", bml = "previousBmlModels")
pc <- new.env()                    # this session's attachment (not reactive)
pc$set <- NULL
pc$added <- setNames(rep(NA_integer_, 4), precomputedTypes)   # NA = not yet added
myrvs$precomputedSet <- NULL       # the attached set, for the loaded-files summary

attachPrecomputedSet <- function(setId) {
  detachPrecomputedSet()
  if (!precomputedExists(setId)) return(invisible())
  acquirePrecomputed(setId)
  pc$set <- setId
  myrvs$precomputedSet <- setId
}

detachPrecomputedSet <- function() {
  if (is.null(pc$set)) return(invisible())
  for (type in precomputedTypes) {
    n <- pc$added[[type]]
    if (!is.na(n) && n > 0) {
      l <- isolate(myrvs[[cacheListName[[type]]]])
      myrvs[[cacheListName[[type]]]] <- l[-seq_len(min(n, length(l)))]
    }
  }
  pc$added[] <- NA_integer_
  releasePrecomputed(pc$set)
  pc$set <- NULL
  myrvs$precomputedSet <- NULL
}

## the session's cache of this type, with the attached set's entries in front
useCache <- function(type) {
  ln <- cacheListName[[type]]
  if (!is.null(pc$set) && is.na(pc$added[[type]])) {
    e <- precomputedEntries(pc$set, type)
    if (length(e)) myrvs[[ln]] <- c(e, isolate(myrvs[[ln]]))
    pc$added[[type]] <- length(e)
  }
  isolate(myrvs[[ln]])
}

## what the "Saved Plots and Models" downloads save: only the results computed from
## the data loaded now (rows added with "Add a Study" included), so a file saved
## for one data set does not carry another's
cacheForDownload <- function(type)
  entriesForData(useCache(type), isolate(myrvs$df.updated))

## "Delete Saved ...": empty the cache; the attached set's entries stay out of it
clearCache <- function(type) {
  myrvs[[cacheListName[[type]]]] <- list()
  if (!is.null(pc$set)) pc$added[[type]] <- 0L
}

## data/precomputed caches are matched with factors as character (factor levels
## are recorded differently on different platforms); see checkOldModels()
cacheMAsNoFactors <- function(entries)
  lapply(entries, function(e) dplyr::mutate_if(as.data.frame(e$MA), is.factor, as.character))

session$onSessionEnded(function() if (!is.null(pc$set)) releasePrecomputed(pc$set))

## the default data's set, at startup
attachPrecomputedSet(defaultPrecomputedSet)
