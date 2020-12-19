# ------------------------------------------------------------------------------
#
# Script name: make-compounds.R
#
# Purpose of script: Scripts to create compounds in text.
#
# Author: Johann Gründl
# Email: mail@johanngruendl.at
#
# Date created: 2020-06-09
# Date updated: 2020-06-09
#
# ******************************************************************************


#' Creates a single compound
#'
#' @description This is an internal function use in the
#' .make_compounds_for_single_pattern function. It is basically a wrapper around
#' stringr::str_replace_all. By default, it replaces all space characters (" ")
#' with underline character ("_").
#'
#' @param compound A simple text (= character vector).
#' @param wordsep The word separator to look for, usually spaces. Defaults to
#' " ".
#' @param concatenator The replacement for wordsep characters (e.g., " ").
#' Defaults to "_".
#'
#' @return The provided text where all occurrences of the wordsep character
#' (by default, " ") were replaced by the concatenator character (by default,
#' "_").
.create_single_compound <- function(compound, wordsep = " ", concatenator = "_") {
  return(stringr::str_replace_all(compound, wordsep, concatenator))
}


#' Replace matches for a single pattern with compounds
#'
#' @description This is an internal function use in the make_compounds function.
#'
#' @param text Text should be provided as a character vector.
#' @param pattern A single pattern with possible regular expressions.
#' @param wordsep The word separator to look for, usually spaces. Defaults to
#' " ".
#' @param concatenator The replacement for wordsep characters (e.g., " ").
#' Defaults to "_".
#' @param lazy Should regular expressions be transformed to lazy versions?
#' Defaults to TRUE to return the shortest possible compounds.
#' @param ignore_case Should the case be ignored when searching for pattern
#' matches? Defaults to TRUE.
#'
#' @return The provided text where all occurrences of the pattern have been
#' converted to multi-word compounds.
.make_compounds_for_single_pattern <- function(
  text,
  pattern,
  wordsep = " ",
  concatenator = "_",
  lazy = TRUE,
  ignore_case = TRUE
) {
  if (lazy) {
    pattern <- regexhelpeR::make_all_regex_lazy(pattern)
  }
  matchindices <- stringr::str_locate_all(
    text,
    stringr::regex(pattern, ignore_case = ignore_case)
  )
  if (length(matchindices) == 1) {
    matchindices <- matchindices[[1]]
  } else {
    stop("Not a single text provided for make_compound_text.")
  }
  if (nrow(matchindices) < 1) {return(text)}
  for (i in 1:nrow(matchindices)) {
    ind <- matchindices[i, ]
    stringr::str_sub(text, ind[1], ind[2]) <-
      .create_single_compound(stringr::str_sub(text, ind[1], ind[2]),
                              wordsep, concatenator)
  }
  return(text)
}


#' Create compounds for complex patterns
#'
#' @description For a list of (multi-word) patterns compounds are created. The
#' function works with quanteda corpus objects and regular texts (character
#' vectors; in general text that can be transformed to a quanteda corpus object
#' by quanteda's corpus() function should work). It expects regular expression
#' patterns but can work with glob expressions as well (if parameter glob is
#' set, internally the patterns are transformed to regular expressions).
#' quanteda has built-in functionality for this; however, it does not
#' allow for patterns which include wildcards that stand for multiple words.
#' For example, something like "the * people" could capture "the honest
#' people", "the hard-working, common people", or "the singer sings songs about
#' people". Such patterns would not work as expected in quanteda. With regular
#' expressions, a lot more sophisticated patterns become possible.
#'
#' @param text A quanteda corpus object or something that can be transformed to
#' a corpus by quanteda::corpus(), for example, a simple character vector
#' @param patterns A character vector where each element is a pattern or a
#' quanteda dictionary object. Patterns are expected to be regular expressions
#' (if glob parameter is not set) or only include glob-style wildcards (if glob
#' parameter is set to TRUE).
#' @param wordsep The word seperator, usually simply a space. Defaults to " ".
#' @param concatenator The character for creating multi-word compounds, defaults
#' to "_".
#' @param at_level At which level should patterns be applied. Possible values
#' are "documents", "sentences", or "paragraphs". Defaults to "sentences".
#' @param glob Do the provided patterns use glob-style wildcards instead of
#' regular expressions? Defaults to FALSE.
#' @param lazy Should regular expressions be transformed to lazy versions?
#' Defaults to TRUE to return the shortest possible compounds.
#' @param ignore_case Should the case be ignored when searching for pattern
#' matches? Defaults to TRUE.
#' @param optimize_regex Should the regular expressions be optimized to allow
#' for quicker lookups (see regexhelpeR package)? Defaults to TRUE.
#'
#' @return The corpus or text object where matched multi-word terms are now
#' replaced by multi-word compounds.
#'
#' @export
make_compounds <- function(
  text,
  patterns,
  wordsep = " ",
  concatenator = "_",
  at_level = "sentences",
  glob = FALSE,
  lazy = TRUE,
  ignore_case = TRUE,
  optimize_regex = TRUE
) {
  is_quanteda_corpus <- quanteda::is.corpus(text)
  if (!is_quanteda_corpus) {
    text <- quanteda::corpus(text)
  }
  # Prepare patterns
  if (quanteda::is.dictionary(patterns)) {
    patterns <- unlist(patterns)
  }
  if (glob) {
    message("Provided glob patterns are replaced with regex patterns.")
    patterns <- regexhelpeR::glob_to_regex(patterns)
  }
  if (optimize_regex) {
    patterns <- regexhelpeR::optimize_regex_patterns(patterns)
  }
  if (length(patterns) < 1) {
    warning("No pattern was provided.")
    return(text)
  }
  patterns <- patterns[order(stringr::str_length(patterns), patterns)]
  patterns <- patterns[stringr::str_detect(patterns, wordsep)]
  patterns <- unique(patterns)
  if (length(patterns) < 1) {
    warning("No multi-word pattern was provided.")
    return(text)
  }

  # Reshape corpus to apply patterns at specified level
  old_level <- quanteda::meta(text, type = "all")$unit
  if (is.null(old_level)) {
    # Older corpus objects do not have the unit meta field -> update corpus
    warning(paste0('Corpus did not include the "unit" meta field. ',
                   'Possibly an old corpus generated with quanteda ',
                   'version < 2.0. The corpus was recast to new corpus ',
                   'object.'))
    text <- quanteda::corpus(text)
    old_level <- quanteda::meta(text, type = "all")$unit
  }
  if (at_level != old_level) {
    message(paste0("Corpus is reshaped to level ", at_level))
    text <- quanteda::corpus_reshape(text, to = at_level)
  }

  # Make compounds for the whole corpus for every pattern
  patterntimes <- rep(NA_real_, length(patterns))
  names(patterntimes) <- patterns
  for (i in 1:length(patterns)) {
    time <- Sys.time()
    message(paste0("Run compound generation for whole text with pattern: ",
                   i, "/", length(patterns)))
    quanteda::texts(text) <- sapply(
      quanteda::texts(text),
      .make_compounds_for_single_pattern,
      pattern = patterns[i],
      wordsep = wordsep,
      concatenator = concatenator,
      lazy = lazy,
      ignore_case = ignore_case
    )
    time <- as.numeric(Sys.time() - time, units = "secs")
    patterntimes[i] <- time
    message(
      paste0(
        patterns[i], " took ", time, " to complete. ",
        "The average time for a pattern is now: ",
        round(mean(patterntimes, na.rm = TRUE), 1),
        " seconds. The remaining ",
        length(patterns) - i,
        " patterns should complete in ",
        round(
          mean(patterntimes, na.rm = TRUE)*(length(patterns) - i),
          1
        ),
        " seconds."
      )
    )
  }
  # print(patterntimes)
  # Return corpus to original shape
  if (old_level != at_level) {
    message(paste0("Corpus is returned to level ", old_level))
    text <- quanteda::corpus_reshape(text, to = old_level)
  }
  # If texts instead of a quanteda corpus were provided
  text <- quanteda::texts(text)
  # Return finished corpus
  return(text)
}
