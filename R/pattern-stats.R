# ------------------------------------------------------------------------------
#
# Script name: pattern-stats.R
#
# Purpose of script: Functions to return stats such as the lookup time and how
# often a pattern occurs.
#
# Author: Johann Gründl
# Email: mail@johanngruendl.at
#
# Date created: 2020-06-12
# Date updated: 2020-06-12
#
# ******************************************************************************


#' Gather statistics for dictionary patterns
#'
#' @description For a list of (possibly multi-word) patterns statistics are
#' generated. Basically the number of matches and the time to run the pattern
#' on the whole corpus are recorded. By default, patterns are applied at the
#' sentence level. Thus, the match_count is the total count of sentences where
#' the pattern was detected.
#'
#' The function works with quanteda corpus objects and regular texts (character
#' vectors; in general text that can be transformed to a quanteda corpus object
#' by quanteda's corpus() function should work). It expects regular expression
#' patterns but can work with glob expressions as well (if parameter glob is
#' set, internally the patterns are transformed to regular expressions).
#'
#' @param text A quanteda corpus object or something that can be transformed to
#' a corpus by quanteda::corpus(), for example, a simple character vector.
#' @param patterns A character vector where each element is a pattern or a
#' quanteda dictionary object. Patterns are expected to be regular expressions
#' (if glob parameter is not set) or only include glob-style wildcars (if glob
#' parameter is set to TRUE).
#' @param at_level At which level should patterns be applied. Possible values
#' are "documents", "sentences", or "paragraphs". Defaults to "sentences".
#' @param glob Do the provided patterns use glob-style wildcards instead of
#' regular expressions? Defaults to FALSE.
#' @param ignore_case Should the case be ignored when searching for pattern
#' matches? Defaults to TRUE.
#' @param optimize_regex Should the regular expressions be optimized to allow
#' for quicker lookups (see regexhelpeR package)? Defaults to TRUE.
#'
#' @return A data frame containing a row for each pattern. It includes the
#' provided pattern (`original_pattern`). The version after pattern
#' transformations (e.g., switch from glob to regex, optimizations) were applied
#' (= the version which was actually tested; `applied_pattern`), the
#' `match_count`, and the `lookup_time`.
#'
#' @export
get_pattern_stats <- function(
  text,
  patterns,
  at_level = "sentences",
  glob = FALSE,
  ignore_case = TRUE,
  optimize_regex = TRUE
) {
  is_quanteda_corpus <- quanteda::is.corpus(text)
  if (!is_quanteda_corpus) {
    message("Quanteda corpus is created")
    text <- quanteda::corpus(text)
  }
  # Prepare patterns
  if (quanteda::is.dictionary(patterns)) {
    patterns <- unlist(patterns)
  }
  patterns_orig <- patterns
  if (glob) {
    message("Provided glob patterns are replaced with regex patterns.")
    patterns <- regexhelpeR::glob_to_regex(patterns)
  }
  if (optimize_regex) {
    message("Patterns are replaced with optimized regex patterns.")
    patterns <- regexhelpeR::optimize_regex_patterns(patterns)
  }
  if (length(patterns) < 1) {
    warning("No pattern was provided.")
    return(text)
  }
  message(paste0('Reshape corpus to ',
                 '"', at_level, '"',
                 ' level'))
  text <- quanteda::corpus_reshape(text, to = at_level)
  # Create empty data frame
  patternstats <- data.frame(original_pattern = patterns_orig,
                             applied_pattern = patterns,
                             match_count = NA_real_,
                             lookup_time = NA_real_,
                             stringsAsFactors = FALSE)
  # Loop through all patterns
  for (i in 1:nrow(patternstats)) {
    pattern <- patternstats[i, "applied_pattern"]
    time <- Sys.time()
    patternstats[i, "match_count"] <-
      sum(
        as.numeric(
          stringr::str_detect(
            quanteda::texts(text),
            stringr::regex(pattern, ignore_case = ignore_case)
          )
        )
      )
    time <- as.numeric(Sys.time() - time, units = "secs")
    patternstats[i, "lookup_time"] <- time
    if (i == 1) {
      print(paste0('First pattern was checked in ',
                   round(time, 1),
                   ' seconds.'))
    }
    if (i %in% ceiling(nrow(patternstats)/100*seq(10, 90, 10))) {
       print(paste0(
         floor(i/nrow(patternstats)*100),
         "% of the patterns have been checked"
       ))
    }
  }
  return(patternstats)
}
