# ------------------------------------------------------------------------------
#
# Script name: run-multidict.R
#
# Purpose of script: Function to run a multi term dictionary.
#
# Author: Johann Gründl
# Email: mail@johanngruendl.at
#
# Date created: 2020-11-20
# Date updated: 2020-11-20
#
# ******************************************************************************


#' Run a dictionary with patterns that might match more than one term (or token)
#'
#' Quanteda's dictionary function does not allow for patterns with wildcards to
#' match more than one token. For example, a regular expression such as
#' "the (.*) people" would not work as expected if you wanted to match
#' expressions such as "the honest people" and "the good people". This
#' function facilitates the usage of dictionaries including such terms. It
#' could be considered the main function for the multidictR package.
#' Internally, the package uses stringr::str_replace_all to replace pattern
#' matches with a random string before then using quanteda to look this string
#' up in the corpus.
#'
#' @param corpus A quanteda corpus object or something that can be transformed
#' to a corpus by quanteda::corpus(), for example, a simple character vector
#' @param dict A character vector where each element is a pattern or a
#' quanteda dictionary object with one dictionary.
#' @param at_level At which level should patterns be applied. Possible values
#' are "documents", "sentences", or "paragraphs". Defaults to "sentences".
#' @param return_value How should the value be returned? Possible values
#' include "count", "binary", "prop", "count_at_level", or "prop_at_level". You
#' get the results from the dictionary at the document level. "count" (the
#' default) gives  the simple frequency of dictionary hits in each document.
#' "count_at_level" gives you the number of sentences or paragraphs in a
#' document where there was at least one pattern match. Together with the
#' include_totals parameter "count" and "count_at_level" give you the most
#' flexibility to work with the results. "binary" returns 0 or 1, depending on
#' whether there was at least one pattern match in the document. "prop" is the
#' proportion of pattern matches relative to the total number of tokens in the
#' document. "prop_at_level" gives you the proportion of sentences or
#' paragraphs (in a document) where a pattern match was found.
#' @param include_totals Should the number sentencens (as "n_sentences") and
#' number of tokens (as "n_tokens") per document also be returned? Defaults to
#' TRUE.
#' @param return_result_only If TRUE, a data.frame containing the results will
#' be returned. If FALSE (the default), you will get the provided corpus with
#' the results attached as new columns.
#' @param pattern_type The type of pattern included in the dictionary. Defaults
#' to "regex" for regular expressions. "glob" is also possible for glob style
#' wildcards. Internally, glob patterns are transformed to regex patterns.
#' Other, usually not needed, possible values include "coll" and "fixed". See
#' the stringr package for details on pattern types.
#' @param case_insensitive Should the case be ignored when searching for
#' pattern matches? Defaults to TRUE.
#' @param regex_optimize Should the regular expressions be optimized by adding
#' word boundaries and removing open wildcards at word boundaries? This is
#' intended for using regular expression dictionary patterns the way I use
#' them in the popdictR package. It then allows for quicker lookups (see
#' regexhelpeR::optimize_regex_patterns)? Defaults to FALSE, so your patterns
#' are not changed.
#' @param regex_make_greedy Should regular expressions be transformed to greedy
#' versions? Defaults to FALSE. Usually not needed. If you switch this to TRUE,
#' while at the same time setting regex_make_lazy to TRUE as well, you will get
#' inverted patterns (i.e., lazy patterns become greedy and greedy patterns
#' become lazy).
#' @param regex_make_lazy Should regular expressions be transformed to lazy
#' versions? Defaults to FALSE, so your patterns are not changed. However, you
#' should probably use lazy regex patterns to replace the shortest possible
#' compounds.
#' @param dict_name You can set a custom name for your dictionary. This is also
#' the name of the variable that contains the results in the return value. If
#' you provided a quanteda dictionary, the name of the first dictionary
#' included will be used. Otherwise, the dict_name defaults to "dict".
#' @param custom_replacement Internally, this function replaces pattern matches
#' with a random string (containing 40 random letters and 10 random numbers)
#' before running quanteda's dictionary lookup function on the corpus. The
#' random string should be unique and there is usually no need to set a custom
#' string.
#' @param tolower Forwarded to quanteda's dfm function, converts all features
#' to lowercase. Defaults to the value for "case_insensitive."
#' @param stem Forwarded to quanteda's dfm function. If TRUE, quanteda stems
#' words. Defaults to FALSE.
#' @param remove Forwarded to quanteda's dfm function. A list of stopwords
#' which are removed from the dfm before running the dictionary.
#' @param ... Additional arguments passed on to quanteda's dfm function (and
#' there to the tokens function). Includes things such as "remove_punct",
#' "remove_symbols", "remove_numbers", etc. See quanteda's tokens function for
#' details.
#'
#' @return Returns the results of running the dictionary. If return_result_only
#' is set, you will get a data.frame with only the results. Otherwise, you the
#' results will be bound to the corpus as new columns. If you only provided
#' texts, the only other column will be these texts of course (variable x). If
#' you provided a quanteda corpus, the results will be stored as variables in
#' the docvars.
#'
#' @export
#' @importFrom rlang .data
run_multidict <- function(
  corpus,
  dict,
  at_level = c("sentences", "paragraphs", "documents"),
  return_value = c(
    "count",
    "binary",
    "prop",
    "count_at_level",
    "prop_at_level"
  ),
  include_totals = TRUE,
  return_result_only = FALSE,
  pattern_type = c("regex", "glob", "coll", "fixed"),
  case_insensitive = TRUE,
  regex_optimize = FALSE,
  regex_make_greedy = FALSE,
  regex_make_lazy = FALSE,
  dict_name,
  custom_replacement,
  tolower = case_insensitive,
  stem = FALSE,
  remove = NULL,
  ...
) {
  if (missing(corpus)) {
    stop("You need to provide a text corpus.")
  }
  if (missing(dict)) {
    stop("You need to provide a dictionary.")
  }
  # Prepare vector arguments
  at_level <- at_level[1]
  return_value <- return_value[1]
  pattern_type <- pattern_type[1]
  if (!(at_level %in% c("sentences", "paragraphs", "documents"))) {
    stop('at_level has to be one of "sentences", "paragraphs", or "documents".')
  }
  if (!(return_value %in% c(
    "prop_at_level",
    "prop",
    "count_at_level",
    "count",
    "binary"
  ))) {
    stop(paste0('return_value has to be one of "prop_at_level", "prop", ',
                '"count_at_level", "count", or "binary".'))
  }
  if (!(pattern_type %in% c("regex", "glob", "coll", "fixed"))) {
    stop('pattern_type has to be one of "regex", "glob", "coll", or "fixed".')
  }
  # Check return_value
  if (at_level == "documents" & return_value == "prop_at_level") {
    warning(paste0('return_value "prop_at_level" does not make sense with ',
                   'analysis at the level of the documents. return_value was ',
                   'changed to "prop" instead.'))
    return_value <- "prop"
  }
  if (at_level == "documents" & return_value == "count_at_level") {
    warning(paste0('return_value "count_at_level" does not make sense with ',
                   'analysis at the level of the documents. return_value was ',
                   'changed to "count" instead.'))
    return_value <- "count"
  }
  # Check if paragraph was used with "at_level" results (not implemented, yet)
  if (return_value == "prop_at_level" & at_level == "paragraphs") {
    stop(paste0('The combination of return_value "', return_value, '" and ',
                'at_level "', at_level, '" has not been implemented yet.'))
  }
  # Quanteda corpus is prepared and original corpus stored if needed
  if (quanteda::is.corpus(corpus)) {
    old_level <- quanteda::meta(corpus, field = "unit", type = "object")
    if ((old_level != at_level) & (old_level != "documents")) {
      warning(paste0("You provided a quanteda corpus object which was not at ",
                     "the documents level and also not at the level you wanted",
                     "to do analysis for. Thus, it is transfered back to ",
                     "documents before running the rest of the operations."))
      corpus <- quanteda::corpus_reshape(corpus, "documents")
      old_level <- "documents"
    }
    if (!return_result_only) {
      corpus_orig <- corpus
      if (old_level != "documents") {
        corpus_orig <- quanteda::corpus_reshape(corpus, "documents")
      } else {
        corpus_orig <- corpus
      }
    }
  } else {
    if (!return_result_only) {
      corpus_orig <- corpus
    }
    old_level <- "documents"
    corpus <- quanteda::corpus(corpus)
  }
  # get the IDs for all documents
  if (old_level != "documents") {
    doc_ids <- quanteda::docid(corpus_orig)
  } else {
    doc_ids <- quanteda::docid(corpus)
  }
  # Prepare replacement
  if (missing(custom_replacement)) {
    set.seed(17062005)
    replacement <- paste0(
      paste0(sample(letters, 20, replace = TRUE), collapse = ""),
      round(stats::runif(1, 1000000000, 9999999999), 0),
      paste0(sample(letters, 20, replace = TRUE), collapse = "")
    )
  } else {
    replacement <- custom_replacement
  }
  if (tolower) {
    replacement <- stringr::str_to_lower(replacement)
  }
  if (missing(dict_name)) {
    dict_name <- "dict"
    if (quanteda::is.dictionary(dict)) {
      dict_name <- names(dict[1])
    }
  }
  # Prepare dictionary
  if (quanteda::is.dictionary(dict)) {
    if (length(dict) > 1) {
      warning(paste0(
        "Only dictionaries of length 1 are supported (i.e., only one ",
        "dictionary in the quanteda dictionary object). Results are only ",
        'based on the first dictionary ("', names(dict[1]), '"). Others are ',
        "dropped."
      ))
      dict <- dict[1]
    }
    dict <- unlist(dict)
  }
  # Prepare dictionary patterns if patterns are of type "regex"
  if ((regex_optimize | regex_make_greedy | regex_make_lazy) &
      (pattern_type == "coll" | pattern_type == "fixed")) {
    message('Regex options are ignored for pattern_type "fixed" or "coll".')
  }
  if (pattern_type == "glob") {
    dict <- regexhelpeR::glob_to_regex(dict)
    pattern_type <- "regex"
    if (regex_optimize) {
      warning(paste0('You should not use the regex_optimize ',
                     'argument with "glob" style patterns. They already ',
                     'come with word boundaries and do not include ',
                     'catch all wildcards at the boundaries. You might get ',
                     'unexpected results.'))
    }
  }
  if (pattern_type == "regex") {
    if (regex_optimize) {
      dict <- regexhelpeR::optimize_regex_patterns(dict)
    }
    if (regex_make_lazy & !regex_make_greedy) {
      dict <- regexhelpeR::make_all_regex_lazy(dict)
    }
    if (!regex_make_lazy & regex_make_greedy) {
      dict <- regexhelpeR::make_all_regex_greedy(dict)
    }
    if (regex_make_lazy & regex_make_greedy) {
      dict <- regexhelpeR::switch_regex_greedy_lazy(dict)
    }
  }
  # Prepare corpus for replacements
  if ((at_level == "sentences" & return_value == "prop_at_level") |
      include_totals ) {
    if (old_level != "documents") {
      tmp_corpus <- quanteda::corpus_reshape(corpus, to = "documents")
      n_sentences <- data.frame(doc_id = quanteda::docid(tmp_corpus),
                                n_sentences = quanteda::nsentence(tmp_corpus))
      if (include_totals) {
        n_tokens <- data.frame(doc_id = quanteda::docid(tmp_corpus),
                               n_tokens = quanteda::ntoken(tmp_corpus, ...))
      }
      rm(tmp_corpus)
    } else {
      n_sentences <- data.frame(doc_id = quanteda::docid(corpus),
                                n_sentences = quanteda::nsentence(corpus))
      n_tokens <- data.frame(doc_id = quanteda::docid(corpus),
                             n_tokens = quanteda::ntoken(corpus, ...))
      # no function for paragraphs in quanteda
    }
  }
  reshaped <- FALSE
  if (old_level != at_level) {
    corpus <- quanteda::corpus_reshape(corpus, to = at_level)
    reshaped <- TRUE
  }
  for (i in dict) {
    if (pattern_type == "regex") {
      quanteda::texts(corpus) <- stringr::str_replace_all(
        quanteda::texts(corpus),
        pattern = stringr::regex(i, ignore_case = case_insensitive),
        replacement = replacement
      )
    }
    if (pattern_type == "coll") {
      quanteda::texts(corpus) <- stringr::str_replace_all(
        quanteda::texts(corpus),
        pattern = stringr::coll(i, ignore_case = case_insensitive),
        replacement = replacement
      )
    }
    if (pattern_type == "fixed") {
      quanteda::texts(corpus) <- stringr::str_replace_all(
        quanteda::texts(corpus),
        pattern = stringr::fixed(i, ignore_case = case_insensitive),
        replacement = replacement
      )
    }
  }
  # Make dictionary from replacement
  lookup_dict <- list(paste0("*", replacement, "*"))
  names(lookup_dict) <- dict_name
  lookup_dict <- quanteda::dictionary(lookup_dict)
  # Prepare dfm
  dfm <- quanteda::dfm(
    corpus,
    tolower = tolower,
    stem = stem,
    remove = remove,
    ... = ...
  )
  # Lookup dictionary and prepare results
  results <- switch(
    return_value,
    "prop_at_level" = {
      # Lookup dictionary
      dfm <- quanteda::dfm_lookup(
        dfm,
        dictionary = lookup_dict,
        valuetype = "glob",
        case_insensitive = case_insensitive,
      )
      # Weight and group for results
      if (at_level != "documents") {
        dfm <- quanteda::dfm_weight(dfm, "boolean")
        dfm <- quanteda::dfm_group(
          dfm,
          groups = quanteda::docid(dfm),
          fill = TRUE,
          force = TRUE
        )
      }
      dfm <- quanteda::convert(dfm, to = "data.frame")
      dfm <- dplyr::left_join(dfm, n_sentences, by = "doc_id")
      dfm[, dict_name] <- dfm[, dict_name]/dfm[, "n_sentences"]
      dfm <- dplyr::select(dfm, -.data$n_sentences)
      dfm
    },
    "prop" = {
      # Group and weight for results
      if (at_level != "documents") {
        dfm <- quanteda::dfm_group(
          dfm,
          groups = quanteda::docid(dfm),
          fill = TRUE,
          force = TRUE
        )
      }
      dfm <- quanteda::dfm_weight(dfm, "prop")
      # Lookup dictionary
      dfm <- quanteda::dfm_lookup(
        dfm,
        dictionary = lookup_dict,
        valuetype = "glob",
        case_insensitive = case_insensitive
      )
      quanteda::convert(dfm, to = "data.frame")
    },
    "count_at_level" = {
      # Lookup dictionary
      dfm <- quanteda::dfm_lookup(
        dfm,
        dictionary = lookup_dict,
        valuetype = "glob",
        case_insensitive = case_insensitive,
      )
      # Weight and group for results
      if (at_level != "documents") {
        dfm <- quanteda::dfm_weight(dfm, "boolean")
        dfm <- quanteda::dfm_group(
          dfm,
          groups = quanteda::docid(dfm),
          fill = TRUE,
          force = TRUE
        )
      }
      quanteda::convert(dfm, to = "data.frame")
    },
    "count" = {
      # Lookup dictionary
      dfm <- quanteda::dfm_lookup(
        dfm,
        dictionary = lookup_dict,
        valuetype = "glob",
        case_insensitive = case_insensitive,
      )
      # Weight and group for results
      if (at_level != "documents") {
        dfm <- quanteda::dfm_group(
          dfm,
          groups = quanteda::docid(dfm),
          fill = TRUE
        )
      }
      quanteda::convert(dfm, to = "data.frame")
    },
    "binary" = {
      # Lookup dictionary
      dfm <- quanteda::dfm_lookup(
        dfm,
        dictionary = lookup_dict,
        valuetype = "glob",
        case_insensitive = case_insensitive,
      )
      # Weight and group for results
      if (at_level != "documents") {
        dfm <- quanteda::dfm_group(
          dfm,
          groups = quanteda::docid(dfm),
          fill = TRUE
        )
      }
      dfm <- quanteda::dfm_weight(dfm, "boolean")
      quanteda::convert(dfm, to = "data.frame")
    }
  )
  # Finish results (also the number of sentences and tokens per doc?)
  results <- dplyr::left_join(
    data.frame(doc_id = doc_ids),
    results,
    by = "doc_id"
  )
  if (include_totals) {
    results <- dplyr::left_join(
      results,
      n_sentences,
      by = "doc_id"
    )
    results <- dplyr::left_join(
      results,
      n_tokens,
      by = "doc_id"
    )
  }
  results[, "doc_id"] <- NULL
  if (!return_result_only) {
    if (quanteda::is.corpus(corpus_orig)) {
      quanteda::docvars(corpus_orig) <- dplyr::bind_cols(
        quanteda::docvars(corpus_orig),
        results,
        .name_repair = "unique"
      )
      results <- corpus_orig
    } else if (is.data.frame(corpus_orig)) {
      results <- dplyr::bind_cols(
        corpus_orig,
        results,
        .name_repair = "unique"
      )
    } else {
      results <- dplyr::bind_cols(
        data.frame(x = corpus_orig),
        results,
        .name_repair = "unique"
      )
    }
  }
  return(results)
}
