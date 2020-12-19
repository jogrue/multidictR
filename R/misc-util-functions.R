# ------------------------------------------------------------------------------
#
# Script name: misc-util-functions.R
#
# Purpose of script: Includes some functions to work with dictionaries.
#
# Author: Johann Gründl
# Email: mail@johanngruendl.at
#
# Date created: 2020-06-09
# Date updated: 2020-06-09
#
# ******************************************************************************


#' Make every term a dictionary
#'
#' @description This functions transforms a dictionary (or a list of terms) to
#' a quanteda dictionary object where each term is it's own dictionary.
#'
#' @param dict A quanteda dictionary or a simple list of dictionary terms
#'
#' @return A quanteda dictionary object where each term is its own dictionary.
#' Dictionary names are simply the provided terms.
#'
#' @export
#'
every_term_a_dict <- function(dict) {
  dict <- unlist(dict)
  names(dict) <- dict
  dict <- sapply(dict, list)
  return(quanteda::dictionary(dict))
}


#' Stepwise locate keywords-in-context
#'
#' @description With large amounts of text and complex (regex) patterns you
#' might run into problems with memory when running quanteda's kwic function.
#' Thus this function takes a stepwise approach. The tokens are split into
#' certain chunks (specified by step) and then kwic is run on these smaller
#' chunks.
#'
#' @param tokens A quanteda tokens object.
#' @param pattern A character vector, list of character vectors, dictionary, or
#' collocations object.
#' @param window The number of context words to be displayed around the keyword.
#' @param valuetype The type of pattern matching: "glob" for "glob"-style
#' wildcard expressions; "regex" for regular expressions; or "fixed" for exact
#' matching. Defaults to "regex".
#' @param step How many tokens should be processed at once? Defaults to 10000.
#' @param ... Additonal arguments passed to the kwic function.
#'
#' @return A kwic classed data.frame.
#'
#' @export
run_kwic_stepwise <- function(
  tokens,
  pattern,
  window = 25,
  valuetype = "regex",
  step = 10000,
  ...
) {

  if (!quanteda::is.tokens(tokens)) {
    stop(paste0("Provided tokens object does not appear to be a quanteda",
                " tokens object."))
  }

  # kwic are created in a loop
  max <- length(tokens)
  # step <- 10000
  first <- 1 - step
  kwic <- NULL
  repeat {
    first <- first + step
    last <- first + step - 1
    if (last > max) {
      last <- max
    }
    tmp <- tokens[first:last]
    print(paste0("Creating kwic for elements ", first, " to ", last, "."))
    tmp <- quanteda::kwic(tmp,
                          pattern = pattern,
                          window = window,
                          valuetype = valuetype,
                          ...)
    if (is.null(kwic)) {
      kwic <- tmp
    } else {
      kwic <- dplyr::bind_rows(kwic, tmp)
    }
    if (last == max) {
      break
    }
  }

  return(kwic)
}


#' Get the amount possible pattern combinations
#'
#' @description Wildcards in patterns can potentially lead to a huge number
#' of possible combinations in tokens. This in turn leads to problems with
#' memory usage, in some quanteda functions. With this function you can check
#' which patterns in a dictionary are the most problematic (= produce the
#' highest number of possible combinations). This was suggested by Kohei
#' Watanabe here:
#' <https://github.com/quanteda/quanteda/issues/1539#issuecomment-451588580>
#'
#' @param text A quanteda corpus object, text that can be transformed to a
#' corpus object, or a quanteda tokens object
#' @param pattern A quanteda pattern, i.e., a character vector, list of
#' character vectors, dictionary, or collocations object.
#' @param glob Are patterns glob-style? Defaults to FALSE (for regex patterns).
#' @param ... Additional parameters passed to quanteda:::pattern2list.
#'
#' @return A table containing the patterns and how many possible combinations
#' they produce for a given text or tokens object. On top are the patterns that
#' produce the most combinations and, thus, need the most memory.
#'
#' @export
check_pattern_performance <- function(text, pattern, glob = FALSE, ...) {
  if (quanteda::is.corpus(text)) {
    text <- quanteda::tokens(text)
  }
  if (!quanteda::is.tokens(text)) {
    text <- quanteda::tokens(quanteda::corpus(text))
  }
  if (!glob) {
    valuetype = "regex"
  } else {
    valuetype = "glob"
  }
  return(
    sort(
      table(
        names(
          quanteda:::pattern2list(
            x = pattern,
            types = quanteda::types(text),
            valuetype = valuetype,
            case_insensitive = TRUE,
            ...
          )
        )
      ),
      decreasing = TRUE
    )
  )
}


#' Stepwise generate compounds from tokens
#'
#' @description With large amounts of text and complex (regex) patterns you
#' might run into problems with memory when running quanteda's tokens_compound
#' function. Thus this function takes a stepwise approach. The tokens are split
#' into certain chunks (specified by step) and then tokens_compound is run on
#' these smaller chunks. See also
#' <https://github.com/quanteda/quanteda/issues/1539> and the
#' check_pattern_performance function.
#'
#' @param tokens A quanteda tokens object.
#' @param pattern A character vector, list of character vectors, dictionary, or
#' collocations object.
#' @param concatenator The concatenation character that will connect the words
#' making up the multi-word sequences. Defaults to "_".
#' @param valuetype The type of pattern matching: "glob" for "glob"-style
#' wildcard expressions; "regex" for regular expressions; or "fixed" for exact
#' matching. Defaults to "regex".
#' @param step How many tokens should be processed at once? Defaults to 10000.
#' @param ... Additonal arguments passed to the tokens_compound function.
#'
#' @return A tokens object in which the token sequences matching pattern have
#' been replaced by new compounded "tokens" joined by the concatenator.
#'
#' @export
run_tokens_compound_stewpise <- function(
  tokens,
  pattern,
  step = 10000,
  concatenator = "_",
  valuetype = "regex",
  ...
) {

  # Compound tokens are created in a loop
  max <- length(tokens)
  first <- 1 - step
  comp_toks <- NULL
  repeat {
    first <- first + step
    last <- first + step - 1
    if (last > max) {
      last <- max
    }
    tmp <- tokens[first:last]
    print(paste0("Creating compounds for elements ", first, " to ", last, "."))
    tmp <- quanteda::tokens_compound(
      x = tmp,
      pattern = pattern,
      concatenator = concatenator,
      valuetype = valuetype,
      ...
    )
    if (is.null(comp_toks)) {
      comp_toks <- tmp
    } else {
      comp_toks <- c(comp_toks, tmp)
    }
    if (last == max) {
      break
    }
  }
  return(comp_toks)
}
