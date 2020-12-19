# ------------------------------------------------------------------------------
#
# Script name: apply_multidict.R
#
# Purpose of script: Apply a dictionary containing complex multi-word patterns
#
# Author: Johann Gründl
# Email: mail@johanngruendl.at
#
# Date created: 2020-06-14
# Date updated: 2020-06-14
#
# ******************************************************************************

# apply_multidict <- function(
#   text,
#   dictionary,
#   glob = FALSE,
#   ignore_case = TRUE
# ) {
#
#   quanteda::dfm_lookup(
#     x = dfm,
#     dictionary = dict,
#     valuetype = valuetype,
#   )
# }
#
# # Replace dictionary terms
# smcorp_et <- readRDS("output/social-media-corpus-sentences.rds")
# # Possible terms
# patterns <- popdictR::gruendl_dictionary_complete %>%
#   # filter(regex_sort <= 2) %>%
#   pull(Word)
#
# # Prepare dictionary terms
# set.seed(20050617)
# all_terms <- tibble(
#   # pattern = popdictR::gruendl_terms,
#   pattern = patterns,
#   replacement = as.character(NA)) %>%
#   mutate(pattern = optimize_regex(pattern)) %>%
#   mutate(pattern = switch_regex_greedy_lazy(pattern))
# all_terms$replacement <-
#   str_c(
#     round(
#       runif(
#         n = nrow(all_terms),
#         min = 10000,
#         max = 99999
#       ),
#       0
#     ),
#     replicate(
#       n = nrow(all_terms),
#       expr = str_c(sample(letters, 10, replace = TRUE), collapse = ""),
#     )
#   )
# saveRDS(all_terms, "output/all-terms-randomized-replacements.rds")
#
# # Replace dictionary hits with replacement strings
# for (i in 1:nrow(all_terms)) {
#   loop_start <- Sys.time()
#   texts(smcorp_et) <- str_replace_all(texts(smcorp_et),
#                                       pattern = regex(pull(all_terms, pattern)[i],
#                                                       ignore_case = TRUE),
#                                       replacement = pull(all_terms,
#                                                          replacement)[i])
#   # print(str_c("Loop ran for pattern ", pull(all_terms, pattern)[i], "."))
#   # print(Sys.time() - loop_start)
# }
# # Return corpus to original shape and save
# saveRDS(smcorp_et,
#         "output/social-media-corpus-sentences-every-term-replaced.rds")
# smcorp_et <- readRDS("output/social-media-corpus-sentences-every-term-replaced.rds")
# all_terms <- readRDS("output/all-terms-randomized-replacements.rds")
# all_terms <- all_terms %>%
#   mutate(replacement = str_c("*", replacement, "*"))
# replaced_dict <- sapply(pull(all_terms, replacement), list)
# names(replaced_dict) <- pull(all_terms, pattern)
# replaced_dict <- dictionary(replaced_dict)
#
# smdfm_et <- dfm(
#   smcorp_et,
#   tolower = TRUE,
#   stem = FALSE,
#   remove_numbers = TRUE,
#   remove_punct = TRUE,
#   remove_symbols = TRUE,
#   remove_separators = FALSE,
#   remove_twitter = TRUE,
#   remove_hyphens = FALSE,
#   remove_url = TRUE,
#   verbose = TRUE
# )
# # Reduce memore need by subsetting
# smdfm_et <- smdfm_et %>%
#   dfm_subset(date >= date("2014-01-01") &
#                date < date("2020-02-29"))
# # Lookup terms
# results <- dfm_lookup(smdfm_et,
#                       replaced_dict,
#                       case_insensitive = TRUE,
#                       valuetype = "glob")
# rm(all_terms, smdfm_et, replaced_dict)
# # Reduce memory need by returning to document level
# results <- ceiling(results/topfeatures(results)[1])
# docvars(results, "tmp_docid") <- docvars(results, "_docid")
# results <- dfm_group(results, "tmp_docid")
# # (results/docvars(results, "doc_sentences"))[sample(1:1000, 10), "\\bwir\\b"]
# # # Prepare for combining with corpus again.
# # results <- results %>%
# #   convert(to = "data.frame") %>%
# #   #select(document) %>%
# #   rename_at(vars(everything()), ~paste0("term_", .))
# # results <- smcorp_et$documents %>%
# #   bind_cols(results)
# # rm(smcorp_et)
# # # Select variables and save
# # results <- results %>%
# #   filter(date >= date("2014-01-01") &
# #            date < date("2020-02-29")) %>%
# #   select(id = `_docid`, date, actor_country, party,
# #          popu_list,
# #          ends_with("_std"),
# #          starts_with("term_"),
# #   ) %>%
# #   group_by(id) %>%
# #   mutate(
# #     date = unique(date),
# #     actor_country = unique(actor_country),
# #     party = unique(party),
# #     sentences = n(),
# #     popu_list = unique(popu_list)) %>%
# #   mutate_at(
# #     .vars = vars(ends_with("_std")),
# #     .funs = unique
# #   ) %>%
# #   mutate_at(
# #     .vars = vars(starts_with("term_")),
# #     .funs = sum
# #   ) %>%
# #   ungroup %>%
# #   distinct
# saveRDS(results, "output/results-every-term.rds")
