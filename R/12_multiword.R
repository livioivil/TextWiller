#' Reset temporary columns created by RAKE multi-word processing
#'
#' @param x Token data frame containing potential temporary columns
#' @return The cleaned data frame
#' @export
rake_reset_tokens <- function(x) {
  if (!is.data.frame(x)) {
    stop("Input must be a data frame of tokens")
  }
  
  cleanup <- x
  if ("upos_original" %in% names(cleanup)) {
    cleanup$upos <- cleanup$upos_original
    cleanup$upos_original <- NULL
  }
  if ("lemma_original_nomultiwords" %in% names(cleanup)) {
    cleanup$lemma <- cleanup$lemma_original_nomultiwords
    cleanup$lemma_original_nomultiwords <- NULL
  }
  if ("token_original_nomultiwords" %in% names(cleanup)) {
    cleanup$token <- cleanup$token_original_nomultiwords
    cleanup$token_original_nomultiwords <- NULL
  }
  if ("ngram" %in% names(cleanup)) {
    cleanup$ngram <- NULL
  }
  cleanup
}

#' Extract multi-word candidates using RAKE or collocation scores
#'
#' @param x Token-level data frame annotated with columns `doc_id`, `term_id`,
#'   `token`, `lemma`, and `upos`
#' @param group Column name describing document grouping (default `"doc_id"`)
#' @param ngram_max Maximum ngram length
#' @param ngram_min Minimum ngram length to keep
#' @param relevant Universal POS tags considered relevant
#' @param freq_min Minimum frequency for candidate selection
#' @param term Either `"lemma"` or `"token"` for candidate generation
#' @param type `"automatic"` or `"manual"` (manual expects `keyword_list`)
#' @param keyword_list Optional data frame with predefined keywords for manual mode
#' @param method Multi-word mining strategy (`"rake"`, `"pmi"`, `"md"`, `"lfmd"`)
#' @return A list with `stats` (multi-word candidates) and `dfMW` (token updates)
#' @export
rake_multiword_candidates <- function(x,
                                      group = "doc_id",
                                      ngram_max = 5,
                                      ngram_min = 2,
                                      relevant = c("PROPN", "NOUN", "ADJ", "VERB"),
                                      freq_min = 10,
                                      term = c("lemma", "token"),
                                      type = c("automatic", "manual"),
                                      keyword_list = NULL,
                                      method = c("rake", "pmi", "md", "lfmd")) {
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("Package 'udpipe' is required for rake_multiword_candidates()")
  }
  
  term <- match.arg(term)
  type <- match.arg(type)
  method <- match.arg(method)
  
  data <- x
  if (!all(c(group, term, "upos") %in% names(data))) {
    stop("Input data frame must contain columns: ", paste(c(group, term, "upos"), collapse = ", "))
  }
  
  stats <- switch(
    type,
    automatic = {
      relevant_rows <- data$upos %in% relevant
      switch(
        method,
        rake = {
          cand <- udpipe::keywords_rake(
            x = data,
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq_min,
            relevant = relevant_rows
          )
          cand[cand$ngram >= ngram_min, , drop = FALSE]
        },
        pmi = {
          cand <- udpipe::keywords_collocation(
            x = data[relevant_rows, , drop = FALSE],
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq_min,
            sep = " "
          )
          cand <- cand[, c("keyword", "ngram", "freq", "pmi")]
          cand[cand$ngram >= ngram_min, , drop = FALSE]
        },
        md = {
          cand <- udpipe::keywords_collocation(
            x = data[relevant_rows, , drop = FALSE],
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq_min,
            sep = " "
          )
          cand <- cand[, c("keyword", "ngram", "freq", "md")]
          cand[cand$ngram >= ngram_min, , drop = FALSE]
        },
        lfmd = {
          cand <- udpipe::keywords_collocation(
            x = data[relevant_rows, , drop = FALSE],
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq_min,
            sep = " "
          )
          cand <- cand[, c("keyword", "ngram", "freq", "lfmd")]
          cand[cand$ngram >= ngram_min, , drop = FALSE]
        }
      )
    },
    manual = {
      if (is.null(keyword_list)) {
        stop("Manual mode requires 'keyword_list'")
      }
      manual <- keyword_list
      manual$keyword <- trimws(manual$keyword)
      manual$ngram <- vapply(strsplit(manual$keyword, " "), length, integer(1))
      manual
    }
  )
  
  # Filter tokens to relevant POS tags
  token_subset <- data[data$upos %in% relevant, , drop = FALSE]
  token_subset$lemma <- as.character(token_subset$lemma)
  token_subset$token <- as.character(token_subset$token)
  
  # Recode into multi-words
  if (term == "lemma") {
    token_subset$multiword <- udpipe::txt_recode_ngram(
      token_subset$lemma,
      compound = stats$keyword,
      ngram = stats$ngram,
      sep = " "
    )
  } else {
    token_subset$multiword <- udpipe::txt_recode_ngram(
      token_subset$token,
      compound = stats$keyword,
      ngram = stats$ngram,
      sep = " "
    )
  }
  
  token_subset$upos_multiword <- ifelse(
    (if (term == "lemma") token_subset$lemma else token_subset$token) == token_subset$multiword,
    token_subset$upos,
    "MULTIWORD"
  )
  token_subset$upos_multiword <- ifelse(
    is.na(token_subset$multiword),
    "NGRAM_MERGED",
    token_subset$upos_multiword
  )
  token_subset <- merge(
    token_subset,
    stats[, c("keyword", "ngram"), drop = FALSE],
    by.x = "multiword",
    by.y = "keyword",
    all.x = TRUE
  )
  
  # Frequency table
  stats_freq <- token_subset[token_subset$upos_multiword == "MULTIWORD" &
                               token_subset$multiword %in% stats$keyword, , drop = FALSE]
  if (nrow(stats_freq) > 0) {
    agg <- aggregate(list(freq = stats_freq$multiword), by = list(keyword = stats_freq$multiword), FUN = length)
    stats_freq <- merge(agg, stats, by = "keyword", all.y = TRUE)
    stats_freq <- stats_freq[stats_freq$freq >= freq_min, , drop = FALSE]
    stats_freq <- stats_freq[order(stats_freq$freq, decreasing = TRUE), , drop = FALSE]
  } else {
    stats_freq <- stats
    stats_freq$freq <- 0
    stats_freq <- stats_freq[0, , drop = FALSE]
  }
  
  list(
    stats = stats_freq,
    dfMW = token_subset
  )
}

#' Apply multi-word consolidation to token tables
#'
#' @param x Original token data frame
#' @param rake_results Output of [rake_multiword_candidates()]
#' @param row_sel Optional row indices of the stats table to keep
#' @param term Use `"lemma"` or `"token"` as replacement target
#' @return Updated token data frame with multi-word replacements
#' @export
apply_rake_multiwords <- function(x, rake_results, row_sel = NULL, term = c("lemma", "token")) {
  term <- match.arg(term)
  if (!is.list(rake_results) || !"stats" %in% names(rake_results) || !"dfMW" %in% names(rake_results)) {
    stop("Invalid rake_results object. Use rake_multiword_candidates() first.")
  }
  
  if (!is.null(row_sel)) {
    rake_results$stats <- rake_results$stats[row_sel, , drop = FALSE]
    rake_results$dfMW <- rake_results$dfMW[rake_results$dfMW$multiword %in% rake_results$stats$keyword, , drop = FALSE]
  }
  
  merged <- merge(
    x,
    rake_results$dfMW[, c("doc_id", "term_id", "multiword", "upos_multiword"), drop = FALSE],
    by = c("doc_id", "term_id"),
    all.x = TRUE
  )
  merged$lemma <- as.character(merged$lemma)
  merged$token <- as.character(merged$token)
  
  if (!"lemma_original_nomultiwords" %in% names(merged)) {
    merged$lemma_original_nomultiwords <- NA_character_
  }
  if (!"token_original_nomultiwords" %in% names(merged)) {
    merged$token_original_nomultiwords <- NA_character_
  }
  if (!"upos_original" %in% names(merged)) {
    merged$upos_original <- NA_character_
  }
  if (!"POSSelected" %in% names(merged)) {
    merged$POSSelected <- TRUE
  }
  
  if (term == "lemma") {
    merged$lemma_original_nomultiwords <- ifelse(is.na(merged$lemma_original_nomultiwords), merged$lemma, merged$lemma_original_nomultiwords)
    merged$lemma <- ifelse(is.na(merged$multiword), merged$lemma, merged$multiword)
  } else {
    merged$token_original_nomultiwords <- ifelse(is.na(merged$token_original_nomultiwords), merged$token, merged$token_original_nomultiwords)
    merged$token <- ifelse(is.na(merged$multiword), merged$token, merged$multiword)
  }
  
  merged$upos_original <- ifelse(is.na(merged$upos_original), merged$upos, merged$upos_original)
  merged$upos <- ifelse(is.na(merged$upos_multiword), merged$upos, merged$upos_multiword)
  
  merged$POSSelected <- ifelse(merged$upos == "MULTIWORD", TRUE,
                               ifelse(merged$upos == "NGRAM_MERGED", FALSE, merged$POSSelected))
  
  # Update end offsets for merged tokens if available
  if ("ngram" %in% names(merged) && "end" %in% names(merged)) {
    idx <- which(!is.na(merged$ngram))
    if (length(idx) > 0) {
      end_idx <- idx + (merged$ngram[idx] - 1)
      in_bounds <- end_idx <= nrow(merged)
      merged$end[idx[in_bounds]] <- merged$end[end_idx[in_bounds]]
    }
  }
  
  merged
}
