preprocess_qualtrics_round2 <- function(d = NULL) {
  # This is to deal with the qualtrics variable names

  # fix some column names
  names(d)[which(names(d) == 'IDCode.')] <- 'IDCode'

  d$ID <- paste0('E', sprintf("%02d", 1:nrow(d))) # add unique expert indices
  d$ID <- as.factor(d$ID)

  df <- pivot_longer(d, cols = c(2:dim(d)[2] - 2)) # Keep the last column (ID) as "unpivoted"
  df_notes <- dplyr::filter(df, str_detect(name, '_note|\\.\\.')) # get notes to a separate data frame: _note AND ..
  df <- dplyr::filter(df, !str_detect(name, '_note|\\.\\.'))

  # save ranks
  df_rank <- dplyr::filter(df, str_detect(name, 'RANK'))
  # save feedback
  df_feedback <- dplyr::filter(df, str_detect(name, 'Feedback'))
  # delete these from the relevance
  df <- dplyr::filter(df, !str_detect(name, 'RANK'))
  df <- dplyr::filter(df, !str_detect(name, 'Feedback'))
  df <- dplyr::filter(df, !str_detect(name, 'GROUP')) # should be kept in?

  #table(df$name)

  # Removing empty Expert in 2nd study
  #d$ID_Code. <- ifelse(df$value == '', NA, df$value)

  # clean up variable names
  df$name <- str_replace_all(df$name, '\\.', '')
  df_rank$name <- str_replace_all(df_rank$name, '\\.', '')
  # make empty values explicitly NA (missing)
  df$value <- ifelse(df$value == '', NA, df$value)
  df_rank$value <- ifelse(df_rank$value == '', NA, df_rank$value)

  #  print(dim(df))
  return <- list(rating = df, rankings = df_rank, feedback = df_feedback) #rankings=df_RANK,feedback=df_Feedback)
}
