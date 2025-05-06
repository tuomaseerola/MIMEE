preprocess_qualtrics <- function(d = NULL) {
  # This is to deal with the qualtrics variable names
  # T. Eerola, 23/10/2024
  #
  # As part of the SME evaluation of Episode Model constructs
  # Connor Kirts has collected this data
  #

  # fix some column names
  names(d)[which(names(d) == 'SL15.')] <- 'SL15_note'
  names(d)[which(names(d) == 'SL17..1')] <- 'SL17_note'
  names(d)[which(names(d) == 'SL19..1')] <- 'SL19_note'

  names(d)[which(names(d) == 'EF19..1')] <- 'EF24.'
  names(d)[which(names(d) == 'EF19_note..1')] <- 'EF24_note.'

  names(d)[which(names(d) == 'ST3_note.')] <- 'ST3.'
  names(d)[which(names(d) == 'ST3_note..1')] <- 'ST3_note.'

  names(d)[which(names(d) == 'IDCode.')] <- 'IDCode'

  d$ID <- paste0('E', sprintf("%02d", 1:nrow(d))) # add unique expert indices
  d$ID <- as.factor(d$ID)

  df <- pivot_longer(d, cols = c(2:dim(d)[2] - 2)) # Keep the last column (ID) as "unpivoted"
  df_notes <- dplyr::filter(df, str_detect(name, '_note|\\.\\.')) # get notes to a separate data frame: _note AND ..
  df <- dplyr::filter(df, !str_detect(name, '_note|\\.\\.'))

  # expert task 2
  df_RANK <- dplyr::filter(df, str_detect(name, 'RANK|\\.\\.')) # finds representative rankings as separate data frame
  df <- dplyr::filter(df, !str_detect(name, 'RANK|\\.\\.'))
  df_Feedback <- dplyr::filter(df, str_detect(name, 'Feedback|\\.\\.')) # finds feedback as a separate data frame
  df <- dplyr::filter(df, !str_detect(name, 'Feedback|\\.\\.'))
  df_GROUP <- dplyr::filter(df, str_detect(name, 'GROUP|\\.\\.')) # finds GROUP and makes separate data frame
  df <- dplyr::filter(df, !str_detect(name, 'GROUP|\\.\\.'))

  # Problem with 1 Experts in 2nd study, cannot figure out how to remove them (no data)

  #d$ID_Code. <- ifelse(df$value == '', NA, df$value)

  # clean up variable names
  df$name <- str_replace_all(df$name, '\\.', '')

  # make empty values explicitly NA (missing)
  df$value <- ifelse(df$value == '', NA, df$value)

  #  print(dim(df))
  return <- list(rating = df, rankings = df_RANK, feedback = df_Feedback)
}
