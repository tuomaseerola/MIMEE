analyse_episode_data_round2 <- function(data = NULL) {
  # describe what the analysis does
  # Expert give ratings on a scale of 1-4
  # Content Validity (CV), CVR (ratio)

  # Content validity ratio (CVR) from https://www.sciencedirect.com/science/article/pii/S2405603023000109
  # CVR = (Ne - N/2)/(N/2)
  # https://rdrr.io/cran/psychometric/man/CVratio.html
  # ITEM-LEVEL
  dfw <- pivot_wider(data, names_from = 'name', values_from = 'value')
  dfw <- dplyr::select(dfw, -ID) # remove the ids before the loop

  U <- names(dfw)
  CVR <- NULL
  CVI <- NULL
  for (k in 1:length(U)) {
    item <- dplyr::select(dfw, U[k])
    item <- item[complete.cases(item), ] # remove NAs from the data (experts and ratings)
    N_experts <- nrow(item)
    N_high <- sum(
      item[1] == 'Extremely Relevant' | item[1] == 'Moderately Relevant'
    )
    CVR <- rbind(CVR, psychometric::CVratio(N_experts, N_high)) # I think CVratio here is for 3 levels rather than 4 like we did

    #  Content validity index (CVI) per item index
    # number of raters giving 3 or 4 / total number of raters
    CVI <- rbind(
      CVI,
      (sum(item[1] == 'Extremely Relevant') +
        sum(item[1] == 'Moderately Relevant')) /
        N_experts
    )
  }

  rownames(CVR) <- U
  colnames(CVR) <- 'Value'
  rownames(CVI) <- U
  colnames(CVI) <- 'Value'
  return <- list(CVR, CVI)
}
