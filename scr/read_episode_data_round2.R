read_episode_data_round2 <- function(filename = NULL) {
  d <- read.csv(filename) # make sure this works
  d <- d[3:nrow(d), ] # delete two first rows (metadata)

  # more operations, delete unwanted data such as IP addresses...
  d <- dplyr::select(d, -c(1:(which(names(d) == 'Consent.')))) # Removes all col. to First item
  d <- dplyr::select(d, -c(which(names(d) == 'Nationality.'))) # tried to remove just nationality to save idcodes
  #d <- dplyr::select(d, -c(((which(tolower(names(d))=='idcode.')-1):ncol(d)) ))
  d <- dplyr::select(d, -c(((which(tolower(names(d)) == 'idcode.') - 1))))
  d <- dplyr::select(d, -c(((which(tolower(names(d)) == 'idcode.') + 1))))
  #print(dim(d))
  return <- data.frame(d)
}

#d <- d[, !names(d) %in% c("Nationality", "End Eval - FM")]
#d <- d %>% select(- c("End Eval - FM"))
