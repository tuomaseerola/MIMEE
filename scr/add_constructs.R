add_constructs <- function(data = NULL, construct = NULL, verbose = FALSE) {
  # Add sub-constructs to the constructs
  # T. Eerola, 23/10/2024
  #
  # As part of the SME evaluation of Episode Model constructs
  # Connor Kirts has collected this data
  #

  if (is.null(data)) {
    stop("No data provided")
  }
  if (is.null(construct)) {
    stop("No construct provided")
  }
  #EDR
  if (construct == 'EDR') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'EJ')] <- 'Enjoyment'
    data$construct[str_detect(data$name, 'DT')] <- 'Distraction'
    data$construct[str_detect(data$name, 'RX')] <- 'Relaxation'
  }

  #CB
  if (construct == 'CB') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'GC')] <- 'Connection'
    data$construct[str_detect(data$name, 'SL')] <- 'Belonging'
  }

  #FM
  if (construct == 'FM') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'EM')] <- 'Energy Control'
    data$construct[str_detect(data$name, 'MF')] <- 'Focus'
  }

  #PEP
  if (construct == 'PEP') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'RC')] <- 'Coping'
    data$construct[str_detect(data$name, 'EF')] <- 'Feeling'
  }

  #AIA
  if (construct == 'AIA') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'MS')] <- 'Moved'
    data$construct[str_detect(data$name, 'CU')] <- 'Curiosity'
    data$construct[str_detect(data$name, 'AE')] <- 'Aesthetics'
  }

  #LMA
  if (construct == 'LMA') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'DF')] <- 'Diffuse'
    data$construct[str_detect(data$name, 'BA')] <- 'Bodily'
    data$construct[str_detect(data$name, 'EA')] <- 'Emotional'
    data$construct[str_detect(data$name, 'AA')] <- 'Associative'
    data$construct[str_detect(data$name, 'SA')] <- 'Structural'
    data$construct[str_detect(data$name, 'RC')] <- 'Reduced'
  }

  #MM
  if (construct == 'MM') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'ST')] <- 'Structure'
    data$construct[str_detect(data$name, 'SE')] <- 'Self'
    data$construct[str_detect(data$name, 'SO')] <- 'Source'
  }

  #QRE
  if (construct == 'QRE') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'QU')] <- 'Qualia'
    data$construct[str_detect(data$name, 'PO')] <- 'Preference'
    data$construct[str_detect(data$name, 'FA')] <- 'Familiar'
  }

  #FC2
  if (construct == 'FC2') {
    data$construct <- NA
    #data$construct[str_detect(data$name,'EDR')]<-'EDR'
    #data$construct[str_detect(data$name,'AIA')]<-'AIA'
    #data$construct[str_detect(data$name,'PEP')]<-'PEP'
    #data$construct[str_detect(data$name,'FM')]<-'FM'
    #data$construct[str_detect(data$name,'CB')]<-'CB'

    data$construct[str_detect(
      data$name,
      'FM_Rele_1$|FM_Rele_2$|FM_Rele_3$|FM_Rele_4$|FM_Rele_5$|FM_Rele_6$|FM_Rele_7$|FM_Rele_8$|FM_Rele_9$|FM_Rele_10$|'
    )] <- 'Energy Control'
    data$construct[str_detect(
      data$name,
      'FM_Rele_11$|FM_Rele_12$|FM_Rele_13$|FM_Rele_14$|FM_Rele_15$|FM_Rele_16$|FM_Rele_17$|FM_Rele_18$|FM_Rele_19$|FM_Rele_20$|FM_Rele_21$|FM_Rele_22$|FM_Rele_23$|FM_Rele_24$|FM_Rele_25$|'
    )] <- 'Focus'

    data$construct[str_detect(
      data$name,
      'CB_Rele_1$|CB_Rele_2$|CB_Rele_3$|CB_Rele_4$|CB_Rele_5$|CB_Rele_6$|CB_Rele_7$|CB_Rele_8$|CB_Rele_9$|CB_Rele_10$|CB_Rele_11$|CB_Rele_12$|CB_Rele_13$|CB_Rele_14$|CB_Rele_15$|CB_Rele_16$|CB_Rele_17$|CB_Rele_18$|'
    )] <- 'Connection'
    data$construct[str_detect(
      data$name,
      'CB_Rele_19$|CB_Rele_20$|CB_Rele_21$|CB_Rele_22$|CB_Rele_23$|CB_Rele_24$|CB_Rele_25$|CB_Rele_26$|CB_Rele_27$|CB_Rele_28$|CB_Rele_29$|CB_Rele_30$|CB_Rele_31$|CB_Rele_32$|'
    )] <- 'Belonging'

    data$construct[str_detect(
      data$name,
      'AIA_Rele_1$|AIA_Rele_2$|AIA_Rele_3$|AIA_Rele_4$|AIA_Rele_5$|AIA_Rele_6$|AIA_Rele_7$|AIA_Rele_8$|AIA_Rele_9$|AIA_Rele_10$|AIA_Rele_11$|'
    )] <- 'Moved'
    data$construct[str_detect(
      data$name,
      'AIA_Rele_12$|AIA_Rele_13$|AIA_Rele_14$|AIA_Rele_15$|AIA_Rele_16$|AIA_Rele_17$|AIA_Rele_18$|AIA_Rele_19$|AIA_Rele_20$|AIA_Rele_21$|AIA_Rele_22$|AIA_Rele_23$|AIA_Rele_24$|'
    )] <- 'Curiosity'
    data$construct[str_detect(
      data$name,
      'AIA_Rele_25$|AIA_Rele_26$|AIA_Rele_27$|AIA_Rele_28$|AIA_Rele_29$|AIA_Rele_30$|AIA_Rele_31$|AIA_Rele_32$|AIA_Rele_33$|AIA_Rele_34$|'
    )] <- 'Aesthetics'

    data$construct[str_detect(
      data$name,
      'PEP_Rele_1$|PEP_Rele_2$|PEP_Rele_3$|PEP_Rele_4$|PEP_Rele_5$|PEP_Rele_6$|PEP_Rele_7$|PEP_Rele_8$|PEP_Rele_9$|PEP_Rele_10$|PEP_Rele_11$|'
    )] <- 'Coping'
    data$construct[str_detect(
      data$name,
      'PEP_Rele_12$|PEP_Rele_13$|PEP_Rele_14$|PEP_Rele_15$|PEP_Rele_16$|PEP_Rele_17$|PEP_Rele_18$|PEP_Rele_19$|PEP_Rele_20$|PEP_Rele_21$|PEP_Rele_22$|PEP_Rele_23$|PEP_Rele_24$|PEP_Rele_25$|'
    )] <- 'Feeling'

    data$construct[str_detect(
      data$name,
      'EDR_Rele_1$|EDR_Rele_2$|EDR_Rele_3$|EDR_Rele_4$|EDR_Rele_5$|EDR_Rele_6$|EDR_Rele_7$|EDR_Rele_8$|EDR_Rele_9$|EDR_Rele_10$|EDR_Rele_11$|EDR_Rele_12$|EDR_Rele_13$|EDR_Rele_14$|EDR_Rele_15$|'
    )] <- 'Relaxation'
    data$construct[str_detect(
      data$name,
      'EDR_Rele_16$|EDR_Rele_17$|EDR_Rele_18$|EDR_Rele_19$|EDR_Rele_20$|EDR_Rele_21$|EDR_Rele_22$|EDR_Rele_23$|EDR_Rele_24$|EDR_Rele_25$|EDR_Rele_26$|'
    )] <- 'Enjoyment'
    data$construct[str_detect(
      data$name,
      'EDR_Rele_27$|EDR_Rele_28$|EDR_Rele_29$|EDR_Rele_30$|EDR_Rele_31$|EDR_Rele_32$|EDR_Rele_33$|EDR_Rele_34$|EDR_Rele_35$|'
    )] <- 'Distraction'
  }

  #LMA2
  if (construct == 'LMA2') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'Dif')] <- 'Diffuse'
    data$construct[str_detect(data$name, 'Bod')] <- 'Bodily'
    data$construct[str_detect(data$name, 'Emo')] <- 'Emotional'
    data$construct[str_detect(data$name, 'Ass')] <- 'Associative'
    data$construct[str_detect(data$name, 'StrA')] <- 'Structural'
    data$construct[str_detect(data$name, 'RCA')] <- 'Reduced'
  }

  #MM2
  if (construct == 'MM2') {
    data$construct <- NA
    data$construct[str_detect(data$name, 'StrM')] <- 'Structure'
    data$construct[str_detect(data$name, 'Sel')] <- 'Self'
    data$construct[str_detect(data$name, 'Sou')] <- 'Source'
    data$construct[str_detect(data$name, 'Qua')] <- 'Qualia'
    data$construct[str_detect(data$name, 'Exp')] <- 'Preference & Familiarity'
  }

  # report completion
  if (verbose == TRUE) {
    print(paste('Added constructs to', construct))
    print('Number of observations in constructs:')
    print(table(data$construct, useNA = "ifany"))
    print('Number of observers:')
    print(length(unique(data$ID)))
    print('Each with observations:')
    print(table(data$ID, useNA = "ifany"))
  }
  #  print("rename IDCode column")
  names(data)[tolower(names(data)) == "idcode."] <- "IDCode"

  return <- data
}
