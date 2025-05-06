# coder_initials.R
#
# Add coder initials to check the completion
# T. Eerola, 23/10/2024
#
# As part of the SME evaluation of Episode Model constructs
# Connor Kirts has collected this data
#

#### Missing initials from two experts -----------
# quick fix : E01 = EDU, E02, SJYU
FM$rating$IDCode[FM$rating$ID == "E01"] <- "EDU"
FM$rating$IDCode[FM$rating$ID == "E02"] <- "SJYU"
CB$rating$IDcode.[CB$rating$ID == "E01"] <- "EDU"
CB$rating$IDcode.[CB$rating$ID == "E02"] <- "SJYU"
EDR$rating$IDcode.[EDR$rating$ID == "E01"] <- "EDU"
EDR$rating$IDcode.[EDR$rating$ID == "E02"] <- "SJYU"
AIA$rating$IDcode.[AIA$rating$ID == "E01"] <- "EDU"
AIA$rating$IDcode.[AIA$rating$ID == "E02"] <- "SJYU"
PEP$rating$IDcode.[PEP$rating$ID == "E01"] <- "EDU"
PEP$rating$IDcode.[PEP$rating$ID == "E02"] <- "SJYU"
LMA$rating$IDcode.[LMA$rating$ID == "E01"] <- "EDU"
LMA$rating$IDcode.[LMA$rating$ID == "E02"] <- "SJYU"
MM$rating$IDcode.[MM$rating$ID == "E01"] <- "EDU"
MM$rating$IDcode.[MM$rating$ID == "E02"] <- "SJYU"
QRE$rating$IDcode.[QRE$rating$ID == "E01"] <- "EDU"
QRE$rating$IDcode.[QRE$rating$ID == "E02"] <- "SJYU"

#### some experts changed their initials during the task ------
# CGIU = JIU
# KJYU = KYU

FM$rating$IDCode[FM$rating$IDCode == "KYU"] <- "KJYU"
CB$rating$IDcode.[CB$rating$IDcode. == "KYU"] <- "KJYU"
EDR$rating$IDcode.[EDR$rating$IDcode. == "KYU"] <- "KJYU"
AIA$rating$IDcode.[AIA$rating$IDcode. == "KYU"] <- "KJYU"
PEP$rating$IDcode.[PEP$rating$IDcode. == "KYU"] <- "KJYU"
LMA$rating$IDcode.[LMA$rating$IDcode. == "KYU"] <- "KJYU"
MM$rating$IDcode.[MM$rating$IDcode. == "KYU"] <- "KJYU"
QRE$rating$IDcode.[QRE$rating$IDcode. == "KYU"] <- "KJYU"

FM$rating$IDCode[FM$rating$IDCode == "JIU"] <- "CGIU"
CB$rating$IDcode.[CB$rating$IDcode. == "JIU"] <- "CGIU"
EDR$rating$IDcode.[EDR$rating$IDcode. == "JIU"] <- "CGIU"
AIA$rating$IDcode.[AIA$rating$IDcode. == "JIU"] <- "CGIU"
PEP$rating$IDcode.[PEP$rating$IDcode. == "JIU"] <- "CGIU"
LMA$rating$IDcode.[LMA$rating$IDcode. == "JIU"] <- "CGIU"
MM$rating$IDcode.[MM$rating$IDcode. == "JIU"] <- "CGIU"
QRE$rating$IDcode.[QRE$rating$IDcode. == "JIU"] <- "CGIU"


#### fix double-coding by ILPU by taking the second round only ------

# double!
LMA_without <- LMA$rating[LMA$rating$IDcode. != "ILPU", ] # only 2nd round
tmp <- LMA$rating[LMA$rating$IDcode. == "ILPU" & LMA$rating$ID == "E17", ] # only 2nd round
LMA <- rbind(LMA_without, tmp)
rm(tmp)

# double!
MM_without <- MM$rating[MM$rating$IDcode. != "ILPU", ] # only 2nd round
tmp <- MM$rating[MM$rating$IDcode. == "ILPU" & MM$rating$ID == "E13", ] # only 2nd round
MM <- rbind(MM_without, tmp)
rm(tmp)

# At the end, mask the coder IDs
