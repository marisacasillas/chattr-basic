source("tabularize-data.R")
source("transition-detectors.R")

my.elan.txt <- "test_files/AAS-tabular/VanFJ11-0GS0.txt"
ai.elan.txt <- "test_files/AltELAN-tabular/CT_sample1.txt"

my.data <- read_spchtbl(filepath = my.elan.txt,
  tbltype = "aas-elan-txt", cliptier = "code")
ai.data <- read_spchtbl(filepath = ai.elan.txt,
  tbltype = "elan-basic-txt", cliptier = "Coded Segment")

