source("tabularize-data.R")
source("transition-detectors.R")

my.elan.txt <- "test_files/AAS-tabular/VanFJ11-0GS0.txt"

data <- read_spchtbl(filepath = my.elan.txt, tbltype = "aas-elan-txt")
