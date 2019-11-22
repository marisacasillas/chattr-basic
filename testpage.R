source("tabularize-data.R")
source("transition-detectors.R")

mc.elan.txt <- "test_files/AAS-tabular/VanFJ11-0GS0.txt"
ai.elan.txt <- "test_files/AltELAN-tabular/CT_sample1.txt"

# read data into the spchtbl format
mc.data <- read_spchtbl(filepath = mc.elan.txt,
  tbltype = "aas-elan-txt", cliptier = "code")
ai.data <- read_spchtbl(filepath = ai.elan.txt,
  tbltype = "elan-basic-txt", cliptier = "Coded Segment")

# find all turn transitions within annotated clips
mc.transitions <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "stretch")
#addressee.tags: TCDS, CDS, none
#mode: stretch, strict

mc.ADU.tiers <- unique(mc.data$speaker)[grep(
  "[MFU]A\\d", unique(mc.data$speaker))]
mc.transitions.ADUonly <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = mc.ADU.tiers,
  addressee.tags = "CDS", mode = "stretch")

my.transitions <- fetch_transitions(spchtbl = ai.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "Child Utterances", interactants = "all-speakers",
  addressee.tags = "none", mode = "strict")
