source("tabularize-data.R")
source("transition-detectors.R")
source("intseq-detectors.R")
source("chattr-helpers.R")

mc.elan.txt <- "test_files/AAS-tabular/VanFJ11-0GS0.txt"
ai.elan.txt <- "test_files/AltELAN-tabular/CT_sample1.txt"

# read data into the spchtbl format
mc.data <- read_spchtbl(filepath = mc.elan.txt,
  tbltype = "aas-elan-txt", cliptier = "code")
ai.data <- read_spchtbl(filepath = ai.elan.txt,
  tbltype = "elan-basic-txt", cliptier = "Coded Segment")

# find all turn transitions within annotated clips
mc.transitions.stretch <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "stretch")
#default values (as above)
#addressee.tags: TCDS, CDS, none
#mode: stretch, strict, luqr, qulr

# use the resulting turn transition table to compute
# interactional sequences
ms.intseqs.stretch <- fetch_intseqs(tttbl = mc.transitions.stretch)


mc.transitions.strict <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
mc.transitions.luqr <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "luqr")
mc.transitions.qulr <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "qulr")

mc.ADU.tiers <- unique(mc.data$speaker)[grep(
  "[MFU]A\\d", unique(mc.data$speaker))]
mc.transitions.ADUonly <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = mc.ADU.tiers,
  addressee.tags = "CDS", mode = "stretch")
mc.transitions.FA1 <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = "FA1",
  addressee.tags = "CDS", mode = "stretch")
mc.transitions.FA2 <- fetch_transitions(spchtbl = mc.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHI", interactants = "FA2",
  addressee.tags = "CDS", mode = "stretch")

ai.transitions <- fetch_transitions(spchtbl = ai.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "Child Utterances", interactants = ".all-speakers",
  addressee.tags = "none", mode = "strict")
