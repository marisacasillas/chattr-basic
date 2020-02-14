# TESTS
rm(list = ls())

source("tabularize-data.R")
source("transition-detectors.R")
source("intseq-detectors.R")
source("chattr-helpers.R")

allowed.gap <- 1000
allowed.overlap <- 2000

## test data 1 ----

### read data into the spchtbl format
testdata1.filename <- "test_files/AltELAN-tabular/test-interaction-AllCDS.txt"
testdata1 <- read_spchtbl(filepath = testdata1.filename,
  tbltype = "aas-elan-txt", cliptier = "clip")

### retrieve transitions
#### stretch
testdata1.stretch <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "stretch")
testdata1.stretch$addressee <- as.character(
  testdata1.stretch$addressee)
#### strict
testdata1.strict <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
testdata1.strict$addressee <- as.character(
  testdata1.strict$addressee)
#### qulr
testdata1.qulr <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "qulr")
testdata1.qulr$addressee <- as.character(
  testdata1.qulr$addressee)
#### luqr
testdata1.luqr <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "luqr")
testdata1.luqr$addressee <- as.character(
  testdata1.luqr$addressee)

### check for a match
testdata1.stretch.answers <- read_csv_answercols(
  "testdata1.stretch-correct.csv")
test1.stretch <- all_equal(testdata1.stretch,
  testdata1.stretch.answers, convert = TRUE)
testdata1.strict.answers <- read_csv_answercols(
  "testdata1.strict-correct.csv")
test1.strict <- all_equal(testdata1.strict,
  testdata1.strict.answers, convert = TRUE)
testdata1.qulr.answers <- read_csv_answercols(
  "testdata1.qulr-correct.csv")
test1.qulr <- all_equal(testdata1.qulr,
  testdata1.qulr.answers, convert = TRUE)
testdata1.luqr.answers <- read_csv_answercols(
  "testdata1.luqr-correct.csv")
test1.luqr <- all_equal(testdata1.luqr,
  testdata1.luqr.answers, convert = TRUE)


## test data 2 ----

### read data into the spchtbl format
testdata2.filename <- "test_files/AltELAN-tabular/test-interaction-XDS.txt"
testdata2 <- read_spchtbl(filepath = testdata2.filename,
  tbltype = "aas-elan-txt", cliptier = "clip")

### retrieve transitions
#### stretch
testdata2.stretch <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "stretch")
testdata2.stretch$addressee <- as.character(
  testdata2.stretch$addressee)
#### strict
testdata2.strict <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
testdata2.strict$addressee <- as.character(
  testdata2.strict$addressee)
#### qulr
testdata2.qulr <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "qulr")
testdata2.qulr$addressee <- as.character(
  testdata2.qulr$addressee)
#### luqr
testdata2.luqr <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "luqr")
testdata2.luqr$addressee <- as.character(
  testdata2.luqr$addressee)

### check for a match
testdata2.stretch.answers <- read_csv_answercols(
  "testdata2.stretch-correct.csv")
test2.stretch <- all_equal(testdata2.stretch,
  testdata2.stretch.answers, convert = TRUE)
testdata2.strict.answers <- read_csv_answercols(
  "testdata2.strict-correct.csv")
test2.strict <- all_equal(testdata2.strict,
  testdata2.strict.answers, convert = TRUE)
testdata2.qulr.answers <- read_csv_answercols(
  "testdata2.qulr-correct.csv")
test2.qulr <- all_equal(testdata2.qulr,
  testdata2.qulr.answers, convert = TRUE)
testdata2.luqr.answers <- read_csv_answercols(
  "testdata2.luqr-correct.csv")
test2.luqr <- all_equal(testdata2.luqr,
  testdata2.luqr.answers, convert = TRUE)


## test data 3 ----

### read data into the spchtbl format
testdata3.filename <- "test_files/AltELAN-tabular/test-interaction-noCHI.txt"
testdata3 <- read_spchtbl(filepath = testdata3.filename,
  tbltype = "aas-elan-txt", cliptier = "clip")

### retrieve transitions
#### stretch
testdata3.stretch <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "stretch")
#### strict
testdata3.strict <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
#### qulr
testdata3.qulr <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "qulr")
#### luqr
testdata3.luqr <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "luqr")

### check for a match
test3.stretch <- nrow(testdata3.stretch) == 0
test3.strict <- nrow(testdata3.strict) == 0
test3.qulr <- nrow(testdata3.qulr) == 0
test3.luqr <- nrow(testdata3.luqr) == 0


## test data 4 ----
### read data into the spchtbl format
testdata4.filename <- "test_files/ITS/e20100727_110707_003581.its"
testdata4 <- read_spchtbl(filepath = testdata4.filename,
  tbltype = "lena-its")

### retrieve transitions
testdata4.strict <- fetch_transitions(
  spchtbl = testdata4, allowed.gap, allowed.overlap,
  focus.child = "CHN", interactants = c("FAN", "MAN"),
  addressee.tags = "none", mode = "strict")
testdata4.strict$addressee <- as.character(
  testdata4.strict$addressee)

### check for a match
testdata4.strict.answers <- read_csv_answercols(
  "testdata4.strict-correct.csv")
test4.strict <- all_equal(testdata4.strict,
  testdata4.strict.answers, convert = TRUE)


## test data 5 ----
### read data into the spchtbl format
testdata5.filename <- "test_files/AltELAN-tabular/CT_sample1.txt"
testdata5 <- read_spchtbl(filepath = testdata5.filename,
  tbltype = "elan-basic-txt", cliptier = "Coded Segment")

### retrieve transitions
testdata5.strict <- fetch_transitions(
  spchtbl = testdata5, allowed.gap, allowed.overlap,
  focus.child = "Child Utterances", interactants = ".all-speakers",
  addressee.tags = "none", mode = "strict")
testdata5.strict$addressee <- as.character(
  testdata5.strict$addressee)

### check for a match
testdata5.strict.answers <- read_csv_answercols(
  "testdata5.strict-correct.csv")
test5.strict <- all_equal(testdata5.strict,
  testdata5.strict.answers, convert = TRUE)


### test data 1: focus.child, interactants, addressee.tags ----
#### change focal speaker
testdata1.strict.FC1.focus <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "FC1", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
testdata1.strict.FC1.focus$addressee <- as.character(
  testdata1.strict.FC1.focus$addressee)
testdata1.strict.FC1.focus.answers <- read_csv_answercols(
  "testdata1.strict.FC1.focus-correct.csv")
test1.strict.FC1.focus <- all_equal(testdata1.strict.FC1.focus,
  testdata1.strict.FC1.focus.answers, convert = TRUE)

testdata1.strict.FA1.focus <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "FA1", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
test1.strict.FA1.focus <- nrow(testdata1.strict.FA1.focus) == 0


#### change interactants
testdata1.strict.intFC1only <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = "FC1",
  addressee.tags = "CDS", mode = "strict")
testdata1.strict.intFC1only$addressee <- as.character(
  testdata1.strict.intFC1only$addressee)
testdata1.strict.intFC1only.answers <- read_csv_answercols(
  "testdata1.strict.intFC1only-correct.csv")
test1.strict.intFC1only <- all_equal(testdata1.strict.intFC1only,
  testdata1.strict.intFC1only.answers, convert = TRUE)

testdata1.strict.intFC1FA1 <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = c("FC1", "FA1"),
  addressee.tags = "CDS", mode = "strict")
testdata1.strict.intFC1FA1$addressee <- as.character(
  testdata1.strict.intFC1FA1$addressee)
test1.strict.intFC1FA1 <- all_equal(testdata1.strict.intFC1FA1,
  testdata1.strict.intFC1only.answers, convert = TRUE)

testdata1.strict.intFA1 <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = "FA1",
  addressee.tags = "CDS", mode = "strict")
test1.strict.intFA1 <- nrow(testdata1.strict.intFA1) == 0

#### change addressee.tags
testdata1.strict.tcds <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "TCDS", mode = "strict")
test1.strict.tcds <- nrow(testdata1.strict.tcds) == 0

testdata1.strict.none <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "none", mode = "strict")
testdata1.strict.none$addressee <- as.character(
  testdata1.strict.none$addressee)
test1.strict.none <- all_equal(testdata1.strict.none,
  testdata1.strict.answers, convert = TRUE)


### tests to write ---- 
# FETCH_INTSEQS

# OLD sandbox tests ----

mc.elan.txt <- "test_files/AAS-tabular/VanFJ11-0GS0.txt"
ai.elan.txt <- "test_files/AltELAN-tabular/CT_sample1.txt"
yd.lena.txt <- "test_files/ITS/e20100727_110707_003581.its"

# read data into the spchtbl format
mc.data <- read_spchtbl(filepath = mc.elan.txt,
  tbltype = "aas-elan-txt", cliptier = "code")
ai.data <- read_spchtbl(filepath = ai.elan.txt,
  tbltype = "elan-basic-txt", cliptier = "Coded Segment")
yd.data <- read_spchtbl(filepath = yd.lena.txt,
  tbltype = "lena-its")

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

# mc.transitions.strict <- fetch_transitions(spchtbl = mc.data,
#   allowed.gap = 1000, allowed.overlap = 2000,
#   focus.child = "CHI", interactants = ".all-speakers",
#   addressee.tags = "CDS", mode = "strict")
# mc.transitions.luqr <- fetch_transitions(spchtbl = mc.data,
#   allowed.gap = 1000, allowed.overlap = 2000,
#   focus.child = "CHI", interactants = ".all-speakers",
#   addressee.tags = "CDS", mode = "luqr")
# mc.transitions.qulr <- fetch_transitions(spchtbl = mc.data,
#   allowed.gap = 1000, allowed.overlap = 2000,
#   focus.child = "CHI", interactants = ".all-speakers",
#   addressee.tags = "CDS", mode = "qulr")
# 
# mc.ADU.tiers <- unique(mc.data$speaker)[grep(
#   "[MFU]A\\d", unique(mc.data$speaker))]
# mc.transitions.ADUonly <- fetch_transitions(spchtbl = mc.data,
#   allowed.gap = 1000, allowed.overlap = 2000,
#   focus.child = "CHI", interactants = mc.ADU.tiers,
#   addressee.tags = "CDS", mode = "stretch")
# mc.transitions.FA1 <- fetch_transitions(spchtbl = mc.data,
#   allowed.gap = 1000, allowed.overlap = 2000,
#   focus.child = "CHI", interactants = "FA1",
#   addressee.tags = "CDS", mode = "stretch")
# mc.transitions.FA2 <- fetch_transitions(spchtbl = mc.data,
#   allowed.gap = 1000, allowed.overlap = 2000,
#   focus.child = "CHI", interactants = "FA2",
#   addressee.tags = "CDS", mode = "stretch")

ai.transitions <- fetch_transitions(spchtbl = ai.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "Child Utterances", interactants = ".all-speakers",
  addressee.tags = "none", mode = "strict")

# use the resulting turn transition table to compute
# interactional sequences
ai.intseqs.stretch <- fetch_intseqs(tttbl = ai.transitions)

### LENA ###
FAN.MAN <- c("FAN", "MAN")
yd.transitions <- fetch_transitions(spchtbl = yd.data,
  allowed.gap = 1000, allowed.overlap = 2000,
  focus.child = "CHN", interactants = FAN.MAN,
  addressee.tags = "none", mode = "strict")

# use the resulting turn transition table to compute
# interactional sequences
yd.intseqs.stretch <- fetch_intseqs(tttbl = yd.transitions)
