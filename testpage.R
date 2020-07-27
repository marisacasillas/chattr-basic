# TESTS
rm(list = ls())

source("tabularize-data.R")
source("transition-detectors.R")
source("chattr-helpers.R")
source("tabularize-data.R")
source("transition-detectors.R")
source("continuation-detectors.R")
source("intseq-detectors.R")
source("estimate-baseline.R")

ttdata.spchtbl.rttm <- read_spchtbl(
  "test_files/rttm/TEST.rttm",
  "rttm")

ttdata.spchtbl.its <- read_spchtbl(
  "../chattr-paper/annotated-data/raw/123522-1904.its",
  "lena-its", lxonly = TRUE)

its.tttbl <- ttdata.spchtbl.its %>%
  fetch_transitions(allowed.gap, allowed.overlap, min.utt.dur,
                    "CH", c("FA", "MA"), "none", "strict")
its.intseqtbl <- its.tttbl %>%
  fetch_intseqs(allowed.gap)

tbl <- ttdata.spchtbl.its
n.runs <- 3
allowed.gap <- 2000
allowed.overlap <- 1000
min.utt.dur <- 300
focus.child <- "FA"
interactants <- c("FA", "MA")
addressee.tags <- "none"
mode <- "strict"
output <- "tttbl" # intseq tttbl

checkme.tttbl.baseline <- fetch_baseline(tbl, n.runs,
                                         allowed.gap, allowed.overlap,
                                         min.utt.dur, focus.child, interactants,
                                         addressee.tags, mode, output)

output <- "intseq" # intseq tttbl
checkme.intseqtbl.baseline <- fetch_baseline(tbl, n.runs,
                                         allowed.gap, allowed.overlap,
                                         min.utt.dur, focus.child, interactants,
                                         addressee.tags, mode, output)

# %>%
#   fetch_transitions(allowed.gap, allowed.overlap, min.utt.dur,
#                     "CH", LENA.interactants, "none", "strict")

ttdata.spchtbl.aas <- read_spchtbl(
  "test_files/AAS-tabular/test-interaction-XDS-lxonly.txt",
  "aas-elan-txt", lxonly = TRUE, cliptier = "clip")

ttdata.spchtbl.ebt <- read_spchtbl(
  "test_files/AltELAN-tabular/CT_sample1-lxvocs.txt",
  "elan-basic-txt", lxonly = "l", cliptier = "Coded Segment")


##########
allowed.gap <- 2000
allowed.overlap <- 1000
min.utt.dur <- 300
LENA.interactants <- c("FAN", "MAN")

ttdata.spchtbl <- read_spchtbl(
  "../chattr-paper/annotated-data/raw/123522-1904.its",
  "lena-its", lxonly = TRUE)

ttdata.turnsonly <- ttdata.spchtbl %>%
  fetch_transitions(allowed.gap, allowed.overlap, min.utt.dur,
    "CHN", LENA.interactants, "none", "strict")

ttdata.intseqs <- ttdata.spchtbl %>%
  fetch_transitions(allowed.gap, allowed.overlap, min.utt.dur,
    "CHN", LENA.interactants, "none", "strict") %>%
  fetch_intseqs(allowed.gap)

# check that each intseq has at least one prompt or response
check.tts.in.intseqs <- ttdata.intseqs %>%
  group_by(intseq.num) %>%
  summarize(
    n.prompts = length(which(!is.na(prompt.spkr))),
    n.responses = length(which(!is.na(response.spkr)))
  ) %>%
  mutate(
    n.tts = n.prompts + n.responses
  ) %>%
  filter(
    n.tts == 0 & !is.na(intseq.num)
  ) %>%
  nrow() == 0

# check that each vocseq has exactly 0 prompts and 0 responses
check.NO.tts.in.vocseqs <- ttdata.intseqs %>%
  group_by(vocseq.num) %>%
  summarize(
    n.prompts = length(which(!is.na(prompt.spkr))),
    n.responses = length(which(!is.na(response.spkr)))
  ) %>%
  mutate(
    n.tts = n.prompts + n.responses
  ) %>%
  filter(
    n.tts > 0 & !is.na(vocseq.num)
  ) %>%
  nrow() == 0

# ^^ these tests need to be integrated
##########

allowed.gap <- 2000
allowed.overlap <- 1000
LENA.interactants <- c("FAN", "MAN")
min.utt.dur <- 0

ttdata.spchtbl <- read_spchtbl("test_files/ITS/e20191024_000253_008232.its",
  "lena-its", lxonly = TRUE)

ttdata.turnsonly <- ttdata.spchtbl %>%
  fetch_transitions(allowed.gap, allowed.overlap, min.utt.dur,
    "CHN", LENA.interactants, "none", "strict")

ttdata.intseqs <- ttdata.spchtbl %>%
  fetch_transitions(allowed.gap, allowed.overlap, min.utt.dur,
    "CHN", LENA.interactants, "none", "strict") %>%
  fetch_intseqs(allowed.gap)

interactional.bursts <- ttdata.intseqs %>%
  filter(!is.na(intseq.num)) %>%
  mutate(
    intseq.dur.ms = intseq.stop.ms - intseq.start.ms,
    intseq.dur.min = intseq.dur.ms/60000,
    intseq.start.hr = intseq.start.ms/3600000) %>%
  group_by(
    intseq.num, intseq.start.ms, intseq.stop.ms,
    intseq.dur.ms, intseq.dur.min, intseq.start.hr,
    intseq.start.spkr, intseq.stop.spkr) %>%
  summarize(
    n.intseq.prompts = sum(!is.na(prompt.start.ms)),
    n.intseq.responses = sum(!is.na(response.start.ms)),
    n.intseq.tts = n.intseq.prompts + n.intseq.responses
  ) # add test: n.intseq.tts should be > 0 in all cases if no bugs

interactional.bursts.lena <- interactional.bursts %>%
  arrange(intseq.num)
between.intseq.times <- tibble()
intseq.data <- interactional.bursts.lena
intseq.data$prev.intseq.stop <- c(0, intseq.data$intseq.stop.ms[1:(nrow(intseq.data)-1)])
intseq.data$time.since.prev.intseq.ms <- intseq.data$intseq.start.ms - intseq.data$prev.intseq.stop
intseq.data$time.since.prev.intseq.min <- intseq.data$time.since.prev.intseq.ms/60000
between.intseq.times.zero <- filter(intseq.data,
  time.since.prev.intseq.ms <= allowed.gap)

tttbl <- ttdata.turnsonly

## Changes in progress; basic sanity check as noted above (see BUT);
# fine-grained check still to come!

# allowed.gap <- 1000
# allowed.overlap <- 2000

## test data 1 ----

### read data into the spchtbl format
testdata1.filename <- "test_files/AAS-tabular/test-interaction-AllCDS.txt"
testdata1 <- read_spchtbl(filepath = testdata1.filename,
  tbltype = "aas-elan-txt", cliptier = "clip")

### retrieve transitions
#### stretch
testdata1.stretch <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "stretch")
testdata1.stretch$addressee <- as.character(
  testdata1.stretch$addressee)
#### strict
testdata1.strict <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
testdata1.strict$addressee <- as.character(
  testdata1.strict$addressee)
#### qulr
testdata1.qulr <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "qulr")
testdata1.qulr$addressee <- as.character(
  testdata1.qulr$addressee)
#### luqr
testdata1.luqr <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "luqr")
testdata1.luqr$addressee <- as.character(
  testdata1.luqr$addressee)

### check for a match
testdata1.stretch.answers <- read_csv_answercols.tt(
  "testdata1.stretch-correct.csv")
test1.stretch <- all_equal(testdata1.stretch,
  testdata1.stretch.answers, convert = TRUE)
testdata1.strict.answers <- read_csv_answercols.tt(
  "testdata1.strict-correct.csv")
test1.strict <- all_equal(testdata1.strict,
  testdata1.strict.answers, convert = TRUE)
testdata1.qulr.answers <- read_csv_answercols.tt(
  "testdata1.qulr-correct.csv")
test1.qulr <- all_equal(testdata1.qulr,
  testdata1.qulr.answers, convert = TRUE)
testdata1.luqr.answers <- read_csv_answercols.tt(
  "testdata1.luqr-correct.csv")
test1.luqr <- all_equal(testdata1.luqr,
  testdata1.luqr.answers, convert = TRUE)


## test data 2 ----

### read data into the spchtbl format
testdata2.filename <- "test_files/AAS-tabular/test-interaction-XDS.txt"
testdata2 <- read_spchtbl(filepath = testdata2.filename,
  tbltype = "aas-elan-txt", cliptier = "clip")

### retrieve transitions
#### stretch
testdata2.stretch <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "stretch")
testdata2.stretch$addressee <- as.character(
  testdata2.stretch$addressee)
#### strict
testdata2.strict <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
testdata2.strict$addressee <- as.character(
  testdata2.strict$addressee)
#### qulr
testdata2.qulr <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "qulr")
testdata2.qulr$addressee <- as.character(
  testdata2.qulr$addressee)
#### luqr
testdata2.luqr <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "luqr")
testdata2.luqr$addressee <- as.character(
  testdata2.luqr$addressee)

### check for a match
testdata2.stretch.answers <- read_csv_answercols.tt(
  "testdata2.stretch-correct.csv")
test2.stretch <- all_equal(testdata2.stretch,
  testdata2.stretch.answers, convert = TRUE)
testdata2.strict.answers <- read_csv_answercols.tt(
  "testdata2.strict-correct.csv")
test2.strict <- all_equal(testdata2.strict,
  testdata2.strict.answers, convert = TRUE)
testdata2.qulr.answers <- read_csv_answercols.tt(
  "testdata2.qulr-correct.csv")
test2.qulr <- all_equal(testdata2.qulr,
  testdata2.qulr.answers, convert = TRUE)
testdata2.luqr.answers <- read_csv_answercols.tt(
  "testdata2.luqr-correct.csv")
test2.luqr <- all_equal(testdata2.luqr,
  testdata2.luqr.answers, convert = TRUE)


## test data 3 ----

### read data into the spchtbl format
testdata3.filename <- "test_files/AAS-tabular/test-interaction-noCHI.txt"
testdata3 <- read_spchtbl(filepath = testdata3.filename,
  tbltype = "aas-elan-txt", cliptier = "clip")

### retrieve transitions
#### stretch
testdata3.stretch <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "stretch")
#### strict
testdata3.strict <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
#### qulr
testdata3.qulr <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "qulr")
#### luqr
testdata3.luqr <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap, min.utt.dur,
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
  spchtbl = testdata4, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHN", interactants = c("FAN", "MAN"),
  addressee.tags = "none", mode = "strict")
testdata4.strict$addressee <- as.character(
  testdata4.strict$addressee)

### check for a match
testdata4.strict.answers <- read_csv_answercols.tt(
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
  spchtbl = testdata5, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "Child Utterances", interactants = ".all-speakers",
  addressee.tags = "none", mode = "strict")
testdata5.strict$addressee <- as.character(
  testdata5.strict$addressee)

### check for a match
testdata5.strict.answers <- read_csv_answercols.tt(
  "testdata5.strict-correct.csv")
test5.strict <- all_equal(testdata5.strict,
  testdata5.strict.answers, convert = TRUE)


### test data 1: focus.child, interactants, addressee.tags ----
#### change focal speaker
testdata1.strict.FC1.focus <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "FC1", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
testdata1.strict.FC1.focus$addressee <- as.character(
  testdata1.strict.FC1.focus$addressee)
testdata1.strict.FC1.focus.answers <- read_csv_answercols.tt(
  "testdata1.strict.FC1.focus-correct.csv")
test1.strict.FC1.focus <- all_equal(testdata1.strict.FC1.focus,
  testdata1.strict.FC1.focus.answers, convert = TRUE)

testdata1.strict.FA1.focus <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "FA1", interactants = ".all-speakers",
  addressee.tags = "CDS", mode = "strict")
test1.strict.FA1.focus <- nrow(testdata1.strict.FA1.focus) == 0


#### change interactants
testdata1.strict.intFC1only <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = "FC1",
  addressee.tags = "CDS", mode = "strict")
testdata1.strict.intFC1only$addressee <- as.character(
  testdata1.strict.intFC1only$addressee)
testdata1.strict.intFC1only.answers <- read_csv_answercols.tt(
  "testdata1.strict.intFC1only-correct.csv")
test1.strict.intFC1only <- all_equal(testdata1.strict.intFC1only,
  testdata1.strict.intFC1only.answers, convert = TRUE)

testdata1.intpattern <- unique(testdata1$speaker)[grep(
  "[MFU]C\\d", unique(testdata1$speaker))]
testdata1.strict.intpattern <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = testdata1.intpattern,
  addressee.tags = "CDS", mode = "strict")
testdata1.strict.intpattern$addressee <- as.character(
  testdata1.strict.intpattern$addressee)
test1.strict.intpattern <- all_equal(testdata1.strict.intpattern,
  testdata1.strict.intFC1only.answers, convert = TRUE)

testdata1.strict.intFC1FA1 <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = c("FC1", "FA1"),
  addressee.tags = "CDS", mode = "strict")
testdata1.strict.intFC1FA1$addressee <- as.character(
  testdata1.strict.intFC1FA1$addressee)
test1.strict.intFC1FA1 <- all_equal(testdata1.strict.intFC1FA1,
  testdata1.strict.intFC1only.answers, convert = TRUE)

testdata1.strict.intFA1 <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = "FA1",
  addressee.tags = "CDS", mode = "strict")
test1.strict.intFA1 <- nrow(testdata1.strict.intFA1) == 0


#### change addressee.tags
testdata1.strict.tcds <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "TCDS", mode = "strict")
test1.strict.tcds <- nrow(testdata1.strict.tcds) == 0

testdata1.strict.none <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  focus.child = "CHI", interactants = ".all-speakers",
  addressee.tags = "none", mode = "strict")
testdata1.strict.none$addressee <- as.character(
  testdata1.strict.none$addressee)
test1.strict.none <- all_equal(testdata1.strict.none,
  testdata1.strict.answers, convert = TRUE)


### test data 1: intseq ----
testdata1.strict.intseq <- fetch_intseqs(testdata1.strict)
testdata1.strict.intseq$addressee <- as.character(
  testdata1.strict.intseq$addressee)
testdata1.strict.intseq.answers <- read_csv_answercols.is(
  "testdata1.strict.intseq-answers.csv")
test1.strict.intseq <- all_equal(testdata1.strict.intseq,
  testdata1.strict.intseq.answers, convert = TRUE)


# REPORT ----
all.tests <- c(test1.stretch, test1.strict,
  test1.qulr, test1.luqr, test2.stretch, test2.strict,
  test2.qulr, test2.luqr, test3.stretch, test3.strict,
  test3.qulr, test3.luqr, test4.strict, test5.strict,
  test1.strict.FC1.focus, test1.strict.FA1.focus,
  test1.strict.intFC1only, test1.strict.intFC1FA1,
  test1.strict.intpattern, test1.strict.tcds,
  test1.strict.none, test1.strict.intseq)

print(paste0("#### ", sum(all.tests), " of ",
  length(all.tests), " tests passed. ####"))
print(paste0("Test data 1, stretch passed: ", test1.stretch))
print(paste0("Test data 1, strict passed: ", test1.strict))
print(paste0("Test data 1, qulr passed: ", test1.qulr))
print(paste0("Test data 1, luqr passed: ", test1.luqr))
print(paste0("Test data 2, stretch passed: ", test2.stretch))
print(paste0("Test data 2, strict passed: ", test2.strict))
print(paste0("Test data 2, qulr passed: ", test2.qulr))
print(paste0("Test data 2, luqr passed: ", test2.luqr))
print(paste0("Test data 3, stretch passed: ", test3.stretch))
print(paste0("Test data 3, strict passed: ", test3.strict))
print(paste0("Test data 3, qulr passed: ", test3.qulr))
print(paste0("Test data 3, luqr passed: ", test3.luqr))
print(paste0("Test data 4, strict passed: ", test4.strict))
print(paste0("Test data 5, strict passed: ", test5.strict))
print(paste0("Test data 1, strict, FC1 focus passed: ", test1.strict.FC1.focus))
print(paste0("Test data 1, strict, FA1 focus passed: ", test1.strict.FA1.focus))
print(paste0("Test data 1, strict, FC1 only int passed: ", test1.strict.intFC1only))
print(paste0("Test data 1, strict, FC1 and FA1 only ints passed: ", test1.strict.intFC1FA1))
print(paste0("Test data 1, strict, child ints pattern passed: ", test1.strict.intpattern))
print(paste0("Test data 1, strict, TCDS tags passed: ", test1.strict.tcds))
print(paste0("Test data 1, strict, unknown tags passed: ", test1.strict.none))
print(paste0("Test data 1, strict, interactional sequences: ", test1.strict.intseq))
