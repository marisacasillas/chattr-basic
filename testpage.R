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
source("fetch-chattr-info.R")


allowed.gap <- 1000
allowed.overlap <- 2000
min.utt.dur <- 0



## test data 1 ----

### read data into the spchtbl format
testdata1.filename <- "test_files/AAS-tabular/test-interaction-AllCDS.txt"
testdata1 <- read_spchtbl(filepath = testdata1.filename,
  tbltype = "aas-elan-txt", cliptier = "clip", lxonly = FALSE, nearonly = FALSE)

### retrieve transitions
#### stretch
testdata1.stretch <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "C", mode = "stretch")
testdata1.stretch$addressee <- as.character(
  testdata1.stretch$addressee)
#### strict
testdata1.strict <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "C", mode = "strict")
testdata1.strict$addressee <- as.character(
  testdata1.strict$addressee)
#### qulr
testdata1.qulr <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "C", mode = "qulr")
testdata1.qulr$addressee <- as.character(
  testdata1.qulr$addressee)
#### luqr
testdata1.luqr <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "C", mode = "luqr")
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
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "[CT]", mode = "stretch")
testdata2.stretch$addressee <- as.character(
  testdata2.stretch$addressee)
#### strict
testdata2.strict <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "[CT]", mode = "strict")
testdata2.strict$addressee <- as.character(
  testdata2.strict$addressee)
#### qulr
testdata2.qulr <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "[CT]", mode = "qulr")
testdata2.qulr$addressee <- as.character(
  testdata2.qulr$addressee)
#### luqr
testdata2.luqr <- fetch_transitions(
  spchtbl = testdata2, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "[CT]", mode = "luqr")
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
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "[CT]", mode = "stretch")
#### strict
testdata3.strict <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "[CT]", mode = "strict")
#### qulr
testdata3.qulr <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "[CT]", mode = "qulr")
#### luqr
testdata3.luqr <- fetch_transitions(
  spchtbl = testdata3, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "[CT]", mode = "luqr")

### check for a match
test3.stretch <- nrow(testdata3.stretch) == 0
test3.strict <- nrow(testdata3.strict) == 0
test3.qulr <- nrow(testdata3.qulr) == 0
test3.luqr <- nrow(testdata3.luqr) == 0



## test data 4 ---
testdata4.fullrun <- fetch_chatter_AAS(
  "test_files/AAS-tabular/test-interaction-XDS-lxonly.txt",
  n.runs = 2)
testdata4.fullrun.real.answers <- read_csv_answercols.is(
  "testdata4.fullrun.real.tt.vals.csv")
test4fr.real <- all_equal(testdata4.fullrun$real.tt.vals,
  testdata4.fullrun.real.answers, convert = TRUE)
# testdata4.fullrun.random.answers <- read_csv_answercols.is(
#   "testdata4.fullrun.random.tt.vals.csv")
# test4fr.random <- all_equal(testdata4.fullrun$random.tt.vals,
#   testdata4.fullrun.random.answers, convert = TRUE)

# TO DO:
# Need to add a test for the summary once that's implemented



## test data 5 ----
### read data into the spchtbl format
testdata5.filename <- "test_files/ITS/e20100727_110707_003581.its"
testdata5 <- read_spchtbl(filepath = testdata5.filename,
  tbltype = "lena-its")

### retrieve transitions
testdata5.strict <- fetch_transitions(
  spchtbl = testdata5, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CH", interactants = "[FM]A",
  addressee.tags = FALSE, mode = "strict")
testdata5.strict$addressee <- as.character(
  testdata5.strict$addressee)

### check for a match
testdata5.strict.answers <- read_csv_answercols.tt(
  "testdata5.strict-correct.csv")
test5.strict <- all_equal(testdata5.strict,
  testdata5.strict.answers, convert = TRUE)

### test with the metafunction
testdata5.fullrun <- fetch_chatter_LENA(
  testdata5.filename, n.runs = 2)
testdata5.fullrun.real.answers <- read_csv_answercols.is(
  "testdata5.fullrun.real.tt.vals.csv")
test5fr.real <- all_equal(testdata5.fullrun$real.tt.vals,
  testdata5.fullrun.real.answers, convert = TRUE)
# testdata5.fullrun.random.answers <- read_csv_answercols.is(
#   "testdata5.fullrun.random.tt.vals.csv")
# test5fr.random <- all_equal(testdata5.fullrun$random.tt.vals,
#   testdata5.fullrun.random.answers, convert = TRUE)

# TO DO:
# Need to add a test for the summary once that's implemented



## test data 6 ----
### read data into the spchtbl format
testdata6.filename <- "test_files/AltELAN-tabular/CT_sample1.txt"
testdata6 <- read_spchtbl(filepath = testdata6.filename,
  tbltype = "basic-speech-tbl", cliptier = "Coded Segment")

### retrieve transitions
testdata6.strict <- fetch_transitions(
  spchtbl = testdata6, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "Child Utterances", interactants = FALSE,
  addressee.tags = FALSE, mode = "strict")
testdata6.strict$addressee <- as.character(
  testdata6.strict$addressee)

### check for a match
testdata6.strict.answers <- read_csv_answercols.tt(
  "testdata6.strict-correct.csv")
test6.strict <- all_equal(testdata6.strict,
  testdata6.strict.answers, convert = TRUE)



## test data 7 ----
testdata7.fullrun <- fetch_chatter_BST(
  "test_files/AltELAN-tabular/CT_sample1-lxvocs.txt",
  cliptier = "Coded Segment",
  target.ptcp = "Child Utterances",
  n.runs = 2)
testdata7.fullrun.real.answers <- read_csv_answercols.is(
  "testdata7.fullrun.real.tt.vals.csv")
test7fr.real <- all_equal(testdata7.fullrun$real.tt.vals,
  testdata7.fullrun.real.answers, convert = TRUE)
# testdata7.fullrun.random.answers <- read_csv_answercols.is(
#   "testdata7.fullrun.random.tt.vals.csv")
# test7fr.random <- all_equal(testdata7.fullrun$random.tt.vals,
#   testdata7.fullrun.random.answers, convert = TRUE)



## test data 8 ----
testdata8.fullrun <- fetch_chatter_RTTM(
  "test_files/rttm/TEST.rttm",
  target.ptcp = "KCHI",
  n.runs = 2)
testdata8.fullrun.real.answers <- read_csv_answercols.is(
  "testdata8.fullrun.real.tt.vals.csv")
test8fr.real <- all_equal(testdata8.fullrun$real.tt.vals,
  testdata8.fullrun.real.answers, convert = TRUE)
# testdata8.fullrun.random.answers <- read_csv_answercols.is(
#   "testdata8.fullrun.random.tt.vals.csv")
# test8fr.random <- all_equal(testdata8.fullrun$random.tt.vals,
#   testdata8.fullrun.random.answers, convert = TRUE)



## test data 1 detailed tests: target.ptcp, interactants, addressee.tags ----

### change focal speaker
testdata1.strict.FC1.focus <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "FC1", interactants = FALSE,
  addressee.tags = "[CT]", mode = "strict")
testdata1.strict.FC1.focus$addressee <- as.character(
  testdata1.strict.FC1.focus$addressee)
testdata1.strict.FC1.focus.answers <- read_csv_answercols.tt(
  "testdata1.strict.FC1.focus-correct.csv")
test1.strict.FC1.focus <- all_equal(testdata1.strict.FC1.focus,
  testdata1.strict.FC1.focus.answers, convert = TRUE)

testdata1.strict.FA1.focus <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "FA1", interactants = FALSE,
  addressee.tags = "[CT]", mode = "strict")
test1.strict.FA1.focus <- nrow(testdata1.strict.FA1.focus) == 0

### change interactants
testdata1.strict.intFC1only <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = "FC1",
  addressee.tags = "[CT]", mode = "strict")
testdata1.strict.intFC1only$addressee <- as.character(
  testdata1.strict.intFC1only$addressee)
testdata1.strict.intFC1only.answers <- read_csv_answercols.tt(
  "testdata1.strict.intFC1only-correct.csv")
test1.strict.intFC1only <- all_equal(testdata1.strict.intFC1only,
  testdata1.strict.intFC1only.answers, convert = TRUE)

testdata1.intpattern <- "[MFU]C\\d"
testdata1.strict.intpattern <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = testdata1.intpattern,
  addressee.tags = FALSE, mode = "strict")
testdata1.strict.intpattern$addressee <- as.character(
  testdata1.strict.intpattern$addressee)
test1.strict.intpattern <- all_equal(testdata1.strict.intpattern,
  testdata1.strict.intFC1only.answers, convert = TRUE)

testdata1.strict.intFC1FA1 <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = "(FC1)|(FA1)",
  addressee.tags = "[CT]", mode = "strict")
testdata1.strict.intFC1FA1$addressee <- as.character(
  testdata1.strict.intFC1FA1$addressee)
test1.strict.intFC1FA1 <- all_equal(testdata1.strict.intFC1FA1,
  testdata1.strict.intFC1only.answers, convert = TRUE)

testdata1.strict.intFA1only <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = "FA1",
  addressee.tags = "[CT]", mode = "strict")
test1.strict.intFA1only <- nrow(testdata1.strict.intFA1only) == 0

### change addressee.tags
testdata1.strict.tcds <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = "T", mode = "strict")
test1.strict.tcds <- nrow(testdata1.strict.tcds) == 0

testdata1.strict.none <- fetch_transitions(
  spchtbl = testdata1, allowed.gap, allowed.overlap, min.utt.dur,
  target.ptcp = "CHI", interactants = FALSE,
  addressee.tags = FALSE, mode = "strict")
testdata1.strict.none$addressee <- as.character(
  testdata1.strict.none$addressee)
test1.strict.none <- all_equal(testdata1.strict.none,
  testdata1.strict.answers, convert = TRUE)



## test data 1: intseq ----
testdata1.strict.intseq <- fetch_intseqs(testdata1.strict, allowed.gap)
testdata1.strict.intseq$addressee <- as.character(
  testdata1.strict.intseq$addressee)
testdata1.strict.intseq.answers <- read_csv_answercols.is(
  "testdata1.strict.intseq-answers.csv")
test1.strict.intseq <- all_equal(testdata1.strict.intseq,
  testdata1.strict.intseq.answers, convert = TRUE)



## test data 5: intseq ----

# check that each intseq has at least one prompt or response
check.tts.in.intseqs <- testdata5.fullrun$real.tt.vals %>%
  group_by(intseq.num) %>%
  summarize(
    .groups = "drop",
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
check.NO.tts.in.vocseqs <- testdata5.fullrun$real.tt.vals %>%
  group_by(vocseq.num) %>%
  summarize(
    .groups = "drop",
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

# check intseq properties via their burst behavior
interactional.bursts <- testdata5.fullrun$real.tt.vals %>%
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
    .groups = "drop",
    n.intseq.prompts = sum(!is.na(prompt.start.ms)),
    n.intseq.responses = sum(!is.na(response.start.ms)),
    n.intseq.tts = n.intseq.prompts + n.intseq.responses
  )

# check that all intseqs have at least one prompt or one response
check.no.nonzero.n.intseq.prompts = ifelse(length(which(
  interactional.bursts$n.intseq.tts >= 0)) == nrow(interactional.bursts),
  TRUE, FALSE)

# check that bursts aren't closer together than the allowed gap
between.intseq.times <- tibble()
interactional.bursts <- interactional.bursts %>%
  arrange(intseq.num)
interactional.bursts$prev.intseq.stop <- c(
  0, interactional.bursts$intseq.stop.ms[1:(nrow(interactional.bursts)-1)])
interactional.bursts$time.since.prev.intseq.ms <- 
  interactional.bursts$intseq.start.ms - interactional.bursts$prev.intseq.stop
interactional.bursts$time.since.prev.intseq.min <- 
  interactional.bursts$time.since.prev.intseq.ms/60000
check.between.intseq.times.zero <- nrow(filter(interactional.bursts,
  time.since.prev.intseq.ms <= allowed.gap)) == 0



# REPORT ----
all.tests <- c(
  test1.stretch, test1.strict, test1.qulr, test1.luqr,
  test2.stretch, test2.strict, test2.qulr, test2.luqr,
  test3.stretch, test3.strict, test3.qulr, test3.luqr,
  test4fr.real, #test4fr.random,
  test5.strict, test5fr.real, #test5fr.random,
  check.tts.in.intseqs, check.NO.tts.in.vocseqs,
  check.no.nonzero.n.intseq.prompts, check.between.intseq.times.zero,
  test6.strict,
  test7fr.real, #test7fr.random,
  test8fr.real, #test8fr.random,
  test1.strict.FC1.focus, test1.strict.FA1.focus,
  test1.strict.intFC1only, test1.strict.intpattern,
  test1.strict.intFC1FA1, test1.strict.intFA1only,
  test1.strict.tcds, test1.strict.none,
  test1.strict.intseq)

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
print(paste0("Test data 4, metafunction fullrun real data passed: ", test4fr.real))
# print(paste0("Test data 4, metafunction fullrun random data passed: ", test4fr.random))
print(paste0("Test data 5, strict passed: ", test5.strict))
print(paste0("Test data 5, metafunction fullrun real data passed: ", test5fr.real))
# print(paste0("Test data 5, metafunction fullrun random data passed: ", test5fr.random))
print(paste0("Test data 5, intseqs contain tts: ", check.tts.in.intseqs))
print(paste0("Test data 5, vocseqs contain NO tts: ", check.NO.tts.in.vocseqs))
print(paste0("Test data 5, intseqs each have 1+ prompts and/or responses: ", check.no.nonzero.n.intseq.prompts))
print(paste0("Test data 5, intseqs are allowed.gap+ apart: ", check.between.intseq.times.zero))
print(paste0("Test data 6, strict passed: ", test6.strict))
print(paste0("Test data 7, metafunction fullrun real data passed: ", test7fr.real))
# print(paste0("Test data 7, metafunction fullrun random data passed: ", test7fr.random))
print(paste0("Test data 8, metafunction fullrun real data passed: ", test8fr.real))
# print(paste0("Test data 8, metafunction fullrun random data passed: ", test8fr.random))
print(paste0("Test data 1, strict, FC1 focus passed: ", test1.strict.FC1.focus))
print(paste0("Test data 1, strict, FA1 focus passed: ", test1.strict.FA1.focus))
print(paste0("Test data 1, strict, FC1 only int passed: ", test1.strict.intFC1only))
print(paste0("Test data 1, strict, child ints pattern passed: ", test1.strict.intpattern))
print(paste0("Test data 1, strict, FC1 and FA1 only ints passed: ", test1.strict.intFC1FA1))
print(paste0("Test data 1, strict, FA1 only int passed: ", test1.strict.intFA1only))
print(paste0("Test data 1, strict, TCDS tags passed: ", test1.strict.tcds))
print(paste0("Test data 1, strict, unknown tags passed: ", test1.strict.none))
print(paste0("Test data 1, strict, interactional sequences: ", test1.strict.intseq))
