ann.marker <- "annotated-" ## ALSO USED IN TABULARIZE DATA
start.ann <- paste0("^", ann.marker, "\\d+.*", collapse = "") ## ALSO USED IN TRANSITION-DETECTORS


# check if speech table format is valid
check_spchtbl <- function(spchtbl) {
  # TO DO!
  nonempty <- nrow(spchtbl) > 0
  return(nonempty)
}

# check if window table format is valid
check_windowtbl <- function(windowtbl) {
  # TO DO!
  nonempty <- nrow(windowtbl) > 0
  return(nonempty)
}

# check if contingency table format is valid
check_conttbl <- function(conttbl) {
  # TO DO!
  nonempty <- nrow(conttbl) > 0
  return(nonempty)
}

# check if turn transition table format is valid
check_tttbl <- function(tttbl) {
  # TO DO!
  nonempty <- nrow(tttbl) > 0
  return(nonempty)
}

# crop utterances to annotated clip boundaries
# and add associated annotated clip
crop_to_annots <- function(spchtbl) {
  if (check_spchtbl(spchtbl)) {
    ann.idx <- which(grepl(start.ann, spchtbl$speaker))
    anns <- spchtbl[ann.idx,]
    spch <- spchtbl[-ann.idx,]
    spch$annot.clip <- "none assigned"
    for (i in 1:nrow(anns)) {
      ann.name <- anns$speaker[i]
      spch.ann <- which(
        spch$start.ms >= anns$start.ms[i] &
          spch$stop.ms <= anns$stop.ms[i])
      spch$annot.clip[spch.ann] <- ann.name
      # include (and crop) utterances that spill over
      # annotated boundaries
      spch.ann.overleft <- which(
        spch$start.ms < anns$start.ms[i] &
          spch$stop.ms > anns$start.ms[i] &
          spch$stop.ms <= anns$stop.ms[i])
      spch$start.ms[spch.ann.overleft] <- anns$start.ms[i]
      spch$annot.clip[spch.ann.overleft] <- ann.name
      spch.ann.overright <- which(
        spch$stop.ms > anns$stop.ms[i] &
          spch$start.ms < anns$stop.ms[i] &
          spch$start.ms >= anns$start.ms[i])
      spch$stop.ms[spch.ann.overright] <- anns$stop.ms[i]
      spch$annot.clip[spch.ann.overright] <- ann.name
    }
  }
  return(spch)
}

# extract focus child speech utterances only
extract_focus_utts <- function(utts, focus) {
  focus.utts <- filter(utts, speaker == focus)
  return(focus.utts)
}

# create a msec table of pre- and post- utt_0 windows
expand_msec_windows <- function(utts, allowed.gap, allowed.overlap) {
  utts <- utts %>%
    mutate(
      prewindow.start = start.ms - allowed.gap,
      prewindow.stop = pmin(stop.ms, start.ms + allowed.overlap),
      postwindow.start = pmax(start.ms, stop.ms - allowed.overlap),
      postwindow.stop = stop.ms + allowed.gap)
  utt0.windows.msec <- tibble(
    msec = c(min(utts$prewindow.start):max(utts$postwindow.stop)),
    focal.utt.idx.prw = "",
    focal.utt.idx.psw = ""
  )
  for (i in 1:nrow(utts)) {
    focal.utt.idx.prw <- which(
      utt0.windows.msec$msec >= utts$prewindow.start[i] &
        utt0.windows.msec$msec <= utts$prewindow.stop[i])
    utt0.windows.msec$focal.utt.idx.prw[focal.utt.idx.prw] <- paste(
      utt0.windows.msec$focal.utt.idx.prw[focal.utt.idx.prw],
      as.character(i), sep = '_')
    focal.utt.idx.psw <- which(
      utt0.windows.msec$msec >= utts$postwindow.start[i] &
        utt0.windows.msec$msec <= utts$postwindow.stop[i])
    utt0.windows.msec$focal.utt.idx.psw[focal.utt.idx.psw] <- paste(
      utt0.windows.msec$focal.utt.idx.psw[focal.utt.idx.psw],
      as.character(i), sep = '_')
  }
  utt0.windows.msec <- filter(utt0.windows.msec,
    focal.utt.idx.prw != "" | focal.utt.idx.prw != "")
  return(utt0.windows.msec)
}

read_csv_answercols.tt <- function(filename) {
  answers <- read_csv(
    filename,
    col_types = cols(
      speaker = col_character(),
      annot.clip = col_character(),
      start.ms = col_integer(),
      stop.ms = col_integer(),
      addressee = col_character(),
      spkr.prev.increment.start = col_integer(),
      spkr.prev.increment.stop = col_integer(),
      spkr.post.increment.start = col_integer(),
      spkr.post.increment.stop = col_integer(),
      prompt.spkr = col_character(),
      prompt.start.ms = col_integer(),
      prompt.stop.ms = col_integer(),
      prompt.prev.increment.start = col_integer(),
      prompt.prev.increment.stop = col_integer(),
      response.spkr = col_character(),
      response.start.ms = col_integer(),
      response.stop.ms = col_integer(),
      response.post.increment.start = col_integer(),
      response.post.increment.stop = col_integer()))
  return(answers)
}

read_csv_answercols.is <- function(filename) {
  answers <- read_csv(
    filename,
    col_types = cols(
      speaker = col_character(),
      annot.clip = col_character(),
      start.ms = col_integer(),
      stop.ms = col_integer(),
      addressee = col_character(),
      spkr.n.increments = col_integer(),
      spkr.prev.increment.start = col_integer(),
      spkr.prev.increment.stop = col_integer(),
      spkr.post.increment.start = col_integer(),
      spkr.post.increment.stop = col_integer(),
      prompt.spkr = col_character(),
      prompt.start.ms = col_integer(),
      prompt.stop.ms = col_integer(),
      prompt.n.increments = col_integer(),
      prompt.prev.increment.start = col_integer(),
      prompt.prev.increment.stop = col_integer(),
      response.spkr = col_character(),
      response.start.ms = col_integer(),
      response.stop.ms = col_integer(),
      response.n.increments = col_integer(),
      response.post.increment.start = col_integer(),
      response.post.increment.stop = col_integer(),
      intseq.num = col_integer(),
      intseq.start.spkr = col_character(),
      intseq.start.ms = col_integer(),
      intseq.stop.spkr = col_character(),
      intseq.stop.ms = col_integer(),
      vocseq.num = col_integer(),
      vocseq.start.ms = col_integer(),
      vocseq.stop.ms = col_integer()))
  return(answers)
}
