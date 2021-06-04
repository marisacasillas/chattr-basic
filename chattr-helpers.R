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
# and add associated annotated clip in a separate column
# if it's not there already
crop_to_annots <- function(spchtbl) {
  if (check_spchtbl(spchtbl)) {
    if ("annot.clip" %in% names(spchtbl)) {
     anns <- unique(spchtbl$annot.clip)
     for (i in 1:length(anns)) {
       ann.start.ms <- as.numeric(unlist(stringr::str_split(anns[i], "[_-]"))[2])
       ann.stop.ms <- as.numeric(unlist(stringr::str_split(anns[i], "[_-]"))[2])
       # crop utterances that spill over annotated boundaries
       spch.ann.overleft <- which(
         spchtbl$start.ms < ann.start.ms &
           spchtbl$stop.ms > ann.start.ms &
           spchtbl$stop.ms <= ann.stop.ms)
       spchtbl$start.ms[spch.ann.overleft] <- ann.start.ms
       spch.ann.overright <- which(
         spchtbl$stop.ms > ann.stop.ms &
           spchtbl$start.ms < ann.stop.ms &
           spchtbl$start.ms >= ann.start.ms)
       spchtbl$stop.ms[spch.ann.overright] <- ann.stop.ms
     }
     return(spchtbl)
    } else {
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
      return(spch)
    }
  }
}

# extract focus child speech utterances only
extract_focus_utts <- function(utts, focus) {
  focus.utts <- dplyr::filter(utts, speaker == focus)
  return(focus.utts)
}

# create a msec table of pre- and post- utt_0 windows
expand_msec_windows <- function(utts, allowed.gap, allowed.overlap) {
  utts <- utts %>%
    dplyr::mutate(
      prewindow.start = start.ms - allowed.gap,
      prewindow.stop = pmin(stop.ms, start.ms + allowed.overlap),
      postwindow.start = pmax(start.ms, stop.ms - allowed.overlap),
      postwindow.stop = stop.ms + allowed.gap)
  utt0.windows.msec <- tibble::tibble(
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
  utt0.windows.msec <- dplyr::filter(utt0.windows.msec,
    focal.utt.idx.prw != "" | focal.utt.idx.prw != "")
  return(utt0.windows.msec)
}

read_csv_answercols.tt <- function(filename) {
  answers <- readr::read_csv(
    filename,
    col_types = readr::cols(
      speaker = readr::col_character(),
      annot.clip = readr::col_character(),
      start.ms = readr::col_integer(),
      stop.ms = readr::col_integer(),
      addressee = readr::col_character(),
      spkr.prev.increment.start = readr::col_integer(),
      spkr.prev.increment.stop = readr::col_integer(),
      spkr.post.increment.start = readr::col_integer(),
      spkr.post.increment.stop = readr::col_integer(),
      prompt.spkr = readr::col_character(),
      prompt.start.ms = readr::col_integer(),
      prompt.stop.ms = readr::col_integer(),
      prompt.prev.increment.start = readr::col_integer(),
      prompt.prev.increment.stop = readr::col_integer(),
      response.spkr = readr::col_character(),
      response.start.ms = readr::col_integer(),
      response.stop.ms = readr::col_integer(),
      response.post.increment.start = readr::col_integer(),
      response.post.increment.stop = readr::col_integer()))
  return(answers)
}

read_csv_answercols.is <- function(filename) {
  answers <- readr::read_csv(
    filename,
    col_types = readr::cols(
      speaker = readr::col_character(),
      annot.clip = readr::col_character(),
      start.ms = readr::col_integer(),
      stop.ms = readr::col_integer(),
      addressee = readr::col_character(),
      spkr.n.increments = readr::col_integer(),
      spkr.prev.increment.start = readr::col_integer(),
      spkr.prev.increment.stop = readr::col_integer(),
      spkr.post.increment.start = readr::col_integer(),
      spkr.post.increment.stop = readr::col_integer(),
      prompt.spkr = readr::col_character(),
      prompt.start.ms = readr::col_integer(),
      prompt.stop.ms = readr::col_integer(),
      prompt.n.increments = readr::col_integer(),
      prompt.prev.increment.start = readr::col_integer(),
      prompt.prev.increment.stop = readr::col_integer(),
      response.spkr = readr::col_character(),
      response.start.ms = readr::col_integer(),
      response.stop.ms = readr::col_integer(),
      response.n.increments = readr::col_integer(),
      response.post.increment.start = readr::col_integer(),
      response.post.increment.stop = readr::col_integer(),
      intseq.num = readr::col_integer(),
      intseq.start.spkr = readr::col_character(),
      intseq.start.ms = readr::col_integer(),
      intseq.stop.spkr = readr::col_character(),
      intseq.stop.ms = readr::col_integer(),
      vocseq.num = readr::col_integer(),
      vocseq.start.ms = readr::col_integer(),
      vocseq.stop.ms = readr::col_integer()))
  return(answers)
}
