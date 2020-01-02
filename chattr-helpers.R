ann.marker <- "annotated-" ## ALSO USED IN TABULARIZE DATA
start.ann <- paste0("^", ann.marker, "\\d+", collapse = "") ## ALSO USED IN TRANSITION-DETECTORS


# check if speech table format is valid
check_spchtbl <- function(spchtbl) {
  # TO DO!
  return(TRUE)
}

# check if window table format is valid
check_windowtbl <- function(windowtbl) {
  # TO DO!
  return(TRUE)
}

# check if contingency table format is valid
check_conttbl <- function(conttbl) {
  # TO DO!
  return(TRUE)
}

# crop utterances to annotated clip boundaries
# and add associated annotated clip
crop_to_annots <- function(spchtbl) {
  if (check_spchtbl(spchtbl)) {
    ann.idx <- which(grepl(start.ann, spchtbl$speaker))
    anns <- spchtbl[ann.idx,]
    spch <- spchtbl[-ann.idx,]
    spch$annotclip <- "none assigned"
    for (i in 1:nrow(anns)) {
      ann.name <- anns$speaker[i]
      spch.ann <- which(
        spch$start.ms >= anns$start.ms[i] &
          spch$stop.ms <= anns$stop.ms[i])
      spch$annotclip[spch.ann] <- ann.name
      # include (and crop) utterances that spill over
      # annotated boundaries
      spch.ann.overleft <- which(
        spch$start.ms < anns$start.ms[i] &
          spch$stop.ms > anns$start.ms[i] &
          spch$stop.ms <= anns$stop.ms[i])
      spch$start.ms[spch.ann.overleft] <- anns$start.ms[i]
      spch$annotclip[spch.ann.overleft] <- ann.name
      spch.ann.overright <- which(
        spch$stop.ms > anns$stop.ms[i] &
          spch$start.ms < anns$stop.ms[i] &
          spch$start.ms >= anns$start.ms[i])
      spch$stop.ms[spch.ann.overright] <- anns$stop.ms[i]
      spch$annotclip[spch.ann.overright] <- ann.name
    }
  }
  return(spch)
}

# extract focus child speech utterances only
extract_focusutts <- function(utts, focus) {
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
