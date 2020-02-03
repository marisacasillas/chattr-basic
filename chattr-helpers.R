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

find_TCU_edge <- function(tttbl, speaker, start, stop,
  direction, allowed.gap) {
  n.candidates <- 1
  boundaries <- tibble(
    start.ms = start, stop.ms = stop)
  while (n.candidates > 0) {
    if (direction == "left") {
      candidates <- tttbl %>%
        filter(start.ms < start &
            stop.ms >= start - allowed.gap &
            speaker == speaker &
            is.na(tttbl$response.spkr))
      if (nrow(candidates) > 0) {
        start <- min(candidates$start.ms)
        stop <- min(candidates$stop.ms)
        boundaries <- tibble(
          start.ms = start, stop.ms = stop)
      }
      n.candidates <- nrow(candidates)
    }
    if (direction == "right") {
      candidates <- tttbl %>%
        filter(start.ms <= stop + allowed.gap &
            stop.ms > stop &
            speaker == speaker &
            is.na(tttbl$prompt.spkr))
      if (nrow(candidates) > 0) {
        start <- min(candidates$start.ms)
        stop <- min(candidates$stop.ms)
        boundaries <- tibble(
          start.ms = start, stop.ms = stop)
      }
      n.candidates <- nrow(candidates)
    }
  }
  return(boundaries)
}

find_tttbl_continuations <- function(tttbl, focus.utts,
  int.utts, allowed.gap) {
  addl.boundaries <- tibble(
    speaker = tttbl$speaker[1],
    spkr.prev.increment.start = integer(),
    spkr.prev.increment.stop = integer(),
    spkr.post.increment.start = integer(),
    spkr.post.increment.stop = integer(),
    prompt.prev.increment.start = integer(),
    prompt.prev.increment.stop = integer(),
    response.post.increment.start = integer(),
    response.post.increment.stop = integer()
  )
  tttbl <- left_join(tttbl, addl.boundaries, by = "speaker")
  for (i in 1:nrow(tttbl)) {
    # add pre- and post-increments for focus utterances
    # exclude potential pre increments that have responses
    focus.utt.prev.increment <- tttbl %>%
      filter(stop.ms >= tttbl$start.ms[i] - allowed.gap &
          start.ms < tttbl$start.ms[i] &
          speaker == tttbl$speaker[i] &
          is.na(tttbl$response.spkr))
    if (nrow(focus.utt.prev.increment) > 0) {
      boundaries <- find_TCU_edge(tttbl, tttbl$speaker[i],
        focus.utt.prev.increment$start.ms[1],
        focus.utt.prev.increment$stop.ms[1],
        "left", allowed.gap)
      tttbl$spkr.prev.increment.start[i] <-
        boundaries$start.ms[1]
      tttbl$spkr.prev.increment.stop[i] <-
        boundaries$stop.ms[1]
    }
    # exclude potential post increments that have prompts
    focus.utt.post.increment <- tttbl %>%
      filter(start.ms < tttbl$stop.ms[i] + allowed.gap &
          stop.ms > tttbl$stop.ms[i] &
          speaker == tttbl$speaker[i] &
          is.na(tttbl$prompt.spkr))
    if (nrow(focus.utt.post.increment) > 0) {
      max.idx <- nrow(focus.utt.post.increment)
      boundaries <- find_TCU_edge(tttbl, tttbl$speaker[i],
        focus.utt.post.increment$start.ms[max.idx],
        focus.utt.post.increment$stop.ms[max.idx],
        "right", allowed.gap)
      tttbl$spkr.post.increment.start[i] <-
        boundaries$start.ms[max.idx]
      tttbl$spkr.post.increment.stop[i] <-
        boundaries$stop.ms[max.idx]
    }
    ##
    ## NEED TO RESHUFFLE INT.UTTS LIKE CHI.TTTBL
    ##
    
    # add pre- and post-increments for prompts
    if (!is.na(tttbl$prompt.spkr[i])) {
      prompt.prev.increment <- int.utts %>%
        filter(speaker == tttbl$prompt.spkr[i] &
            stop.ms >= tttbl$prompt.start.ms[i] - allowed.gap &
            start.ms < tttbl$prompt.start.ms[i])
      if (nrow(prompt.prev.increment) > 0) {
        boundaries <- find_TCU_edge(tttbl, tttbl$prompt.spkr[i],
          prompt.prev.increment$start.ms[1],
          prompt.prev.increment$stop.ms[1],
          "left", allowed.gap)
        tttbl$prompt.prev.increment.start[i] <-
          boundaries$start.ms[1]
        tttbl$prompt.prev.increment.stop[i] <-
          boundaries$stop.ms[1]
      }
    }
    # add pre- and post-increments for responses
    if (!is.na(tttbl$response.spkr[i])) {
      response.post.increment <- int.utts %>%
        filter(speaker == tttbl$response.spkr[i] &
            start.ms <= tttbl$response.stop.ms[i] + allowed.gap &
            stop.ms > tttbl$response.stop.ms[i])
      if (nrow(response.post.increment) > 0) {
        boundaries <- find_TCU_edge(tttbl, tttbl$response.spkr[i],
          response.post.increment$start.ms[1],
          response.post.increment$stop.ms[1],
          "left", allowed.gap)
        tttbl$response.post.increment.start[i] <-
          boundaries$start.ms[1]
        tttbl$response.post.increment.stop[i] <-
          boundaries$stop.ms[1]
      }
    }
  }
  if (!("addressee" %in% names(tttbl))) {
    tttbl <- tttbl %>%
      mutate(addressee = NA)
  }
  tttbl <- tttbl %>%
    select(speaker, annot.clip, start.ms, stop.ms, addressee,
      spkr.prev.increment.start, spkr.prev.increment.stop,
      spkr.post.increment.start, spkr.post.increment.stop,
      prompt.spkr, prompt.start.ms, prompt.stop.ms,
      prompt.prev.increment.start, prompt.prev.increment.stop,
      response.spkr, response.start.ms, response.stop.ms,
      response.post.increment.start, response.post.increment.stop)
  return(tttbl)
}

