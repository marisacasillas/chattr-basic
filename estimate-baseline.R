ann.marker <- "annotated-" ## ALSO USED IN TABULARIZE DATA
start.ann <- paste0("^", ann.marker, "\\d+", collapse = "") ## ALSO USED IN CHATTR HELPERS
modes <- c("strict", "stretch", "luqr", "qulr")
prewindow.string <- "pre"
postwindow.string <- "post"

randomize_intervals <- function(num_intervals, period) {
  intervals <- sort(sample(1:period, num_intervals, replace = TRUE))
  d_intervals <- diff(intervals)
  return(c(d_intervals, period - sum(d_intervals)))
}

shuffle_vocs <- function(tbl) {
  unique.spkrs <- unique(tbl$speaker)[which((!grepl(
    ann.marker, unique(tbl$speaker))))]
  unique.clips <- unique(tbl$speaker)[which((grepl(
    ann.marker, unique(tbl$speaker))))]
  shuffled.tbl <- tbl %>%
    filter(grepl(ann.marker, speaker))
  for (spkr in unique.spkrs) {
    for (clip in unique.clips) {
      clip.idx <- which(tbl$speaker == clip)
      clip.on <- tbl$start.ms[clip.idx]
      clip.off <- tbl$stop.ms[clip.idx]
      clip.dur <- tbl$duration[clip.idx]
      spkr.vocs <- tbl %>%
        filter(speaker == spkr &
                 start.ms >= clip.on & stop.ms <= clip.off)
      if (nrow(spkr.vocs) > 0) {
        between.utt.durs <- randomize_intervals(nrow(spkr.vocs) + 1,
                            clip.dur - sum(spkr.vocs$duration))
        # Create shuffled speaker onsets
        ## randomize utterance order
        spkr.vocs <- slice(spkr.vocs, sample(1:n()))
        ## insert randomized between-utterance periods
        spkr.vocs$onset.interval <- between.utt.durs[
          1:length(between.utt.durs) - 1]
        spkr.vocs$dur.cum <- cumsum(spkr.vocs$duration)
        spkr.vocs$interval.cum <- cumsum(spkr.vocs$onset.interval)
        spkr.vocs$stop.ms <- spkr.vocs$interval.cum + spkr.vocs$dur.cum
        spkr.vocs$start.ms <- spkr.vocs$stop.ms - spkr.vocs$duration
        spkr.vocs <- spkr.vocs %>%
          dplyr::select(-onset.interval, -dur.cum, - interval.cum)
        # Write shuffled onsets to the output table
        shuffled.tbl <- bind_rows(shuffled.tbl, spkr.vocs)
      }
    }
  }
  return(shuffled.tbl)
}

# option for just shuffling focal or just shuffling interactants?
fetch_baseline <- function(tbl, n.runs = 100,
                           allowed.gap, allowed.overlap,
                           min.utt.dur, focus.child, interactants,
                           addressee.tags, mode, behavior) {
  if (behavior == "intseq" | behavior == "tttbl") {
    # extract the true turn-transition table
    real.tttbl <- fetch_transitions(tbl, allowed.gap, allowed.overlap,
                                    min.utt.dur, focus.child, interactants,
                                    addressee.tags, mode)
    # extract turn-transition tables with shuffled vocalizations
    random.tttbls <- real.tttbl[1,] %>%
      mutate(random.run.num = 0) %>%
      full_join(tibble(random.run.num = sort(rep(
        seq(1:n.runs), nrow(real.tttbl))))) %>%
      filter(random.run.num > 0)
    for (i in 1:n.runs) {
      # what about a progress bar?
      print(paste0("Random run tttbl: ", i))
      run.idx <- which(random.tttbls$random.run.num == i)
      random.tttbls[run.idx, 1:(length(random.tttbls)-1)] <- fetch_transitions(
        shuffle_vocs(tbl), allowed.gap, allowed.overlap,
        min.utt.dur, focus.child, interactants,
        addressee.tags, mode)
    }
    if (behavior == "intseq") {
      # extract interaction sequence tables with shuffled vocalizations
      real.intseqtbl <- fetch_intseqs(real.tttbl, allowed.gap)
      random.intseqtbls <- real.intseqtbl[1,] %>%
        mutate(random.run.num = 0) %>%
        full_join(tibble(random.run.num = sort(rep(
          seq(1:n.runs), nrow(real.intseqtbl))))) %>%
        filter(random.run.num > 0)
      for (i in 1:n.runs) {
        print(paste0("Random run intseq: ", i))
        run.idx <- which(random.intseqtbls$random.run.num == i)
        current.tttbl <- filter(random.tttbls, random.run.num == i) %>%
          dplyr::select(-random.run.num)
        random.intseqtbls[run.idx,
                          1:(length(random.intseqtbls)-1)] <- fetch_intseqs(
                            current.tttbl, allowed.gap)
      }
      # need to also return real info!
      return(random.intseqtbls)
    } else {
      # need to also return real info!
      return(random.tttbls)  
    }
  } else {
    print("Invalid value for behavior of interest.")
  }
  
}
