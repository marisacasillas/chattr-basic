ann.marker <- "annotated-" ## ALSO USED IN TABULARIZE DATA
start.ann <- paste0("^", ann.marker, "\\d+", collapse = "") ## ALSO USED IN CHATTR HELPERS
modes <- c("strict", "stretch", "luqr", "qulr")
prewindow.string <- "pre"
postwindow.string <- "post"

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
        # split n (total non-spch time) into k (# utts + 1) parts of random size
        # (NOTE: must be in ms, uses integers)
        # very slow :(
        non.spch.dur <- clip.dur - sum(spkr.vocs$duration)
        num.between.utt.intervals <- nrow(spkr.vocs) + 1
        between.utt.durs <- as.vector(table(sample(
          1:num.between.utt.intervals, non.spch.dur, replace = TRUE)))
        # If there are fewer than the max number of between.utt.intervals, set
        # these to zero and re-randomize
        if (length(between.utt.durs) < num.between.utt.intervals) {
          missing.durs <- num.between.utt.intervals - length(between.utt.durs)
          between.utt.durs <- sample(c(between.utt.durs, rep(0, missing.durs)))
        }
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
}

# option for just shuffling focal or just shuffling interactants?
fetch_baseline <- function(tbl, n.runs = 100,
                           spchtbl, allowed.gap, allowed.overlap,
                           min.utt.dur, focus.child, interactants,
                           addressee.tags, mode, behavior) {
  if (behavior == "intseq") {
    # extract the true turn-transition table
    real.intseqtbl <- fetch_transitions(tbl, allowed.gap, allowed.overlap,
                                    min.utt.dur, focus.child, interactants,
                                    addressee.tags, mode) %>%
      fetch_intseqs()
    # extract turn-transition tables with shuffled vocalizations
    random.intseqtbls <- tibble(random.run.num = sort(rep(seq(1:n.runs),
                                                      nrow(real.intseqtbl))))
    random.intseqtbls[, names(real.intseqtbl)] <- NA
    for (i in 1:n.runs) {
      run.idx <- which(random.tttbls$random.run.num == i)
      random.intseqtbls[run.idx, -1] <- fetch_transitions(
        shuffle_vocs(tbl), allowed.gap, allowed.overlap,
        min.utt.dur, focus.child, interactants,
        addressee.tags, mode) %>%
        fetch_intseqs()
    }
    return(random.intseqtbls)
  } else if (behavior == "tttbl") {
    # extract the true turn-transition table
    real.tttbl <- fetch_transitions(tbl, allowed.gap, allowed.overlap,
                                        min.utt.dur, focus.child, interactants,
                                        addressee.tags, mode)
    # extract turn-transition tables with shuffled vocalizations
    random.tttbls <- tibble(random.run.num = sort(rep(seq(1:n.runs),
                                                          nrow(real.tttbl))))
    random.tttbls[, names(real.tttbl)] <- NA
    for (i in 1:n.runs) {
      run.idx <- which(random.tttbls$random.run.num == i)
      random.tttbls[run.idx, -1] <- fetch_transitions(
        shuffle_vocs(tbl), allowed.gap, allowed.overlap,
        min.utt.dur, focus.child, interactants,
        addressee.tags, mode)
    }
    return(random.tttbls)
  } else {
    print("Invalid value for behavior of interest.")
  }
  
}
