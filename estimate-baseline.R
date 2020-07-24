
shuffle_vocs <- function(tbl) {
  
  # split n (total non-spch time) into k (# utts + 1) parts of random size
  # (NOTE: must be in ms; uses integers!)
  non.spch.dur <- TBD
  num.between.utt.intervals <- TBD
  between.utt.durs <- sample(as.vector(sample(
    1:num.between.utt.intervals, non.spch.dur, replace = TRUE)))
  # if there are fewer than the max number of between.utt.intervals, set
  # these to zero and re-randomize
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
