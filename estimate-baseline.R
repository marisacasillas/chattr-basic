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


fetch_randomruns <- function(
  spchtbl, n.runs = default.n.runs,
  allowed.gap = default.max.gap, allowed.overlap = default.max.overlap,
  min.utt.dur = default.min.utt.dur,
  focus.child, interactants = default.interactants, 
  addressee.tags = default.addressee.tags,
  mode = default.mode, output = default.output,
  input.tttbl = NULL, return.real = TRUE) {
  # get the tttbl and intseq table with on which the random runs will be based
  # (typically to be passed to function, but also designed to handle a spchtbl
  # input table from a direct call of this function by a user)
  if (!is.null(input.tttbl)) {
    tbltype <- case_when(
      is.tttbl(input.tttbl) == TRUE ~ "tttbl",
      is.intseqtbl(input.tttbl) == TRUE ~ "intseqtbl",
      is.spchtbl(input.tttbl) == TRUE ~ "spchtbl",
      TRUE ~ "invalid tbltype"
    )
  }
  if (tbltype == "tttbl") {
    real.tttbl <- input.tttbl
    } else if (tbltype == "intseqtbl") {
    real.tttbl <- input.tttbl %>%
      dplyr::select(contains("seq")) # removes intseq and vocseq cols
    } else if (tbltype == "spchtbl" | is.spchtbl(spchtbl)) {
    # direct spchtbl input takes priority over input.tttbl
    spchtbl <- ifelse(is.spchtbl(spchtbl), spchtbl, input.tttbl)
    real.tttbl <- fetch_transitions(spchtbl, allowed.gap, allowed.overlap,
                                      min.utt.dur, focus.child, interactants,
                                      addressee.tags, mode)
    } else {
      print("Invalid input table or spchtbl to fetch_randomruns().")
      }
  
  # conduct random runs
  if (n.runs > 0) {
    if (output == "intseq" | output == "tttbl") {
      # extract turn-transition tables with shuffled vocalizations
      random.tttbls <- real.tttbl[1,] %>%
        mutate(random.run.num = 0) %>%
        full_join(tibble(random.run.num = sort(rep(
          seq(1:n.runs), nrow(real.tttbl)))), by = "random.run.num") %>%
        filter(random.run.num > 0)
      for (i in 1:n.runs) {
        print(paste0("Random run of turn transition detection: ",
                     i, " of ", n.runs))
        run.idx <- which(random.tttbls$random.run.num == i)
        random.tttbls[run.idx,
                      1:(length(random.tttbls)-1)] <- fetch_transitions(
                        shuffle_vocs(spchtbl), allowed.gap, allowed.overlap,
                        min.utt.dur, focus.child, interactants,
                        addressee.tags, mode)
      }
      if (output == "intseq") {
        # extract interaction sequence tables with new tttbls of shuffled vocs
        print(paste0("Random run of intseq detection: 1 of ", n.runs))
        run.idx <- which(random.intseqtbls$random.run.num == 1)
        current.tttbl <- filter(random.tttbls, random.run.num == 1) %>%
          dplyr::select(-random.run.num)
        random.intseqtbls <- fetch_intseqs(current.tttbl, allowed.gap) %>%
          mutate(random.run.num = 1) %>%
          full_join(tibble(random.run.num = sort(rep(
            seq(2:n.runs), nrow(real.intseqtbl)))), by = "random.run.num")
        for (i in 2:n.runs) {
          print(paste0("Random run of intseq detection: ", i, " of ", n.runs))
          run.idx <- which(random.intseqtbls$random.run.num == i)
          current.tttbl <- filter(random.tttbls, random.run.num == i) %>%
            dplyr::select(-random.run.num)
          random.intseqtbls[run.idx,
                            1:(length(random.intseqtbls)-1)] <- fetch_intseqs(
                              current.tttbl, allowed.gap)
        }
        random.tttbls <- NULL
      }
      if (return.real == TRUE) {
        all.tbls <- list(
          real.tt.vals = real.tttbl,
          random.tt.vals = ifelse(!is.null(random.tttbls),
                                  random.tttbls, random.intseqtbls))
      } else if (return.real == FALSE) {
        all.tbls <- list(
          real.tt.vals = NA,
          random.tt.vals = ifelse(!is.null(random.tttbls),
                                  random.tttbls, random.intseqtbls))
      }
      return (all.tbls)
    } else {
      print("Invalid output type for fetch_randomruns().")
    }
  }
}
