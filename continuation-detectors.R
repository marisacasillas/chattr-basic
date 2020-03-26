find_TCU_edge_focal <- function(tttbl, speaker, start, stop,
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
      if (!(NA %in% candidates$spkr.prev.increment.start)) {
        prev.intseq.end <- tttbl %>%
          filter(!is.na(spkr.prev.increment.start)) %>%
          group_by(spkr.prev.increment.start) %>%
          summarize(
            end.resp = max(response.stop.ms, na.rm = TRUE),
            end.resp.increment = max(spkr.post.increment.stop, na.rm = TRUE),
            end.intseq.resp = max(end.resp, end.resp.increment, na.rm = TRUE)) %>%
          filter(end.intseq.resp > 0) %>%
          select(spkr.prev.increment.start, end.intseq.resp)
        focus.utt.prev.increment.ineligible <- candidates %>%
          left_join(prev.intseq.end, by = "spkr.prev.increment.start") %>%
          filter(end.intseq.resp >= stop.ms &
              start >= end.intseq.resp) %>%
          select(start.ms)
        candidates <- anti_join(candidates,
          focus.utt.prev.increment.ineligible, by = "start.ms")
      }
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

find_TCU_edge_nonfocal <- function(tttbl, foc.speaker,
  int.utts, speaker, start, stop, direction, allowed.gap) {
  n.candidates <- 1
  boundaries <- tibble(
    start.ms = start, stop.ms = stop)
  while (n.candidates > 0) {
    if (direction == "left") {
      candidates <- int.utts %>%
        filter(start.ms < start &
            stop.ms >= start - allowed.gap &
            speaker == speaker)
      candidates.with.responses <- tttbl %>%
        filter(prompt.start.ms %in% candidates$start.ms &
            prompt.spkr == speaker)
      if (nrow(candidates.with.responses) > 0) {
        candidates.with.responses <- candidates.with.responses %>%
          mutate(has.response == 1) %>%
          select(prompt.start.ms, has.response) %>%
          rename(start.ms = prompt.start.ms)
        candidates <- candidates %>%
          anti_join(candidates.with.responses, by = "start.ms")
      }
      if (nrow(candidates) > 0) {
        start <- min(candidates$start.ms)
        stop <- min(candidates$stop.ms)
        boundaries <- tibble(
          start.ms = start, stop.ms = stop)
      }
      n.candidates <- nrow(candidates)
    }
    if (direction == "right") {
      candidates <- int.utts %>%
        filter(start.ms <= stop + allowed.gap &
            stop.ms > stop &
            speaker == speaker)
      candidates.with.prompts <- tttbl %>%
        filter(response.start.ms %in% candidates$start.ms &
            response.spkr == speaker)
      if (nrow(candidates.with.prompts) > 0) {
        candidates.with.prompts <- candidates.with.prompts %>%
          mutate(has.prompt == 1) %>%
          select(response.start.ms, has.prompt) %>%
          rename(start.ms = response.start.ms)
        candidates <- candidates %>%
          anti_join(candidates.with.prompts, by = "start.ms")
      }
      if (nrow(candidates) > 0) {
        start <- max(candidates$start.ms)
        stop <- max(candidates$stop.ms)
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
      # check if it's an unattached edge utterance associated with
      # the end of the previous interactional sequence
      if (!(NA %in% focus.utt.prev.increment$spkr.prev.increment.start)) {
        prev.intseq.end <- tttbl %>%
          filter(!is.na(spkr.prev.increment.start)) %>%
          group_by(spkr.prev.increment.start) %>%
          summarize(
            end.resp = max(response.stop.ms, na.rm = TRUE),
            end.resp.increment = max(spkr.post.increment.stop, na.rm = TRUE),
            end.intseq.resp = max(end.resp, end.resp.increment, na.rm = TRUE)) %>%
          filter(end.intseq.resp > 0) %>%
          select(spkr.prev.increment.start, end.intseq.resp)
        focus.utt.prev.increment.ineligible <- focus.utt.prev.increment %>%
          left_join(prev.intseq.end, by = "spkr.prev.increment.start") %>%
          filter(end.intseq.resp >= stop.ms &
              tttbl$start.ms[i] >= end.intseq.resp) %>%
          select(start.ms)
        focus.utt.prev.increment <- anti_join(focus.utt.prev.increment,
          focus.utt.prev.increment.ineligible, by = "start.ms")
      }
      if (nrow(focus.utt.prev.increment) > 0) {
        boundaries <- find_TCU_edge_focal(tttbl, tttbl$speaker[i],
          focus.utt.prev.increment$start.ms[1],
          focus.utt.prev.increment$stop.ms[1],
          "left", allowed.gap)
        tttbl$spkr.prev.increment.start[i] <-
          boundaries$start.ms[1]
        tttbl$spkr.prev.increment.stop[i] <-
          boundaries$stop.ms[1]
      } else if (nrow(focus.utt.prev.increment) > 0 &
          nrow(focus.utt.prev.increment.ineligible) > 0) {
        boundaries <- find_TCU_edge_focal(tttbl, tttbl$speaker[i],
          focus.utt.prev.increment$start.ms[1],
          focus.utt.prev.increment$stop.ms[1],
          "left", allowed.gap)
        tttbl$spkr.prev.increment.start[i] <-
          boundaries$start.ms[1]
        tttbl$spkr.prev.increment.stop[i] <-
          boundaries$stop.ms[1]
      }
    }
    # exclude potential post increments that have prompts
    focus.utt.post.increment <- tttbl %>%
      filter(start.ms < tttbl$stop.ms[i] + allowed.gap &
          stop.ms > tttbl$stop.ms[i] &
          speaker == tttbl$speaker[i] &
          is.na(tttbl$prompt.spkr))
    if (nrow(focus.utt.post.increment) > 0) {
      max.idx <- nrow(focus.utt.post.increment)
      boundaries <- find_TCU_edge_focal(tttbl, tttbl$speaker[i],
        focus.utt.post.increment$start.ms[max.idx],
        focus.utt.post.increment$stop.ms[max.idx],
        "right", allowed.gap)
      tttbl$spkr.post.increment.start[i] <-
        boundaries$start.ms
      tttbl$spkr.post.increment.stop[i] <-
        boundaries$stop.ms
    }
    # add pre-increments for prompts
    if (!is.na(tttbl$prompt.spkr[i])) {
      prompt.prev.increment <- int.utts %>%
        filter(speaker == tttbl$prompt.spkr[i] &
            stop.ms >= tttbl$prompt.start.ms[i] - allowed.gap &
            start.ms < tttbl$prompt.start.ms[i])
      if (nrow(prompt.prev.increment) > 0) {
        # check the very last possible candidate for responses
        response.check <- tttbl %>%
          filter(prompt.start.ms == prompt.prev.increment$start.ms[nrow(
            prompt.prev.increment)] &
              prompt.spkr == tttbl$prompt.spkr[i])
        # if it doesn't have responses, check for further increments
        if (nrow(response.check) == 0) {
          boundaries <- find_TCU_edge_nonfocal(
            tttbl, tttbl$speaker[i], int.utts, tttbl$prompt.spkr[i],
            prompt.prev.increment$start.ms[nrow(prompt.prev.increment)],
            prompt.prev.increment$stop.ms[nrow(prompt.prev.increment)],
            "left", allowed.gap)
          tttbl$prompt.prev.increment.start[i] <-
            boundaries$start.ms
          tttbl$prompt.prev.increment.stop[i] <-
            boundaries$stop.ms
        }
      }
    }
    # add post-increments for responses
    if (!is.na(tttbl$response.spkr[i])) {
      response.post.increment <- int.utts %>%
        filter(speaker == tttbl$response.spkr[i] &
            start.ms <= tttbl$response.stop.ms[i] + allowed.gap &
            stop.ms > tttbl$response.stop.ms[i])
      if (nrow(response.post.increment) > 0) {
        # check the very first possible candidate for prompts
        prompt.check <- tttbl %>%
          filter(response.start.ms == response.post.increment$start.ms[1] &
              response.spkr == tttbl$response.spkr[i])
        # if it doesn't have prompts, check for further increments
        if (nrow(prompt.check) == 0) {
          boundaries <- find_TCU_edge_nonfocal(
            tttbl, tttbl$speaker[i], int.utts, tttbl$response.spkr[i],
            response.post.increment$start.ms[1],
            response.post.increment$stop.ms[1],
            "right", allowed.gap)
          tttbl$response.post.increment.start[i] <-
            boundaries$start.ms
          tttbl$response.post.increment.stop[i] <-
            boundaries$stop.ms
        }
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

