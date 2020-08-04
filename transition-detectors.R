ann.marker <- "annotated-" ## ALSO USED IN TABULARIZE DATA
start.ann <- paste0("^", ann.marker, "\\d+", collapse = "") ## ALSO USED IN CHATTR HELPERS
modes <- c("strict", "stretch", "luqr", "qulr")

empty.continuation.utts <- tibble(
  speaker = character(),
  annot.clip = character(),
  start.ms = integer(),
  stop.ms = integer(),
  addressee = character(),
  spkr.prev.increment.start = integer(),
  spkr.prev.increment.stop = integer(),
  spkr.post.increment.start = integer(),
  spkr.post.increment.stop = integer(),
  prompt.spkr = character(),
  prompt.start.ms = double(),
  prompt.stop.ms = double(),
  prompt.prev.increment.start = integer(),
  prompt.prev.increment.stop = integer(),
  response.start.ms = double(),
  response.stop.ms = double(),
  response.spkr = character(),
  response.post.increment.start = integer(),
  response.post.increment.stop = integer()
)

# Finds turn transitions between a focus child and other speakers
# within the annotated clips indicated in the spchtbl
fetch_transitions <- function(spchtbl, allowed.gap, allowed.overlap,
  min.utt.dur, focus.child, interactants, addressee.tags, mode) {
  # exclude utterances deemed too short to "count"
  if (!("duration" %in% names(spchtbl))) {
    spchtbl <- mutate(spchtbl,
      duration = stop.ms - start.ms)
  }
  spchtbl <- filter(spchtbl, duration > min.utt.dur)
  # only include utterances occurring within annotated clips
  # (clips over-hanging utterances)
  spchtbl.cropped <- crop_to_annots(spchtbl)
  spchtbl.annsubset <- filter(spchtbl.cropped,
    annot.clip != "none assigned")
  # extract the interactant utterances
  if (interactants == FALSE) {
    int.utts <- filter(spchtbl.annsubset, speaker != focus.child &
                         !grepl(start.ann, speaker))
  } else {
    int.utts <- filter(spchtbl.annsubset, speaker != focus.child &
                         grepl(interactants, speaker) & 
                         !grepl(start.ann, speaker))
  }
  # only include those that are addressed appropriately
  if (addressee.tags != FALSE) {
    int.utts <- filter(int.utts, grepl(addressee.tags, addressee))
    if (nrow(int.utts) < 1) {
      print("No utterances with the specified interactant(s).")
      return(empty.continuation.utts)
    }
  } else {
    # no need to subset further if there is no addressee coding
    if (nrow(int.utts) < 1) {
      print("No utterances with the specified interactant(s).")
      return(empty.continuation.utts)
    }
  }

  # extract the focus child utterances (each is considered an 'utt_0')
  # and add transition window information
  chi.utts <- extract_focus_utts(spchtbl.annsubset, focus.child)
  if (nrow(chi.utts) < 1) {
    print("No utterances from the specified focus speaker.")
    return(empty.continuation.utts)
  }
  chi.utts <- mutate(
    chi.utts, utt.idx = c(1:nrow(chi.utts)),
    prewindow.start = start.ms - allowed.gap,
    prewindow.stop = pmin(stop.ms, start.ms + allowed.overlap),
    postwindow.start = pmax(start.ms, stop.ms - allowed.overlap),
    postwindow.stop = stop.ms + allowed.gap)
  sub.int.utts <- tibble()
  for (u in 1:nrow(chi.utts)) {
    prompts <- filter(int.utts,
      stop.ms >= chi.utts$prewindow.start[u] &
        stop.ms <= chi.utts$prewindow.stop[u] &
        start.ms < chi.utts$start.ms[u])
    if (nrow(prompts) > 0) {
      prompts$focal.utt.start.prompt = chi.utts$start.ms[u]
      sub.int.utts <- bind_rows(sub.int.utts, prompts)
    }
    responses <- filter(int.utts,
      start.ms >= chi.utts$postwindow.start[u] &
        start.ms <= chi.utts$postwindow.stop[u] &
        stop.ms > chi.utts$stop.ms[u])
    if (nrow(responses) > 0) {
      responses$focal.utt.start.response = chi.utts$start.ms[u]
      sub.int.utts <- bind_rows(sub.int.utts, responses)
    }
  }
  
  # PROMPTS
  # align potential prompts with their focal speaker utterances
  sub.int.prompts <- sub.int.utts %>%
    filter(!is.na(focal.utt.start.prompt)) %>%
    mutate(
      prompt.spkr = speaker,
      prompt.start.ms = start.ms,
      prompt.stop.ms = stop.ms) %>%
    dplyr::select(prompt.spkr, prompt.start.ms, prompt.stop.ms,
      focal.utt.start.prompt)
  chi.utts.prompts <- right_join(chi.utts, sub.int.prompts,
    by = c("start.ms" = "focal.utt.start.prompt"))
  # choose only one prompt per utterance;
  # each prompt can only be used once...
  # establish which prompts are re-used
  prompts.used.multi.times <- chi.utts.prompts %>%
    group_by(prompt.start.ms, prompt.spkr) %>%
    summarize(
      `.groups` = "drop",
      n = n()) %>%
    filter(n > 1) %>%
    pull(prompt.start.ms)
  # establish which focal utterances are re-used
  chiutt.assoc.multi.prompts <- chi.utts.prompts %>%
    group_by(start.ms) %>%
    summarize(
      `.groups` = "drop",
      n.instances = n()) %>%
    filter (n.instances > 1) %>%
    pull(start.ms)
  chi.utts.prompts <- chi.utts.prompts %>%
    mutate(
      prompt.used.elsewhere = ifelse(
        prompt.start.ms %in% prompts.used.multi.times, 1, 0),
      chiutt.used.elsewhere = ifelse(
        start.ms %in% chiutt.assoc.multi.prompts, 1, 0))
  easy.utts.prompts <- filter(chi.utts.prompts,
    prompt.used.elsewhere + chiutt.used.elsewhere == 0) %>%
    dplyr::select(-prompt.used.elsewhere, -chiutt.used.elsewhere)
  # select the contingencies among the transitions with multiple options
  hard.utts.prompts <- filter(chi.utts.prompts,
    prompt.used.elsewhere + chiutt.used.elsewhere > 0) %>%
    arrange(prompt.start.ms)
  unique.prompt.chiutts <- unique(hard.utts.prompts$start.ms)
  for (uttstart in unique.prompt.chiutts) {
    if (uttstart %in% unique(hard.utts.prompts$start.ms)) {
      if (mode == "strict" | mode == "qulr") {
        prompt.stop <- max(filter(
          hard.utts.prompts, start.ms == uttstart)$prompt.stop.ms)
      } else if (mode == "stretch" | mode == "luqr") {
        prompt.stop <- min(filter(
          hard.utts.prompts, start.ms == uttstart)$prompt.stop.ms)
      }
      # if there are two options, pick the utterance that started earlier,
      # otherwise pick the alphabetically first speaker
      if (nrow(filter(hard.utts.prompts,
        prompt.stop.ms == prompt.stop & start.ms == uttstart)) > 1) {
        stimultaneous.prompts <- filter(
          hard.utts.prompts, prompt.stop.ms == prompt.stop) %>%
          arrange(prompt.start.ms)
        prompt.start <- min(stimultaneous.prompts$prompt.start.ms)
        stimultaneous.prompts <- filter(
          stimultaneous.prompts, prompt.start.ms == prompt.start)
        if (nrow(stimultaneous.prompts) > 1) {
          stimultaneous.prompts <- arrange(stimultaneous.prompts, prompt.spkr)
        }
        stimultaneous.prompts.spkr <- stimultaneous.prompts$prompt.spkr[1]
        hard.utts.prompts <- hard.utts.prompts %>%
          filter(
            (start.ms == uttstart & prompt.stop.ms == prompt.stop &
                prompt.spkr == stimultaneous.prompts.spkr) |
              start.ms != uttstart & prompt.stop.ms != prompt.stop)
        next
      }
      hard.utts.prompts <- hard.utts.prompts %>%
        filter(
          (start.ms == uttstart & prompt.stop.ms == prompt.stop) |
            start.ms != uttstart & prompt.stop.ms != prompt.stop)
    }
  }
  # rejoin all the prompts together
  hard.utts.prompts <- hard.utts.prompts %>%
    dplyr::select(-prompt.used.elsewhere, -chiutt.used.elsewhere)
  chi.utts.prompts <- bind_rows(easy.utts.prompts, hard.utts.prompts) %>%
    arrange(start.ms)
  
  # RESPONSES
  # align potential responses with their focal speaker utterances
  sub.int.responses <- sub.int.utts %>%
    filter(!is.na(focal.utt.start.response)) %>%
    mutate(
      response.spkr = speaker,
      response.start.ms = start.ms,
      response.stop.ms = stop.ms) %>%
    dplyr::select(response.spkr, response.start.ms, response.stop.ms,
      focal.utt.start.response)
  chi.utts.responses <- right_join(chi.utts, sub.int.responses,
    by = c("start.ms" = "focal.utt.start.response"))
  # choose only one prompt per utterance;
  # each prompt can only be used once...
  # establish which prompts are re-used
  responses.used.multi.times <- chi.utts.responses %>%
    group_by(response.start.ms, response.spkr) %>%
    summarize(
      `.groups` = "drop",
      n = n()) %>%
    filter(n > 1) %>%
    pull(response.start.ms)
  # establish which focal utterances are re-used
  chiutt.assoc.multi.response <-  chi.utts.responses %>%
    group_by(start.ms) %>%
    summarize(
      `.groups` = "drop",
      n.instances = n()) %>%
    filter (n.instances > 1) %>%
    pull(start.ms)
  chi.utts.responses <- chi.utts.responses %>%
    mutate(
      response.used.elsewhere = ifelse(
        response.start.ms %in% responses.used.multi.times, 1, 0),
      chiutt.used.elsewhere = ifelse(
        start.ms %in% chiutt.assoc.multi.response, 1, 0))
  easy.utts.responses <- filter(chi.utts.responses,
    response.used.elsewhere + chiutt.used.elsewhere == 0) %>%
    dplyr::select(-response.used.elsewhere, -chiutt.used.elsewhere)
  # select the contingencies among the transitions with multiple options
  hard.utts.responses <- filter(chi.utts.responses,
    response.used.elsewhere + chiutt.used.elsewhere > 0) %>%
    arrange(response.start.ms)
  unique.response.chiutts <- unique(hard.utts.responses$start.ms)
  for (uttstart in unique.response.chiutts) {
    if (uttstart %in% unique(hard.utts.responses$start.ms)) {
      if (mode == "strict" | mode == "luqr") {
        response.start <- min(filter(
          hard.utts.responses, start.ms == uttstart)$response.start.ms)
      } else if (mode == "stretch" | mode == "qulr") {
        response.start <- max(filter(
          hard.utts.responses, start.ms == uttstart)$response.start.ms)
      }
      # if there are two options, pick the utterance that stops later,
      # otherwise pick the alphabetically first speaker
      if (nrow(filter(hard.utts.responses,
        response.start.ms == response.start & start.ms == uttstart)) > 1) {
        stimultaneous.responses <- filter(
          hard.utts.responses, response.start.ms == response.start) %>%
          arrange(response.stop.ms)
        response.stop <- max(stimultaneous.responses$response.stop.ms)
        stimultaneous.responses <- filter(
          stimultaneous.responses, response.stop.ms == response.stop)
        if (nrow(stimultaneous.responses) > 1) {
          stimultaneous.responses <- arrange(stimultaneous.responses, response.spkr)
        }
        stimultaneous.responses.spkr <- stimultaneous.responses$response.spkr[1]
        hard.utts.responses <- hard.utts.responses %>%
          filter(
            (start.ms == uttstart & response.start.ms == response.start &
                response.spkr == stimultaneous.responses.spkr) |
              start.ms != uttstart & response.start.ms != response.start)
        next
      }
      hard.utts.responses <- hard.utts.responses %>%
        filter(
          (start.ms == uttstart & response.start.ms == response.start) |
            start.ms != uttstart & response.start.ms != response.start)
    }
  }
  # rejoin all the prompts together
  hard.utts.responses <- hard.utts.responses %>%
    dplyr::select(-response.used.elsewhere, -chiutt.used.elsewhere)
  chi.utts.responses <- bind_rows(easy.utts.responses, hard.utts.responses) %>%
    arrange(start.ms)

  # combine the contingent utterances
  if (nrow(chi.utts.prompts) > 0) {
    chi.utts.prompts.min <- chi.utts.prompts %>%
      dplyr::select(start.ms,
        prompt.spkr, prompt.start.ms, prompt.stop.ms) %>%
      filter(!is.na(prompt.spkr))
    chi.tttbl <- left_join(chi.utts, chi.utts.prompts.min,
      by = "start.ms")
  } else {
    chi.tttbl <- chi.utts %>%
      mutate(
        prompt.spkr = NA,
        prompt.start.ms = NA,
        prompt.stop.ms = NA
      )
  }
  if (nrow(chi.utts.responses) > 0) {
    chi.utts.responses.min <- chi.utts.responses %>%
      dplyr::select(start.ms,
        response.spkr, response.start.ms, response.stop.ms) %>%
      filter(!is.na(response.spkr))
    chi.tttbl <- left_join(chi.tttbl, chi.utts.responses.min,
      by = "start.ms")
  } else {
    chi.tttbl <- chi.tttbl %>%
      mutate(
        response.spkr = NA,
        response.start.ms = NA,
        response.stop.ms = NA
      )
  }
  # add multi-TCU information
  if (nrow(chi.tttbl) > 0) {
    chi.tttbl <- chi.tttbl %>%
      dplyr::select(speaker, start.ms, stop.ms, annot.clip,
        prewindow.start, prewindow.stop,
        postwindow.start, postwindow.stop,
        prompt.start.ms, prompt.stop.ms, prompt.spkr,
        response.start.ms, response.stop.ms, response.spkr)
    continuation.utts <- find_tttbl_continuations(chi.tttbl,
      chi.utts, int.utts, allowed.gap) %>%
      mutate(
        speaker = as.character(speaker),
        annot.clip = as.character(annot.clip),
        start.ms = as.integer(start.ms),
        stop.ms = as.integer(stop.ms),
        addressee = as.character(addressee),
        spkr.n.increments = as.integer(spkr.n.increments),
        spkr.n.increments = as.integer(spkr.n.increments),
        spkr.n.increments = as.integer(spkr.n.increments),
        spkr.prev.increment.start = as.integer(spkr.prev.increment.start),
        spkr.prev.increment.stop = as.integer(spkr.prev.increment.stop),
        spkr.post.increment.start = as.integer(spkr.post.increment.start),
        spkr.post.increment.stop = as.integer(spkr.post.increment.stop),
        prompt.spkr = as.character(prompt.spkr),
        prompt.start.ms = as.integer(prompt.start.ms),
        prompt.stop.ms = as.integer(prompt.stop.ms),
        prompt.n.increments = as.integer(prompt.n.increments),
        prompt.prev.increment.start = as.integer(prompt.prev.increment.start),
        prompt.prev.increment.stop = as.integer(prompt.prev.increment.stop),
        response.spkr = as.character(response.spkr),
        response.start.ms = as.integer(response.start.ms),
        response.stop.ms = as.integer(response.stop.ms),
        response.n.increments = as.integer(response.n.increments),
        response.post.increment.start = as.integer(response.post.increment.start),
        response.post.increment.stop = as.integer(response.post.increment.stop)
      )
    return(continuation.utts)
  } else {
    return(empty.continuation.utts)
  }
}
