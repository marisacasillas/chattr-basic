find_tttbl_continuations <- function(tttbl, focus.utts,
  int.utts, allowed.gap) {
  
  # find sequences of vocalizations by the focal speaker
  # that are unbroken by turn transitions to another speaker
  tttbl$stop.prev.ms <- c(
    0, tttbl$stop.ms[1:nrow(tttbl)-1])
  tttbl$response.prev.spkr <- c(
    NA, tttbl$response.spkr[1:nrow(tttbl)-1])
  tttbl <- tttbl %>%
    # mark a focal speaker vocalization as initiating
    # a new turn at talk when:
    dplyr::mutate(new.turn = dplyr::case_when(
      # the prior focal speaker vocalization had a response
      !is.na(response.prev.spkr) ~ 1,
      # the current focal speaker vocalization has a prompt
      !is.na(prompt.spkr) ~ 1,
      # the current focal speaker vocalization is separated from
      # its preceding vocalization by more than the allowed gap
      start.ms - stop.prev.ms > allowed.gap ~ 1,
      TRUE ~ 0))
  tttbl$speaker.turn.num <- cumsum(tttbl$new.turn == 1)
  tttbl <- tttbl %>%
    dplyr::select(-stop.prev.ms, -response.prev.spkr, -new.turn)
  tttbl.spkr.turns.multiincrement <- tttbl %>%
    dplyr::group_by(speaker.turn.num) %>%
    dplyr::summarize(
      `.groups` = "drop",
      spkr.prev.increment.start = min(start.ms),
      spkr.post.increment.stop = max(stop.ms),
      spkr.n.increments = dplyr::n()) %>%
    dplyr::filter(spkr.n.increments > 1) %>%
    dplyr::left_join(dplyr::select(tttbl, c(start.ms, stop.ms)),
      by = c("spkr.prev.increment.start" = "start.ms")) %>%
    dplyr::left_join(dplyr::select(tttbl, c(start.ms, stop.ms)),
      by = c("spkr.post.increment.stop" = "stop.ms")) %>%
    dplyr::rename("spkr.prev.increment.stop" = "stop.ms",
      "spkr.post.increment.start" = "start.ms") %>%
    dplyr::select(speaker.turn.num, spkr.n.increments,
      spkr.prev.increment.start, spkr.prev.increment.stop,
      spkr.post.increment.start, spkr.post.increment.stop)
  tttbl <- tttbl %>%
    dplyr::left_join(tttbl.spkr.turns.multiincrement, by = "speaker.turn.num") %>%
    dplyr::mutate(
      spkr.prev.increment.start = ifelse(
        spkr.prev.increment.start == start.ms,
        NA, spkr.prev.increment.start),
      spkr.prev.increment.stop = ifelse(
        is.na(spkr.prev.increment.start),
        NA, spkr.prev.increment.stop),
      spkr.post.increment.start = ifelse(
        spkr.post.increment.start == start.ms,
        NA, spkr.post.increment.start),
      spkr.post.increment.stop = ifelse(
        is.na(spkr.post.increment.start),
        NA, spkr.post.increment.stop)) %>%
    tidyr::replace_na(list(spkr.n.increments = 1))
  
  # find sequences of vocalizations by the prompt/response speakers
  # that are unbroken by turn transitions to the focal speaker
  prompts.basic <- tttbl %>%
    dplyr::filter(!is.na(prompt.spkr)) %>%
    dplyr::select(prompt.spkr, prompt.start.ms, prompt.stop.ms) %>%
    dplyr::distinct() %>%
    dplyr::rename(cont.spkr = prompt.spkr, cont.start.ms = prompt.start.ms,
      cont.stop.ms = prompt.stop.ms) %>%
    dplyr::mutate(has.response = 1)
  responses.basic <- tttbl %>%
    dplyr::filter(!is.na(response.spkr)) %>%
    dplyr::select(response.spkr, response.start.ms, response.stop.ms) %>%
    dplyr::distinct() %>%
    dplyr::rename(cont.spkr = response.spkr, cont.start.ms = response.start.ms,
      cont.stop.ms = response.stop.ms) %>%
    dplyr::mutate(has.prompt = 1)
  if (nrow(prompts.basic) > 0 | nrow(responses.basic) > 0) {
    if (nrow(prompts.basic) > 0 & nrow(responses.basic) > 0) {
      contingent.utts.basic <- dplyr::full_join(prompts.basic, responses.basic,
                                         by = c("cont.spkr", "cont.start.ms",
                                                "cont.stop.ms"))
    } else if (nrow(prompts.basic) > 0) {
      contingent.utts.basic <- prompts.basic %>%
        dplyr::mutate(has.prompt = NA)
    } else if (nrow(responses.basic) > 0) {
      contingent.utts.basic <- responses.basic %>%
        dplyr::mutate(has.response = NA)
    }
  } else {
    contingent.utts.basic <- tibble::tibble(
      cont.spkr = character(),
      cont.start.ms = numeric(),
      cont.stop.ms = numeric(),
      has.prompt = numeric(),
      has.response = numeric()
    )
  }
  # for each speaker in contingent utts
  unique.cont.spkrs <- unique(contingent.utts.basic$cont.spkr)
  cont.spkrs.continuations <- tibble::tibble()
  if (length(unique.cont.spkrs) > 0) {
    for (spkr in unique.cont.spkrs) {
      spkr.int.utts <- int.utts %>%
        dplyr::filter(speaker == spkr) %>%
        dplyr::select(speaker, start.ms, stop.ms)
      cont.spkr.tbl <- contingent.utts.basic %>%
        dplyr::filter(cont.spkr == spkr) %>%
        dplyr::full_join(spkr.int.utts, by = c(
          "cont.spkr" = "speaker", "cont.start.ms" = "start.ms",
          "cont.stop.ms" = "stop.ms")) %>%
        dplyr::arrange(cont.start.ms)
      cont.spkr.tbl$stop.prev.ms <- c(
        0, cont.spkr.tbl$cont.stop.ms[1:nrow(cont.spkr.tbl)-1])
      cont.spkr.tbl$response.prev.spkr <- c(
        NA, cont.spkr.tbl$has.response[1:nrow(cont.spkr.tbl)-1])
      if ("has.prompt" %in% names(cont.spkr.tbl)) {
        cont.spkr.tbl <- cont.spkr.tbl %>%
          dplyr::mutate(new.turn = dplyr::case_when(
            response.prev.spkr == 1 ~ 1,
            has.prompt == 1 ~ 1,
            cont.start.ms - stop.prev.ms > allowed.gap ~ 1,
            TRUE ~ 0))
        
      } else {
        browser()
      }
      cont.spkr.tbl$cont.spkr.turn.num <- cumsum(cont.spkr.tbl$new.turn == 1)
      cont.spkr.tbl <- cont.spkr.tbl %>%
        dplyr::select(-stop.prev.ms, -response.prev.spkr, -new.turn) %>%
        dplyr::ungroup()
      cont.spkr.tbl.multiincrement <- cont.spkr.tbl %>%
        dplyr::group_by(cont.spkr, cont.spkr.turn.num) %>%
        dplyr::summarize(
          `.groups` = "drop",
          cont.spkr.prev.increment.start = min(cont.start.ms),
          cont.spkr.post.increment.stop = max(cont.stop.ms),
          cont.spkr.n.increments = dplyr::n()) %>%
        dplyr::filter(cont.spkr.n.increments > 1) %>%
        dplyr::left_join(dplyr::select(int.utts, c(speaker, start.ms, stop.ms)),
                  by = c("cont.spkr" = "speaker",
                         "cont.spkr.prev.increment.start" = "start.ms")) %>%
        dplyr::left_join(dplyr::select(int.utts, c(speaker, start.ms, stop.ms)),
                  by = c("cont.spkr" = "speaker",
                         "cont.spkr.post.increment.stop" = "stop.ms")) %>%
        dplyr::rename("cont.spkr.prev.increment.stop" = "stop.ms",
                      "cont.spkr.post.increment.start" = "start.ms") %>%
        dplyr::ungroup() %>%
        dplyr::select(cont.spkr, cont.spkr.turn.num, cont.spkr.n.increments,
                      cont.spkr.prev.increment.start, cont.spkr.prev.increment.stop,
                      cont.spkr.post.increment.start, cont.spkr.post.increment.stop)
      spkr.contingent.utts.basic <- contingent.utts.basic %>%
        dplyr::filter(cont.spkr == spkr) %>%
        dplyr::left_join(dplyr::select(cont.spkr.tbl, c("cont.start.ms", "cont.spkr.turn.num")),
                  by = "cont.start.ms") %>%
        dplyr::left_join(cont.spkr.tbl.multiincrement,
                  by = c("cont.spkr.turn.num", "cont.spkr")) %>%
        dplyr::mutate(
          cont.spkr.prev.increment.start = ifelse(
            cont.spkr.prev.increment.start == cont.start.ms,
            NA, cont.spkr.prev.increment.start),
          cont.spkr.prev.increment.stop = ifelse(
            is.na(cont.spkr.prev.increment.start),
            NA, cont.spkr.prev.increment.stop),
          cont.spkr.post.increment.start = ifelse(
            cont.spkr.post.increment.start == cont.start.ms,
            NA, cont.spkr.post.increment.start),
          cont.spkr.post.increment.stop = ifelse(
            is.na(cont.spkr.post.increment.start),
            NA, cont.spkr.post.increment.stop)
        ) %>%
        tidyr::replace_na(list(cont.spkr.n.increments = 1))
      cont.spkrs.continuations <- dplyr::bind_rows(cont.spkrs.continuations,
                                            spkr.contingent.utts.basic)
    }
  }
  # add these continuation utterance start/stop times into the main tibble
  if (nrow(cont.spkrs.continuations) > 0) {
    tttbl <- tttbl %>%
      dplyr::left_join(dplyr::select(cont.spkrs.continuations, c("cont.spkr", "cont.start.ms",
                                                          "cont.spkr.prev.increment.start", "cont.spkr.prev.increment.stop",
                                                          "cont.spkr.n.increments")),
                by = c("prompt.spkr" = "cont.spkr", "prompt.start.ms" = "cont.start.ms")) %>%
      dplyr::rename("prompt.prev.increment.start" = "cont.spkr.prev.increment.start",
                    "prompt.prev.increment.stop" = "cont.spkr.prev.increment.stop",
                    "prompt.n.increments" = "cont.spkr.n.increments") %>%
      dplyr::left_join(dplyr::select(cont.spkrs.continuations, c("cont.spkr", "cont.start.ms",
                                                          "cont.spkr.post.increment.start", "cont.spkr.post.increment.stop",
                                                          "cont.spkr.n.increments")),
                by = c("response.spkr" = "cont.spkr", "response.start.ms" = "cont.start.ms")) %>%
      dplyr::rename("response.post.increment.start" = "cont.spkr.post.increment.start",
                    "response.post.increment.stop" = "cont.spkr.post.increment.stop",
                    "response.n.increments" = "cont.spkr.n.increments")
  } else {
    tttbl <- tttbl %>%
      dplyr::mutate(
        prompt.prev.increment.start = NA,
        prompt.prev.increment.stop = NA,
        prompt.n.increments = NA,
        response.post.increment.start = NA,
        response.post.increment.stop = NA,
        response.n.increments = NA,
      )
  }

  # clean up for return
  if (!("addressee" %in% names(tttbl))) {
    tttbl <- tttbl %>%
      dplyr::mutate(addressee = NA)
  }
  tttbl <- tttbl %>%
    dplyr::select(speaker, annot.clip, start.ms, stop.ms, addressee, spkr.n.increments,
      spkr.prev.increment.start, spkr.prev.increment.stop,
      spkr.post.increment.start, spkr.post.increment.stop,
      prompt.spkr, prompt.start.ms, prompt.stop.ms, prompt.n.increments,
      prompt.prev.increment.start, prompt.prev.increment.stop,
      response.spkr, response.start.ms, response.stop.ms, response.n.increments,
      response.post.increment.start, response.post.increment.stop)
  return(tttbl)
}

