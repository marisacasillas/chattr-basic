ann.marker <- "annotated-" ## ALSO USED IN TABULARIZE DATA
start.ann <- paste0("^", ann.marker, "\\d+", collapse = "") ## ALSO USED IN CHATTR HELPERS
modes <- c("strict", "stretch", "luqr", "qulr")

# Returns the latest (largest) contingent
# utterances found per utt_0
choose_bigger_idx_LR <- function(conttbl) {
  if(check_conttbl(conttbl)) {
    prev_boundary <- NA
    for (i in 1:nrow(conttbl)) {
      boundaries <- as.numeric(unlist(strsplit(
        conttbl$cont.utt.boundary[i], "_")))
      speakers <- unlist(strsplit(
        conttbl$cont.utt.speaker[i], "_"))
      match.prev <- which(boundaries == prev_boundary)
      if (length(match.prev) > 0) {
        boundaries <- boundaries[-match.prev]
        speakers <- speakers[-match.prev]
      }
      if (length(boundaries) > 0) {
        conttbl$cont.utt.boundary[i] <- boundaries[length(boundaries)]
        conttbl$cont.utt.speaker[i] <- speakers[length(speakers)]
        prev_boundary <- boundaries[length(boundaries)]
      } else {
        conttbl$cont.utt.boundary[i] <- NA
        conttbl$cont.utt.speaker[i] <- NA
      }
    }
  }
  return(conttbl)
}

# Returns the earliest (smallest) contingent
# utterances found per utt_0
choose_smaller_idx_LR <- function(conttbl) {
  if(check_conttbl(conttbl)) {
    prev_boundary <- NA
    for (i in 1:nrow(conttbl)) {
      boundaries <- as.numeric(unlist(strsplit(
        conttbl$cont.utt.boundary[i], "_")))
      speakers <- unlist(strsplit(
        conttbl$cont.utt.speaker[i], "_"))
      match.prev <- which(boundaries == prev_boundary)
      if (length(match.prev) > 0) {
        boundaries <- boundaries[-match.prev]
        speakers <- speakers[-match.prev]
      }
      if (length(boundaries) > 0) {
        conttbl$cont.utt.boundary[i] <- boundaries[1]
        conttbl$cont.utt.speaker[i] <- speakers[1]
        prev_boundary <- boundaries[length(boundaries)]
      } else {
        conttbl$cont.utt.boundary[i] <- NA
        conttbl$cont.utt.speaker[i] <- NA
      }
    }
  }
  return(conttbl)
}

# Converts msec window match tables into a (proto-)contingency table
convert_prwpswtbl <- function(windowtbl) {
  if(check_windowtbl(windowtbl)) {
    names(windowtbl)[2] <- "matching.utt.idx"
    uniq.utt.idx <- as.numeric(unique(unlist(strsplit(paste(
      pull(windowtbl, matching.utt.idx), collapse = ""), "_"))))
    uniq.utt.idx <- uniq.utt.idx[which(!(is.na(uniq.utt.idx)))]
  }
  protoconttbl <- tibble(
    utt.idx = uniq.utt.idx,
    cont.utt.boundary = NA,
    cont.utt.speaker = NA
  )
  if (nrow(protoconttbl) > 0){
    for (i in 1:nrow(protoconttbl)) {
      utt.ptn <- paste0(
        "(_", protoconttbl$utt.idx[i], "$)|(_", protoconttbl$utt.idx[i], "_)")
      cont.utt.rows <- which(grepl(
        utt.ptn, windowtbl$matching.utt.idx))
      protoconttbl$cont.utt.boundary[i] <- paste(
        windowtbl$msec[cont.utt.rows], collapse = "_")
      protoconttbl$cont.utt.speaker[i] <- paste(
        windowtbl$speaker[cont.utt.rows], collapse = "_")
    }
  }
  return(protoconttbl)
}

# Converts pre- and post-window match results into a contingency table
combine_prwpsw_utts <- function(prewindow.stops, postwindow.starts) {
  prompts <- convert_prwpswtbl(prewindow.stops) %>%
    mutate(cont.type = "prompt")
  responses <- convert_prwpswtbl(postwindow.starts) %>%
    mutate(cont.type = "response")
  conttbl <- bind_rows(prompts, responses)
  if (nrow(conttbl) == 0) {
    conttbl <- tibble(
      utt.idx = double(),
      cont.utt.boundary = character(),
      cont.utt.speaker = character(),
      cont.type = character()
    )
  }
  return(conttbl)
}

# Selects up to one prompt and one response per utt_0 and returns
# a turn transition table with boundary time and speaker for each
# contingent utterance (prompt or response), with one utt_0 per row
# also includes speaker continuation information for each transition
create_tttbl <- function(conttbl, mode) {
  if(check_conttbl(conttbl)) {
    conttbl <- conttbl %>%
      arrange(cont.type, utt.idx)
    if (!(mode %in% modes)) {
      # TO DO
      print("NOT A MODE")
    } else {
      if (mode == "strict") {
        prompts <- choose_bigger_idx_LR(filter(conttbl,
          cont.type == "prompt"))
        responses <- choose_smaller_idx_LR(filter(conttbl,
          cont.type == "response"))
      }
      if (mode == "stretch") {
        prompts <- choose_smaller_idx_LR(filter(conttbl,
          cont.type == "prompt"))
        responses <- choose_bigger_idx_LR(filter(conttbl,
          cont.type == "response"))
      }
      if (mode == "luqr") {
        prompts <- choose_smaller_idx_LR(filter(conttbl,
          cont.type == "prompt"))
        responses <- choose_smaller_idx_LR(filter(conttbl,
          cont.type == "response"))
      }
      if (mode == "qulr") {
        prompts <- choose_bigger_idx_LR(filter(conttbl,
          cont.type == "prompt"))
        responses <- choose_bigger_idx_LR(filter(conttbl,
          cont.type == "response"))
      }
      prompts <- prompts %>%
        filter(!is.na(cont.utt.boundary)) %>%
        rename(prompt.stop.ms = cont.utt.boundary,
          prompt.spkr = cont.utt.speaker) %>%
        select(-cont.type)
      responses <- responses %>%
        filter(!is.na(cont.utt.boundary)) %>%
        rename(response.start.ms = cont.utt.boundary,
          response.spkr = cont.utt.speaker) %>%
        select(-cont.type)
      tttbl <- full_join(prompts, responses, by = "utt.idx") %>%
        arrange(utt.idx)
    }
  } else {
    tttbl <- tibble(
      utt.idx = double(),
      prompt.stop.ms = character(),
      prompt.spkr = character(),
      response.start.ms = character(),
      response.spkr = character()
    )
  }
  return(tttbl)
}

# Finds turn transitions between a focus child and other speakers
# within the annotated clips indicated in the spchtbl
fetch_transitions <- function(spchtbl, allowed.gap, allowed.overlap,
  focus.child, interactants, addressee.tags, mode) {
  # only include utterances occurring within annotated clips
  # (clips over-hanging utterances)
  spchtbl.cropped <- crop_to_annots(spchtbl)
  spchtbl.annsubset <- filter(spchtbl.cropped,
    annot.clip != "none assigned")
  # extract the interactant utterances
  if (paste(interactants, collapse = "_") == ".all-speakers") {
    int.utts <- filter(spchtbl.annsubset, speaker != focus.child &
        !grepl(start.ann, speaker))
  } else {
    int.utts <- filter(spchtbl.annsubset, speaker %in% interactants &
        !grepl(start.ann, speaker))
  }
  # only include those that are addressed appropriately
  if (addressee.tags == "TCDS") {
    int.utts <- filter(int.utts, addressee == "T")
  } else if (addressee.tags == "CDS") {
    int.utts <- filter(int.utts, addressee == "C")
  } # no need to subset further if there is no addressee coding
    # (i.e., if addressee == "none")
  # extract the focus child utterances (each is considered an 'utt_0')
  # and add transition window information
  chi.utts <- extract_focusutts(spchtbl.annsubset, focus.child)
  chi.utts <- mutate(chi.utts, utt.idx = c(1:nrow(chi.utts)))
  # define pre- and post- utterance windows for all utt_0s
  # (window overlap is possible with closely sequenced utts)
  chi.utts.ms.tbl <- expand_msec_windows(chi.utts, allowed.gap, allowed.overlap)
  # OTH-CHI transitions (i.e., utt_-1)
  # find candidate utterances to which utt_0 can be a response
  prewindow.stops <- filter(chi.utts.ms.tbl, focal.utt.idx.prw != "") %>%
    dplyr::select(-focal.utt.idx.psw) %>%
    left_join(select(int.utts, c(speaker, stop.ms)),
      by = c("msec" = "stop.ms")) %>%
    filter(!is.na(speaker))
  # CHI-OTH transitions (i.e., utt_+1)
  # find candidate utterances which can be a response to utt_0
  postwindow.starts <- filter(chi.utts.ms.tbl, focal.utt.idx.psw != "") %>%
    dplyr::select(-focal.utt.idx.prw) %>%
    left_join(select(int.utts, c(speaker, start.ms)),
      by = c("msec" = "start.ms")) %>%
    filter(!is.na(speaker))
  contingent.utts <- combine_prwpsw_utts(prewindow.stops, postwindow.starts)
  chi.idx.tttbl <- create_tttbl(contingent.utts, mode)
  if (nrow(chi.idx.tttbl) > 0) {
    chi.tttbl <- right_join(chi.utts, chi.idx.tttbl, by = "utt.idx") %>%
      select(-utt.idx) %>%
      mutate(
        prompt.stop.ms = as.numeric(prompt.stop.ms),
        response.start.ms = as.numeric(response.start.ms),
        annot.clip = gsub(ann.marker, "", annot.clip))
  } else {
    chi.tttbl <- tibble(
      speaker = character(),
      start.ms = integer(),
      stop.ms = integer(),
      addressee = character(),
      annot.clip = character(),
      prompt.stop.ms = double(),
      prompt.spkr = character(),
      response.start.ms = double(),
      response.spkr = character()
    )
  }
  return(chi.tttbl)
}

