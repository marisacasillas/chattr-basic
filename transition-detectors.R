ann.marker <- "annotated-" ## ALSO USED IN TABULARIZE DATA
start.ann <- paste0("^", ann.marker, "\\d+", collapse = "") ## ALSO USED IN CHATTR HELPERS
modes <- c("strict", "stretch", "luqr", "qulr")


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

#mode: stretch, strict, luqr, qulr

# converts msec window match tables into a (proto-)contingency table
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
  for (i in 1:nrow(protoconttbl)) {
    utt.ptn <- paste0("(_", protoconttbl$utt.idx[i], "$)|(_", protoconttbl$utt.idx[i], "_)")
    cont.utt.rows <- which(grepl(utt.ptn, windowtbl$matching.utt.idx))
    protoconttbl$cont.utt.boundary[i] <- paste(windowtbl$msec[cont.utt.rows], collapse = "_")
    protoconttbl$cont.utt.speaker[i] <- paste(windowtbl$speaker[cont.utt.rows], collapse = "_")
  }
  return(protoconttbl)
}

# converts pre- and post-window match results into a contingency table
combine_prwpsw_utts <- function(prewindow.stops, postwindow.starts) {
  prompts <- convert_prwpswtbl(prewindow.stops) %>%
    mutate(cont.type = "prompt")
  responses <- convert_prwpswtbl(postwindow.starts) %>%
    mutate(cont.type = "response")
  conttbl <- bind_rows(prompts, responses)
  return(conttbl)
}

# selects up to one prompt and one response per utt_0 and returns
# a turn transition table with boundary time and speaker for each
# contingent utterance (prompt or response), with one utt_0 per row
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
    annotclip != "none assigned")
  # extract the interactant utterances
  if (interactants == ".all-speakers") {
    int.utts <- filter(spchtbl.annsubset, speaker != "CHI" &
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
  chi.utts <- extract_focusutts(spchtbl.annsubset, focus.child) %>%
    mutate(utt.idx = c(1:nrow(chi.utts)))
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
  chi.tttbl <- right_join(chi.utts, chi.idx.tttbl, by = "utt.idx") %>%
    select(-utt.idx) %>%
    mutate(
      prompt.stop.ms = as.numeric(prompt.stop.ms),
      response.start.ms = as.numeric(response.start.ms))
  return(chi.tttbl)
}

  # Select the final set of transitions from these candidates
  # - one pre and one post per child voc (uses stretch/strict)
  # - each int voc can only be one of each type max
  # - no crossing w/i dyad: L->R dominance
  # - other multiple options, even w/ stretch vs. strict setting?
  #        > int continuity > alignment score
  

## OLD: create summaries
turn.transitions.overview.o_c <- turn.transitions %>%
  filter(!(is.na(tm1.val))) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(n.o_c.tts = n()) %>%
  full_join(all.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(n.o_c.tts = 0)) %>%
  arrange(aclew_child_id, segment) %>%
  mutate(duration = ifelse(grepl('extension', sample_type), 5,
                           ifelse(grepl('random', sample_type), 5, 1)))

turn.transitions.overview.c_o <- turn.transitions %>%
  filter(!(is.na(tp1.val))) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(n.c_o.tts = n()) %>%
  full_join(all.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(n.c_o.tts = 0)) %>%
  arrange(aclew_child_id, segment) %>%
  mutate(duration = ifelse(grepl('extension', sample_type), 5,
                           ifelse(grepl('random', sample_type), 5, 1)))

# Combine the turn-taking info into one table
turn.transitions.overview <- turn.transitions.overview.o_c %>%
  full_join(dplyr::select(turn.transitions.overview.c_o,
                          c("aclew_child_id", "segment", "n.c_o.tts")),
            by = c("aclew_child_id", "segment")) %>%
  mutate(n.o_c.tpm = n.o_c.tts/duration,
         n.c_o.tpm = n.c_o.tts/duration) %>%
  left_join(ptcp.info, by = "aclew_child_id") %>%
  left_join(spkrs.per.seg.all, by = c("aclew_child_id", "segment",
                                      "sample", "sample_type")) %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName"))

# Summaries of turn taking by sample
turn.taking.by.sample <- turn.transitions.overview %>%
  group_by(sample) %>%
  summarise(mean.n.o_c.tpm = mean(n.o_c.tpm),
         median.n.o_c.tpm = median(n.o_c.tpm),
         min.n.o_c.tpm = min(n.o_c.tpm),
         max.n.o_c.tpm = max(n.o_c.tpm),
         mean.n.c_o.tpm = mean(n.c_o.tpm),
         median.n.c_o.tpm = median(n.c_o.tpm),
         min.n.c_o.tpm = min(n.c_o.tpm),
         max.n.c_o.tpm = max(n.c_o.tpm))
turn.taking.by.child.bysample <- turn.transitions.overview %>%
  group_by(aclew_child_id, sample) %>%
  summarise(mean.n.o_c.tpm = mean(n.o_c.tpm),
         median.n.o_c.tpm = median(n.o_c.tpm),
         min.n.o_c.tpm = min(n.o_c.tpm),
         max.n.o_c.tpm = max(n.o_c.tpm),
         mean.n.c_o.tpm = mean(n.c_o.tpm),
         median.n.c_o.tpm = median(n.c_o.tpm),
         min.n.c_o.tpm = min(n.c_o.tpm),
         max.n.c_o.tpm = max(n.c_o.tpm))

# # Turn transitions:
# # other-to-child and child-to-other
# turn.transitions <- tibble()
# for (i in 1:nrow(all.segments)) {
#   subdata <- all.data %>%
#     filter(aclew_child_id == all.segments$aclew_child_id[i] &
#              segment == all.segments$segment[i])
#   c_utt <- subdata %>%
#     filter(tier == "CHI")
#   used.tm1s <- c()
#   used.tp1s <- c()
#   child <- all.segments$aclew_child_id[i]
#   # Save turn-by-turn info
#   chi.turn.info <- c_utt %>%
#     select(aclew_child_id, segment, tier, speaker, start, stop) %>%
#     mutate(tm1.tier = NA, tm1.speaker = NA, tm1.start = NA, tm1.stop = NA, tm1.val = NA,
#            tp1.tier = NA, tp1.speaker = NA, tp1.start = NA, tp1.stop = NA, tp1.val = NA)
#   for (j in 1:nrow(c_utt)) {
#     # Find CHI-OTH transitions
#     # "T" responses that start:
#     #    - earliest: when the child starts vocalizing, with a limit on vocal overlap
#     #    - latest: before the maximum allowed gap after the child's voc ends
#     tp1.start <- max((c_utt$stop[j] - allowed.overlap), c_utt$start[j])
#     tp1.stop <- c_utt$stop[j] + allowed.gap
#     t.plus1 <- which(subdata$speaker != "CHI" &
#                     (subdata$val == "T") &
#                     subdata$start >= tp1.start &
#                     subdata$start <= tp1.stop)
#     # Find OTH-CHI transitions
#     # "T" prompts that start:
#     #    - earliest: within the maximum gap allowed before the child begins vocalizing
#     #    - latest: when the child stops vocalizing, with a limit on vocal overlap
#     tm1.start <- c_utt$start[j] - allowed.gap
#     tm1.stop <- min((c_utt$start[j] + allowed.overlap), c_utt$stop[j])
#     t.minus1 <- which(subdata$speaker != "CHI" &
#                     (subdata$val == "T") &
#                     subdata$stop <= tm1.stop &
#                     subdata$stop >= tm1.start)
#     if(length(t.plus1) > 0) {
#       tp1.match <- 0
#       for (turn in t.plus1) {
#         # Each OTH turn can only be a response once 
#         if (!(turn %in% used.tp1s) & tp1.match == 0) {
#           used.tp1s <- c(used.tp1s, turn)
#           chi.turn.info$tp1.tier[j] <- subdata$tier[turn]
#           chi.turn.info$tp1.speaker[j] <- subdata$speaker[turn]
#           chi.turn.info$tp1.start[j] <- subdata$start[turn]
#           chi.turn.info$tp1.stop[j] <- subdata$stop[turn]
#           chi.turn.info$tp1.val[j] <- subdata$val[turn]
#           tp1.match <- 1
#         }
#       }
#     }
#     if(length(t.minus1) > 0) {
#       tm1.match <- 0
#       for (turn in t.minus1) {
#         # Each OTH turn can only be a prompt once 
#         if (!(turn %in% used.tm1s) & tm1.match == 0) {
#           used.tm1s <- c(used.tm1s, turn)
#           chi.turn.info$tm1.tier[j] <- subdata$tier[turn]
#           chi.turn.info$tm1.speaker[j] <- subdata$speaker[turn]
#           chi.turn.info$tm1.start[j] <- subdata$start[turn]
#           chi.turn.info$tm1.stop[j] <- subdata$stop[turn]
#           chi.turn.info$tm1.val[j] <- subdata$val[turn]
#           tm1.match <- 1
#         }
#       }
#     }
#   }
#   turn.transitions <- bind_rows(turn.transitions, chi.turn.info)
# }
