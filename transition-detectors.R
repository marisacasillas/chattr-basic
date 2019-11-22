# Finds turn transitions between a focus child and other speakers
# within the annotated clips indicated in the spchtbl
fetch_transitions <- function(spchtbl, allowed.gap, allowed.overlap,
  focus.child, interactants, addressee.tags, mode) {
  # extract the focus child utterances (each is considered an 'utt_0')
  chi.utts <- filter(spchtbl, speaker == focus.child)
  # extract the interactant utterances
  if (interactants == ".all-speakers") {
    int.utts <- filter(spchtbl, speaker != "CHI" &
        !grepl("^annotated-", speaker))
  } else {
    int.utts <- filter(spchtbl, speaker %in% interactants)
  }
  # only include those that are addressed appropriately
  if (addressee.tags == "TCDS") {
    int.utts <- filter(int.utts, addressee == "T")
  } else if (addressee.tags == "CDS") {
    int.utts <- filter(int.utts, addressee == "C")
  }
  # OTH-CHI transitions (i.e., utt_-1)
  # find candidate utterances to which utt_0 can be a response
  
  # CHI-OTH transitions (i.e., utt_+1)
  # find candidate utterances which can be a response to utt_0
  
}


# Turn transitions:
# other-to-child and child-to-other
turn.transitions <- tibble()
for (i in 1:nrow(all.segments)) {
  subdata <- all.data %>%
    filter(aclew_child_id == all.segments$aclew_child_id[i] &
             segment == all.segments$segment[i])
  c_utt <- subdata %>%
    filter(tier == "CHI")
  used.tm1s <- c()
  used.tp1s <- c()
  child <- all.segments$aclew_child_id[i]
  # Save turn-by-turn info
  chi.turn.info <- c_utt %>%
    select(aclew_child_id, segment, tier, speaker, start, stop) %>%
    mutate(tm1.tier = NA, tm1.speaker = NA, tm1.start = NA, tm1.stop = NA, tm1.val = NA,
           tp1.tier = NA, tp1.speaker = NA, tp1.start = NA, tp1.stop = NA, tp1.val = NA)
  for (j in 1:nrow(c_utt)) {
    # Find CHI-OTH transitions
    # "T" responses that start:
    #    - earliest: when the child starts vocalizing, with a limit on vocal overlap
    #    - latest: before the maximum allowed gap after the child's voc ends
    tp1.start <- max((c_utt$stop[j] - allowed.overlap), c_utt$start[j])
    tp1.stop <- c_utt$stop[j] + allowed.gap
    t.plus1 <- which(subdata$speaker != "CHI" &
                    (subdata$val == "T") &
                    subdata$start >= tp1.start &
                    subdata$start <= tp1.stop)
    # Find OTH-CHI transitions
    # "T" prompts that start:
    #    - earliest: within the maximum gap allowed before the child begins vocalizing
    #    - latest: when the child stops vocalizing, with a limit on vocal overlap
    tm1.start <- c_utt$start[j] - allowed.gap
    tm1.stop <- min((c_utt$start[j] + allowed.overlap), c_utt$stop[j])
    t.minus1 <- which(subdata$speaker != "CHI" &
                    (subdata$val == "T") &
                    subdata$stop <= tm1.stop &
                    subdata$stop >= tm1.start)
    if(length(t.plus1) > 0) {
      tp1.match <- 0
      for (turn in t.plus1) {
        # Each OTH turn can only be a response once 
        if (!(turn %in% used.tp1s) & tp1.match == 0) {
          used.tp1s <- c(used.tp1s, turn)
          chi.turn.info$tp1.tier[j] <- subdata$tier[turn]
          chi.turn.info$tp1.speaker[j] <- subdata$speaker[turn]
          chi.turn.info$tp1.start[j] <- subdata$start[turn]
          chi.turn.info$tp1.stop[j] <- subdata$stop[turn]
          chi.turn.info$tp1.val[j] <- subdata$val[turn]
          tp1.match <- 1
        }
      }
    }
    if(length(t.minus1) > 0) {
      tm1.match <- 0
      for (turn in t.minus1) {
        # Each OTH turn can only be a prompt once 
        if (!(turn %in% used.tm1s) & tm1.match == 0) {
          used.tm1s <- c(used.tm1s, turn)
          chi.turn.info$tm1.tier[j] <- subdata$tier[turn]
          chi.turn.info$tm1.speaker[j] <- subdata$speaker[turn]
          chi.turn.info$tm1.start[j] <- subdata$start[turn]
          chi.turn.info$tm1.stop[j] <- subdata$stop[turn]
          chi.turn.info$tm1.val[j] <- subdata$val[turn]
          tm1.match <- 1
        }
      }
    }
  }
  turn.transitions <- bind_rows(turn.transitions, chi.turn.info)
}

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
