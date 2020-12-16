summarize_chattr <- function(tttbl.list) {
  # Real data summary
  if (nrow(tttbl.list$real.tt.vals) > 0) {
    tttbl.real<- summarize_tttbl(tttbl.list$real.tt.vals, "real data")    
  } else {
    tttbl.real <- tibble()
  }
  # Random data summary
  if (nrow(tttbl.list$random.tt.vals) > 0) {
    tttbl.rand<- summarize_tttbl(tttbl.list$random.tt.vals, "random data")    
  } else {
    tttbl.rand <- tibble()
  }
  chattr.summary <- bind_rows(tttbl.real, tttbl.rand)
  return(chattr.summary)
}

summarize_tttbl <- function(tttbl, data.type) {
  tttbl <- tttbl %>%
    mutate(
      clip.start.msec = as.numeric(unlist(str_split(annot.clip, "[-_]"))[2]),
      clip.end.msec = as.numeric(unlist(str_split(annot.clip, "[-_]"))[3]),
      clip.duration.msec = clip.end.msec - clip.start.msec,
      clip.duration.min = clip.duration.msec/60000,
      pmt = ifelse(is.na(prompt.spkr), 0, 1),
      pmt.timing = start.ms - prompt.stop.ms,
      rsp = ifelse(is.na(response.spkr), 0, 1),
      rsp.timing = response.start.ms - stop.ms
    )
  tt.timings <- c(tttbl$pmt.timing, tttbl$rsp.timing)
  tttbl.summary <- tttbl %>%
    group_by(annot.clip, clip.start.msec, clip.end.msec,
      clip.duration.msec, clip.duration.min) %>%
    summarize(
      `.groups` = "drop",
      num.prompts = sum(pmt),
      num.responses = sum(rsp),
      num.transitions = num.prompts + num.responses,
      prompt.latency.msec.mean = mean(pmt.timing, na.rm = TRUE),
      prompt.latency.msec.median = median(pmt.timing, na.rm = TRUE),
      prompt.latency.msec.min = min(pmt.timing, na.rm = TRUE),
      prompt.latency.msec.max = max(pmt.timing, na.rm = TRUE),
      prompt.latency.msec.sd = sd(pmt.timing, na.rm = TRUE),
      response.latency.msec.mean = mean(rsp.timing, na.rm = TRUE),
      response.latency.msec.median = median(rsp.timing, na.rm = TRUE),
      response.latency.msec.min = min(rsp.timing, na.rm = TRUE),
      response.latency.msec.max = max(rsp.timing, na.rm = TRUE),
      response.latency.msec.sd = sd(rsp.timing, na.rm = TRUE)) %>%
    mutate(
      prompts.per.min = num.prompts/clip.duration.min,
      responses.per.min = num.responses/clip.duration.min,
      transitions.per.min = num.transitions/clip.duration.min,
      transition.latency.msec.mean = mean(tt.timings, na.rm = TRUE),
      transition.latency.msec.median = median(tt.timings, na.rm = TRUE),
      transition.latency.msec.min = min(tt.timings, na.rm = TRUE),
      transition.latency.msec.max = max(tt.timings, na.rm = TRUE),
      transition.latency.msec.sd = sd(tt.timings, na.rm = TRUE),
      data.type = data.type) %>%
    select(
      data.type, annot.clip, clip.start.msec, clip.duration.msec,
      num.prompts, prompts.per.min,
      prompt.latency.msec.mean, prompt.latency.msec.median,
      prompt.latency.msec.min, prompt.latency.msec.max,
      prompt.latency.msec.sd,
      num.responses, responses.per.min,
      response.latency.msec.mean, response.latency.msec.median,
      response.latency.msec.min, response.latency.msec.max,
      response.latency.msec.sd,
      num.transitions, transitions.per.min,
      transition.latency.msec.mean, transition.latency.msec.median,
      transition.latency.msec.min, transition.latency.msec.max,
      transition.latency.msec.sd)
  # check for intseqs before summarizing them
  # TO DO
  return(tttbl.summary)
}


# ## OLD: create summaries
# turn.transitions.overview.o_c <- turn.transitions %>%
#   filter(!(is.na(tm1.val))) %>%
#   group_by(aclew_child_id, segment) %>%
#   summarise(n.o_c.tts = n()) %>%
#   full_join(all.segments, by = c("aclew_child_id", "segment")) %>%
#   replace_na(list(n.o_c.tts = 0)) %>%
#   arrange(aclew_child_id, segment) %>%
#   mutate(duration = ifelse(grepl('extension', sample_type), 5,
#                            ifelse(grepl('random', sample_type), 5, 1)))
# 
# turn.transitions.overview.c_o <- turn.transitions %>%
#   filter(!(is.na(tp1.val))) %>%
#   group_by(aclew_child_id, segment) %>%
#   summarise(n.c_o.tts = n()) %>%
#   full_join(all.segments, by = c("aclew_child_id", "segment")) %>%
#   replace_na(list(n.c_o.tts = 0)) %>%
#   arrange(aclew_child_id, segment) %>%
#   mutate(duration = ifelse(grepl('extension', sample_type), 5,
#                            ifelse(grepl('random', sample_type), 5, 1)))
# 
# # Combine the turn-taking info into one table
# turn.transitions.overview <- turn.transitions.overview.o_c %>%
#   full_join(dplyr::select(turn.transitions.overview.c_o,
#                           c("aclew_child_id", "segment", "n.c_o.tts")),
#             by = c("aclew_child_id", "segment")) %>%
#   mutate(n.o_c.tpm = n.o_c.tts/duration,
#          n.c_o.tpm = n.c_o.tts/duration) %>%
#   left_join(ptcp.info, by = "aclew_child_id") %>%
#   left_join(spkrs.per.seg.all, by = c("aclew_child_id", "segment",
#                                       "sample", "sample_type")) %>%
#   left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
#             by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName"))
# 
# # Summaries of turn taking by sample
# turn.taking.by.sample <- turn.transitions.overview %>%
#   group_by(sample) %>%
#   summarise(mean.n.o_c.tpm = mean(n.o_c.tpm),
#          median.n.o_c.tpm = median(n.o_c.tpm),
#          min.n.o_c.tpm = min(n.o_c.tpm),
#          max.n.o_c.tpm = max(n.o_c.tpm),
#          mean.n.c_o.tpm = mean(n.c_o.tpm),
#          median.n.c_o.tpm = median(n.c_o.tpm),
#          min.n.c_o.tpm = min(n.c_o.tpm),
#          max.n.c_o.tpm = max(n.c_o.tpm))
# turn.taking.by.child.bysample <- turn.transitions.overview %>%
#   group_by(aclew_child_id, sample) %>%
#   summarise(mean.n.o_c.tpm = mean(n.o_c.tpm),
#          median.n.o_c.tpm = median(n.o_c.tpm),
#          min.n.o_c.tpm = min(n.o_c.tpm),
#          max.n.o_c.tpm = max(n.o_c.tpm),
#          mean.n.c_o.tpm = mean(n.c_o.tpm),
#          median.n.c_o.tpm = median(n.c_o.tpm),
#          min.n.c_o.tpm = min(n.c_o.tpm),
#          max.n.c_o.tpm = max(n.c_o.tpm))

# # Sequence duration summary
# turn.sequences <- turn.sequences %>%
#   filter(!(is.na(seq.num))) %>%
#   mutate(seq.dur = (seq.stop - seq.start)/60000)
# turn.sequences.overview <- turn.sequences %>%
#   group_by(aclew_child_id, sample, segment, seq.num, seq.start, seq.stop, seq.dur) %>%
#   summarise(n_cvcs_seq = n()) %>%
#   left_join(ptcp.info, by = "aclew_child_id") %>%
#   left_join(spkrs.per.seg.all, by = c("aclew_child_id", "segment", "sample")) %>%
#   left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
#     by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName"))
# turn.seq.by.sample <- turn.sequences.overview %>%
#   group_by(sample) %>%
#   summarise(mean.tsq_dur = mean(seq.dur),
#     median.tsq_dur = median(seq.dur),
#     min.tsq_dur = min(seq.dur),
#     max.tsq_dur = max(seq.dur),
#     mean.cvcs = mean(n_cvcs_seq),
#     median.cvcs = median(n_cvcs_seq),
#     min.cvcs = min(n_cvcs_seq),
#     max.cvcs = max(n_cvcs_seq))
# turn.seq.by.sample.by.child <- turn.sequences.overview %>%
#   group_by(aclew_child_id, sample) %>%
#   summarise(mean.tsq_dur = mean(seq.dur),
#     median.tsq_dur = median(seq.dur),
#     min.tsq_dur = min(seq.dur),
#     max.tsq_dur = max(seq.dur),
#     mean.cvcs = mean(n_cvcs_seq),
#     median.cvcs = median(n_cvcs_seq),
#     min.cvcs = min(n_cvcs_seq),
#     max.cvcs = max(n_cvcs_seq))
