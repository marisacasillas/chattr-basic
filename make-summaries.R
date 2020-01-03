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
