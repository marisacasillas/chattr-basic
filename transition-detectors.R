library(tidyverse)

options(scipen=999)
data.path <- "transcripts/anon/" # text files exported from ELAN
plot.path <- "plots/" # output plots
seg.index.file <- "Segment-order-inventory.csv"
ptcp.info.file <- "recording-info.csv"
comparison.file <- "comparison_studies.csv"
samplelabels <- c("High activity  ", "Random  ")
col.sample.bu <- list(
  scale_fill_manual(labels=samplelabels, values=viridis(2)),
  scale_color_manual(labels=samplelabels, values=viridis(2)))
col.sample.bu3 <- list(
  scale_fill_manual(labels=samplelabels, values=viridis(3)),
  scale_color_manual(labels=samplelabels, values=viridis(3)))
allowed.overlap <- 1000 #ms
allowed.gap <- 2000 #ms
text.col.figs <- "black"

# Read in annotation files
files <- list.files(path=data.path,pattern="*.txt")
all.data <- data.frame()
for (i in 1:length(files)) {
#  print(files[i])
  newfile <- read_csv(paste0(data.path, files[i]), col_types = cols(val = col_character()))
  newfile$aclew_child_id <- unlist(strsplit(files[i], '\\.'))[1]
  all.data <- rbind(all.data, newfile)
}
all.data$row <- c(1:nrow(all.data))

# Read in supplementary data
ptcp.info <- read_csv(ptcp.info.file, col_types = cols()) %>%
  dplyr::select(-row)
seg.info <- read_csv(seg.index.file)

# Extract and convert start time of each sample
seg.info$start.hhmmss <- regmatches(seg.info$Media,
                                    regexpr("[[:digit:]]{6}", seg.info$Media))
seg.info$start.sec <- as.numeric(substr(seg.info$start.hhmmss,1,2))*3600 +
  as.numeric(substr(seg.info$start.hhmmss,3,4))*60 +
  as.numeric(substr(seg.info$start.hhmmss,5,6))
seg.info$start.hr <- round(seg.info$start.sec/3600, 3)

seg.info$clipoffset.hhmmss <- regmatches(seg.info$Media,
                                    regexpr("(?<=[[:digit:]]{6}_)[[:digit:]]{6}",
                                            seg.info$Media, perl = TRUE))
seg.info$clipoffset.sec <- as.numeric(substr(seg.info$clipoffset.hhmmss,1,2))*3600 +
  as.numeric(substr(seg.info$clipoffset.hhmmss,3,4))*60 +
  as.numeric(substr(seg.info$clipoffset.hhmmss,5,6))
seg.info$clipoffset.hr <- round(seg.info$clipoffset.sec/3600, 3)

# Add mean and sd values for participant-level predictors to ptcp.info
ptcp.info <- ptcp.info %>%
  mutate(
    tchiyr.m = mean(age_mo_round),
    motyr.m = mean(mother_age),
    nsb.m = mean(number_older_sibs),
    hsz.m = mean(household_size),
    tchiyr.sd = sd(age_mo_round),
    motyr.sd = sd(mother_age),
    nsb.sd = sd(number_older_sibs),
    hsz.sd = sd(household_size)
    )

# Merge in participant and segment info to the main data table
codes <- all.data %>% filter(tier == "code")

all.data <- all.data %>%
  filter(speaker != "") %>%
  left_join(ptcp.info, by = "aclew_child_id") %>%
  mutate(segment = "", sample = "",
         sample_type = "", segment_dur = 0)

for (i in 1:nrow(codes)) {
  rec <- codes$aclew_child_id[i]
  seg <- as.character(codes$val[i])
  seg.on <- codes$start[i]
  seg.off <- codes$stop[i]
  seg.idx <- which(all.data$aclew_child_id == rec &
                     all.data$start < seg.off &
                     all.data$stop > seg.on)
  all.data$segment[seg.idx] <- seg
}

# Label samples
all.data$sample[which(
  grepl('^random', all.data$segment))] <- "random"
all.data$sample[which(
  grepl('tt', all.data$segment))] <- "turn-taking"
all.data$sample[which(
  grepl('va', all.data$segment))] <- "high-activity"

# Label sample types and durations
random.samples <- which(grepl('^random', all.data$segment))
all.data$sample_type[random.samples] <- "random"
all.data$segment_dur[random.samples] <- 5

ext.samples <- which(grepl('^extension', all.data$segment))
all.data$sample_type[ext.samples] <- "extension"
all.data$segment_dur[ext.samples] <- 5

tt.samples <- which(grepl('^tt', all.data$segment))
all.data$sample_type[tt.samples] <- "turn-taking"
all.data$segment_dur[tt.samples] <- 1

va.samples <- which(grepl('^va', all.data$segment))
all.data$sample_type[va.samples] <- "turn-taking"
all.data$segment_dur[va.samples] <- 1

# Add in segment start time
all.data <- all.data %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hhmmss",
                                      "start.sec", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName"))

avg.utt.len.tseltal <- all.data %>%
  filter(speaker != "CHI") %>%
  dplyr::select(dur) %>%
  summarise(mean.utt.len = mean(dur),
            median.utt.len = median(dur))

sample.demog <- ptcp.info %>%
  mutate(mat_age_rd = as.integer(round(mother_age, 0)),
         fat_age_rd = as.integer(round(father_age, 0)),
         agev2 = gsub("[[:alpha:]]", "", age),
         hszrd = as.integer(round(household_size + 1, 0))) %>%
  select(homebank_child_id, agev2, child_sex, mat_age_rd, mat_ed, hszrd) %>%
#  select(homebank_child_id, agev2, child_sex, mat_age_rd, mat_ed, fat_age_rd, fat_ed, hszrd) %>%
  rename("HB ID" = homebank_child_id, "Age" = agev2, "Sex" = child_sex,
         "Mot age" = mat_age_rd, "Mot edu" = mat_ed,
#         "Fat age" = father_age, "Fat edu" = fat_ed,
         "Ppl in house" = hszrd)

ptcp.info <- mutate(ptcp.info,
                    rec.start.hr = lubridate::hour(start_of_recording) +
                      lubridate::minute(start_of_recording)/60 +
                      lubridate::second(start_of_recording)/3600,
                    rec.stop.hr = rec.start.hr + length_of_recording/3600) %>%
  arrange(age_mo_round) %>%
  mutate(order = seq(1:10))

used.clips <- seg.info %>%
  filter(Include == 1) %>%
  left_join(ptcp.info, by = c("aclew_id" = "aclew_child_id")) %>%
  mutate(clip.dur = ifelse(grepl(('random'), CodeName), 5,
                           ifelse(grepl('extension', CodeName), 6, 1)),
         sample.type = ifelse(grepl(('random'), CodeName), "Random",
                           ifelse(grepl('tt', CodeName), "Turn taking", "High activity")))
used.clips$sample.type = factor(used.clips$sample.type, levels = c("Random", "Turn taking", "High activity"))
  

clip.distribution.1 <- ggplot() +
  geom_segment(data = ptcp.info,
               aes(x = rec.start.hr, y = order, xend = rec.stop.hr, yend = order), color = "black") +
  theme_apa() +
  scale_x_continuous(breaks = 7:21) +
  scale_y_continuous(breaks = 1:10, labels = ptcp.info$age_mo_round) +
  ylab("Child age (mo)") + xlab("Time of day (hr)") + labs(color = "Sample type") +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		legend.position = "none")

clip.distribution.2 <- ggplot() +
  geom_segment(data = ptcp.info,
               aes(x = rec.start.hr, y = order, xend = rec.stop.hr, yend = order), color = "black") +
  geom_segment(data = subset(used.clips, sample.type == "Random"),
               aes(x = start.hr, y = order, xend = start.hr + clip.dur/60, yend = order,
                   color = sample.type), size = 10) +
  theme_apa() +
  scale_x_continuous(breaks = 7:21) +
  scale_y_continuous(breaks = 1:10, labels = ptcp.info$age_mo_round) +
  ylab("Child age (mo)") + xlab("Time of day (hr)") + labs(color = "Sample type") +
  scale_color_manual(values = c("black")) +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		legend.position = "none")

clip.distribution.3 <- ggplot() +
  geom_segment(data = ptcp.info,
               aes(x = rec.start.hr, y = order, xend = rec.stop.hr, yend = order), color = "black") +
  geom_segment(data = subset(used.clips, sample.type != "High activity"),
               aes(x = start.hr, y = order, xend = start.hr + clip.dur/60, yend = order,
                   color = sample.type), size = 10) +
  theme_apa() +
  scale_x_continuous(breaks = 7:21) +
  scale_y_continuous(breaks = 1:10, labels = ptcp.info$age_mo_round) +
  ylab("Child age (mo)") + xlab("Time of day (hr)") + labs(color = "Sample type") +
  scale_color_manual(values = c("black", "red")) +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		legend.position = "none")

clip.distribution.4 <- ggplot() +
  geom_segment(data = ptcp.info,
               aes(x = rec.start.hr, y = order, xend = rec.stop.hr, yend = order), color = "black") +
  geom_segment(data = used.clips,
               aes(x = start.hr, y = order, xend = start.hr + clip.dur/60, yend = order,
                   color = sample.type), size = 10) +
  theme_apa() +
  scale_x_continuous(breaks = 7:21) +
  scale_y_continuous(breaks = 1:10, labels = ptcp.info$age_mo_round) +
  ylab("Child age (mo)") + xlab("Time of day (hr)") + labs(color = "Sample type") +
  scale_color_manual(values = c("black", "red", "blue")) +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		legend.position = "none")

png(paste("diff_figs/","clip.distribution.1.png", sep=""),
    width=1200, height=600,units="px", bg = "transparent")
print(clip.distribution.1)
dev.off()
png(paste("diff_figs/","clip.distribution.2.png", sep=""),
    width=1200, height=600,units="px", bg = "transparent")
print(clip.distribution.2)
dev.off()
png(paste("diff_figs/","clip.distribution.3.png", sep=""),
    width=1200, height=600,units="px", bg = "transparent")
print(clip.distribution.3)
dev.off()
png(paste("diff_figs/","clip.distribution.4.png", sep=""),
    width=1200, height=600,units="px", bg = "transparent")
print(clip.distribution.4)
dev.off()


# RANDOM
# Get min/hr speech measures
n.unique.rand.segs <- length(unique(all.data$segment[grepl("random", all.data$segment)]))
n.unique.recs <- length(unique(all.data$aclew_child_id))
all.rand.segments <- tibble(
  aclew_child_id = rep(unique(all.data$aclew_child_id),
                n.unique.rand.segs),
  segment = rep(unique(all.data$segment[grepl("random", all.data$segment)]),
                n.unique.recs))
# XDS
xds.per.seg.rand <- all.data %>%
  filter(sample == "random" & speaker != "CHI" &
           grepl("xds@", tier)) %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(xds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 5, xds_min = 0)) %>%
  mutate(xds_mph = (xds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)
# ODS
ods.per.seg.rand <- all.data %>%
  filter(sample == "random" & speaker != "CHI" &
           grepl("xds@", tier) & val != "T") %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(ods_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 5, ods_min = 0)) %>%
  mutate(ods_mph = (ods_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)
# TDS
tds.per.seg.rand <- all.data %>%
  filter(sample == "random" & speaker != "CHI" &
           grepl("xds@", tier) & val == "T") %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(tds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 5, tds_min = 0)) %>%
  mutate(tds_mph = (tds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)
# All CDS
cds.per.seg.rand <- all.data %>%
  filter(sample == "random" & speaker != "CHI" &
           grepl("xds@", tier) & (val == "T" | val == "C")) %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(cds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 5, cds_min = 0)) %>%
  mutate(cds_mph = (cds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)
# Number of speakers per clip
spkrs.per.seg.rand <- all.data %>%
  filter(sample == "random" & speaker != "CHI" &
           !(grepl("@", tier))) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(n_spkrs_clip = length(unique(speaker)))
# All together
quantity.rand <- xds.per.seg.rand %>%
  full_join(ods.per.seg.rand, by = c("aclew_child_id", "segment", "segment_dur")) %>%
  full_join(tds.per.seg.rand, by = c("aclew_child_id", "segment", "segment_dur")) %>%
  full_join(cds.per.seg.rand, by = c("aclew_child_id", "segment", "segment_dur")) %>%
  full_join(spkrs.per.seg.rand, by = c("aclew_child_id", "segment")) %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName")) %>%
  full_join(ptcp.info, by = "aclew_child_id") %>%
  replace_na(list(xds_min = 0, xds_mph = 0,
                  tds_min = 0, tds_mph = 0,
                  cds_min = 0, cds_mph = 0,
                  n_spkrs_clip = 0)) %>%
  mutate(prop_tds = tds_min/xds_min)
  # Don't replace NAs with 0s in this case; proportion is not meaningful w/o any speech
quantity.rand.bychild <- quantity.rand %>%
  group_by(aclew_child_id) %>%
  summarise(
    xds_min = mean(xds_min),
    xds_mph = mean(xds_mph),
    ods_min = mean(ods_min),
    ods_mph = mean(ods_mph),
    tds_min = mean(tds_min),
    tds_mph = mean(tds_mph),
    cds_min = mean(cds_min),
    cds_mph = mean(cds_mph),
    prop_tds = mean(prop_tds, na.rm = TRUE),
    m_n_spkrs = mean(n_spkrs_clip)) %>%
  full_join(ptcp.info, by = "aclew_child_id")

# Get xds and tds min/hr by speaker type
all.data$SpkrAge <- "Not known"
all.data$SpkrAge[grepl("FA|MA|UA", all.data$speaker)] <- "Adult"
all.data$SpkrAge[grepl("FC|MC|UC", all.data$speaker)] <- "Child"
all.rand.segments.sa <- tibble(
  aclew_child_id = rep(unique(all.data$aclew_child_id),
                2*n.unique.rand.segs),
  segment = rep(unique(all.data$segment[grepl("random", all.data$segment)]),
                2*n.unique.recs),
  SpkrAge = c(rep("Adult", (n.unique.rand.segs * n.unique.recs)),
              rep("Child", (n.unique.rand.segs * n.unique.recs))))
# XDS
xds.per.seg.rand.sa <- all.data %>%
  filter(sample == "random" & speaker != "CHI" & SpkrAge != "Not known" &
           grepl("xds@", tier)) %>%
  group_by(aclew_child_id, SpkrAge, segment, segment_dur) %>%
  summarise(xds_min.sa = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments.sa, by = c("aclew_child_id", "segment", "SpkrAge")) %>%
  replace_na(list(segment_dur = 5, xds_min.sa = 0)) %>%
  mutate(xds_mph.sa = (xds_min.sa/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrAge)
# ODS
ods.per.seg.rand.sa <- all.data %>%
  filter(sample == "random" & speaker != "CHI" & SpkrAge != "Not known" &
           grepl("xds@", tier) & val != "T") %>%
  group_by(aclew_child_id, SpkrAge, segment, segment_dur) %>%
  summarise(ods_min.sa = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments.sa, by = c("aclew_child_id", "segment", "SpkrAge")) %>%
  replace_na(list(segment_dur = 5, ods_min.sa = 0)) %>%
  mutate(ods_mph.sa = (ods_min.sa/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrAge)
# TDS
tds.per.seg.rand.sa <- all.data %>%
  filter(sample == "random" & speaker != "CHI" & SpkrAge != "Not known" &
           grepl("xds@", tier) & val == "T") %>%
  group_by(aclew_child_id, SpkrAge, segment, segment_dur) %>%
  summarise(tds_min.sa = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments.sa, by = c("aclew_child_id", "segment", "SpkrAge")) %>%
  replace_na(list(segment_dur = 5, tds_min.sa = 0)) %>%
  mutate(tds_mph.sa = (tds_min.sa/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrAge)
# All CDS
cds.per.seg.rand.sa <- all.data %>%
  filter(sample == "random" & speaker != "CHI" & SpkrAge != "Not known" &
           grepl("xds@", tier) & (val == "T" | val == "C")) %>%
  group_by(aclew_child_id, SpkrAge, segment, segment_dur) %>%
  summarise(cds_min.sa = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments.sa, by = c("aclew_child_id", "segment", "SpkrAge")) %>%
  replace_na(list(segment_dur = 5, cds_min.sa = 0)) %>%
  mutate(cds_mph.sa = (cds_min.sa/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrAge)
# Number of speakers per clip
spkrs.per.seg.rand.sa <- all.data %>%
  filter(sample == "random" & speaker != "CHI" & SpkrAge != "Not known" &
           !(grepl("@", tier))) %>%
  group_by(aclew_child_id, SpkrAge, segment) %>%
  summarise(n_spkrs_clip = length(unique(speaker)))
# All together
quantity.rand.sa <- xds.per.seg.rand.sa %>%
  full_join(ods.per.seg.rand.sa, by = c("aclew_child_id", "SpkrAge",
                                        "segment", "segment_dur")) %>%
  full_join(tds.per.seg.rand.sa, by = c("aclew_child_id", "SpkrAge",
                                        "segment", "segment_dur")) %>%
  full_join(cds.per.seg.rand.sa, by = c("aclew_child_id", "SpkrAge",
                                        "segment", "segment_dur")) %>%
  full_join(dplyr::select(quantity.rand, c("aclew_child_id", "segment", "tds_min")),
            by = c("aclew_child_id", "segment")) %>%
  full_join(spkrs.per.seg.rand.sa, by = c("aclew_child_id", "SpkrAge", "segment")) %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName")) %>%
  full_join(ptcp.info, by = "aclew_child_id") %>%
  replace_na(list(xds_min.sa = 0, xds_mph.sa = 0,
                  ods_min.sa = 0, ods_mph.sa = 0,
                  tds_min.sa = 0, tds_mph.sa = 0,
                  cds_min.sa = 0, cds_mph.sa = 0,
                  n_spkrs_clip = 0)) %>%
  mutate(prop_tds.sa = tds_min.sa/xds_min.sa,
         prop_sa.tds = tds_min.sa/tds_min)
  # Don't replace NAs with 0s in this case; proportion is not meaningful w/o any speech


# NON-RANDOM
# Get min/hr speech measures
all.nonrand.segments <- seg.info %>%
  filter(!(grepl("random", CodeName))) %>%
  select(aclew_id, CodeName) %>%
  rename(aclew_child_id = aclew_id, segment = CodeName)
all.nonrand.segments$sample <- ifelse(grepl("va", all.nonrand.segments$segment),
                                      "high-activity","turn-taking")
all.nonrand.segments$sample_type <- ifelse(grepl("^va", all.nonrand.segments$segment),
                                      "high-activity", ifelse(
                                        grepl("^tt", all.nonrand.segments$segment),"turn-taking",
                                        "extension"))
# XDS
xds.per.seg.nonrand <- all.data %>%
  filter(sample != "random" & speaker != "CHI" &
           grepl("xds@", tier)) %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(xds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 1, xds_min = 0)) %>%
  mutate(segment_dur = ifelse(grepl("ext", segment), 5, 1),
         xds_mph = (xds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)
# ODS
ods.per.seg.nonrand <- all.data %>%
  filter(sample != "random" & speaker != "CHI" &
           grepl("xds@", tier) & val != "T") %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(ods_min = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 1, ods_min = 0)) %>%
  mutate(segment_dur = ifelse(grepl("ext", segment), 5, 1),
         ods_mph = (ods_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)
# TDS
tds.per.seg.nonrand <- all.data %>%
  filter(sample != "random" & speaker != "CHI" &
           grepl("xds@", tier) & val == "T") %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(tds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 1, tds_min = 0)) %>%
  mutate(segment_dur = ifelse(grepl("ext", segment), 5, 1),
         tds_mph = (tds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)
# All CDS
cds.per.seg.nonrand <- all.data %>%
  filter(sample != "random" & speaker != "CHI" &
           grepl("xds@", tier) & (val == "T" | val == "C")) %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(cds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 1, cds_min = 0)) %>%
  mutate(segment_dur = ifelse(grepl("ext", segment), 5, 1),
         cds_mph = (cds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)
# Number of speakers per clip
spkrs.per.seg.nonrand <- all.data %>%
  filter(sample != "random" & speaker != "CHI" & SpkrAge != "Not known" &
           !(grepl("@", tier))) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(n_spkrs_clip = length(unique(speaker)))
# All together
quantity.nonrand <- xds.per.seg.nonrand %>%
  full_join(ods.per.seg.nonrand, by = c("aclew_child_id", "segment",
                                        "segment_dur", "sample", "sample_type")) %>%
  full_join(tds.per.seg.nonrand, by = c("aclew_child_id", "segment",
                                        "segment_dur", "sample", "sample_type")) %>%
  full_join(cds.per.seg.nonrand, by = c("aclew_child_id", "segment",
                                        "segment_dur", "sample", "sample_type")) %>%
  full_join(ptcp.info, by = "aclew_child_id") %>%
  full_join(spkrs.per.seg.nonrand, by = c("aclew_child_id", "segment")) %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName")) %>%
  replace_na(list(xds_min = 0, xds_mph = 0,
                  ods_min = 0, ods_mph = 0,
                  tds_min = 0, tds_mph = 0,
                  cds_min = 0, cds_mph = 0,
                  n_spkrs_clip = 0)) %>%
  mutate(prop_tds = tds_min/xds_min)
  # Don't replace NAs with 0s in this case; proportion is not meaningful w/o any speech
quantity.nonrand.bychild <- quantity.nonrand %>%
  group_by(aclew_child_id, sample) %>%
  summarise(
    xds_min = mean(xds_min),
    xds_mph = mean(xds_mph),
    ods_min = mean(ods_min),
    ods_mph = mean(ods_mph),
    tds_min = mean(tds_min),
    tds_mph = mean(tds_mph),
    cds_min = mean(cds_min),
    cds_mph = mean(cds_mph),
    prop_tds = mean(prop_tds, na.rm = TRUE),
    m_n_spkrs = mean(n_spkrs_clip)) %>%
  full_join(ptcp.info, by = "aclew_child_id")

# Get xds and tds min/hr by speaker type
all.nonrand.segments.sa <- tibble(
  aclew_child_id = rep(all.nonrand.segments$aclew_child_id, 2),
  segment = rep(all.nonrand.segments$segment, 2),
  sample = rep(all.nonrand.segments$sample, 2),
  sample_type = rep(all.nonrand.segments$sample_type, 2),
  SpkrAge = c(rep("Adult", nrow(all.nonrand.segments)),
              rep("Child", nrow(all.nonrand.segments))))
# XDS
xds.per.seg.nonrand.sa <- all.data %>%
  filter(sample != "random" & speaker != "CHI" & SpkrAge != "Not known" &
           grepl("xds@", tier)) %>%
  group_by(aclew_child_id, SpkrAge, segment, segment_dur) %>%
  summarise(xds_min.sa = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments.sa, by = c("aclew_child_id", "segment", "SpkrAge")) %>%
  replace_na(list(segment_dur = 1, xds_min.sa = 0)) %>%
  mutate(segment_dur = ifelse(grepl("ext", segment), 5, 1),
         xds_mph.sa = (xds_min.sa/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrAge)
# ODS
ods.per.seg.nonrand.sa <- all.data %>%
  filter(sample != "random" & speaker != "CHI" & SpkrAge != "Not known" &
           grepl("xds@", tier) & val != "T") %>%
  group_by(aclew_child_id, SpkrAge, segment, segment_dur) %>%
  summarise(ods_min.sa = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments.sa, by = c("aclew_child_id", "segment", "SpkrAge")) %>%
  replace_na(list(segment_dur = 1, ods_min.sa = 0)) %>%
  mutate(segment_dur = ifelse(grepl("ext", segment), 5, 1),
         ods_mph.sa = (ods_min.sa/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrAge)
# TDS
tds.per.seg.nonrand.sa <- all.data %>%
  filter(sample != "random" & speaker != "CHI" & SpkrAge != "Not known" &
           grepl("xds@", tier) & val == "T") %>%
  group_by(aclew_child_id, SpkrAge, segment, segment_dur) %>%
  summarise(tds_min.sa = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments.sa, by = c("aclew_child_id", "segment", "SpkrAge")) %>%
  replace_na(list(segment_dur = 1, tds_min.sa = 0)) %>%
  mutate(segment_dur = ifelse(grepl("ext", segment), 5, 1),
         tds_mph.sa = (tds_min.sa/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrAge)
# All CDS
cds.per.seg.nonrand.sa <- all.data %>%
  filter(sample != "random" & speaker != "CHI" & SpkrAge != "Not known" &
           grepl("xds@", tier) & (val == "T" | val == "C")) %>%
  group_by(aclew_child_id, SpkrAge, segment, segment_dur) %>%
  summarise(cds_min.sa = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments.sa, by = c("aclew_child_id", "segment", "SpkrAge")) %>%
  replace_na(list(segment_dur = 1, cds_min.sa = 0)) %>%
  mutate(segment_dur = ifelse(grepl("ext", segment), 5, 1),
         cds_mph.sa = (cds_min.sa/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrAge)
# Number of speakers per clip
spkrs.per.seg.nonrand.sa <- all.data %>%
  filter(sample != "random" & speaker != "CHI" & SpkrAge != "Not known" &
           !(grepl("@", tier))) %>%
  group_by(aclew_child_id, SpkrAge, segment) %>%
  summarise(n_spkrs_clip = length(unique(speaker)))
# All together
quantity.nonrand.sa <- xds.per.seg.nonrand.sa %>%
  full_join(ods.per.seg.nonrand.sa, by = c("aclew_child_id", "SpkrAge",
                                           "segment", "segment_dur",
                                           "sample", "sample_type")) %>%
  full_join(tds.per.seg.nonrand.sa, by = c("aclew_child_id", "SpkrAge",
                                           "segment", "segment_dur",
                                           "sample", "sample_type")) %>%
  full_join(cds.per.seg.nonrand.sa, by = c("aclew_child_id", "SpkrAge",
                                           "segment", "segment_dur",
                                           "sample", "sample_type")) %>%
  full_join(select(quantity.nonrand, c("aclew_child_id", "segment", "tds_min")),
            by = c("aclew_child_id", "segment")) %>%
  full_join(ptcp.info, by = "aclew_child_id") %>%
  full_join(spkrs.per.seg.nonrand.sa, by = c("aclew_child_id", "SpkrAge", "segment")) %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName")) %>%
  replace_na(list(xds_min.sa = 0, xds_mph.sa = 0,
                  ods_min.sa = 0, ods_mph.sa = 0,
                  tds_min.sa = 0, tds_mph.sa = 0,
                  cds_min.sa = 0, cds_mph.sa = 0,
                  n_spkrs_clip = 0)) %>%
  mutate(prop_tds.sa = tds_min.sa/xds_min.sa,
         prop_sa.tds = tds_min.sa/tds_min)
  # Don't replace NAs with 0s in this case; proportion is not meaningful w/o any speech

# Subset the non-random samples (used for differnt purposes)
quantity.nonrand.tt <- filter(quantity.nonrand, sample == "turn-taking")
quantity.nonrand.tt.sa <- filter(quantity.nonrand.sa, sample == "turn-taking")
quantity.nonrand.va <- filter(quantity.nonrand, sample != "turn-taking")
quantity.nonrand.va.sa <- filter(quantity.nonrand.sa, sample != "turn-taking")


## Get variables ready for modeling
# random sample
quantity.rand$child_sex <- as.factor(quantity.rand$child_sex)
quantity.rand$mat_ed <- as.factor(quantity.rand$mat_ed)
nspkrs.m <- mean(quantity.rand$n_spkrs_clip)
nspkrs.sd <- sd(quantity.rand$n_spkrs_clip)
quantity.rand <- quantity.rand %>%
  mutate(
    xds_mph.nz = ifelse(xds_mph > 0, 1, 0),
    ods_mph.nz = ifelse(ods_mph > 0, 1, 0),
    tds_mph.nz = ifelse(tds_mph > 0, 1, 0),
    cds_mph.nz = ifelse(cds_mph > 0, 1, 0),
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.m)/nspkrs.sd),
    stthr.std = (start.hr - 12)/12)

quantity.rand.sa$child_sex <- as.factor(quantity.rand.sa$child_sex)
quantity.rand.sa$mat_ed <- as.factor(quantity.rand.sa$mat_ed)
nspkrs.sa.m <- mean(quantity.rand.sa$n_spkrs_clip)
nspkrs.sa.sd <- sd(quantity.rand.sa$n_spkrs_clip)
quantity.rand.sa <- quantity.rand.sa %>%
  mutate(
    xds_mph.sa.nz = ifelse(xds_mph.sa > 0, 1, 0),
    ods_mph.sa.nz = ifelse(ods_mph.sa > 0, 1, 0),
    tds_mph.sa.nz = ifelse(tds_mph.sa > 0, 1, 0),
    cds_mph.sa.nz = ifelse(cds_mph.sa > 0, 1, 0),
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.sa.m)/nspkrs.sa.sd),
    stthr.std = (start.hr - 12)/12)

# tt sample
quantity.nonrand.tt$child_sex <- as.factor(quantity.nonrand.tt$child_sex)
quantity.nonrand.tt$mat_ed <- as.factor(quantity.nonrand.tt$mat_ed)
nspkrs.m <- mean(quantity.nonrand.tt$n_spkrs_clip)
nspkrs.sd <- sd(quantity.nonrand.tt$n_spkrs_clip)
quantity.nonrand.tt <- quantity.nonrand.tt %>%
  mutate(
    xds_mph.nz = ifelse(xds_mph > 0, 1, 0),
    ods_mph.nz = ifelse(ods_mph > 0, 1, 0),
    tds_mph.nz = ifelse(tds_mph > 0, 1, 0),
    cds_mph.nz = ifelse(cds_mph > 0, 1, 0),
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.m)/nspkrs.sd),
    stthr.std = (start.hr - 12)/12)

quantity.nonrand.tt.sa$child_sex <- as.factor(quantity.nonrand.tt.sa$child_sex)
quantity.nonrand.tt.sa$mat_ed <- as.factor(quantity.nonrand.tt.sa$mat_ed)
nspkrs.sa.m <- mean(quantity.nonrand.tt.sa$n_spkrs_clip)
nspkrs.sa.sd <- sd(quantity.nonrand.tt.sa$n_spkrs_clip)
quantity.nonrand.tt.sa <- quantity.nonrand.tt.sa %>%
  mutate(
    xds_mph.sa.nz = ifelse(xds_mph.sa > 0, 1, 0),
    ods_mph.sa.nz = ifelse(ods_mph.sa > 0, 1, 0),
    tds_mph.sa.nz = ifelse(tds_mph.sa > 0, 1, 0),
    cds_mph.sa.nz = ifelse(cds_mph.sa > 0, 1, 0),
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.sa.m)/nspkrs.sa.sd),
    stthr.std = (start.hr - 12)/12)

quantity.nonrand.tt.minimum <- dplyr::select(quantity.nonrand.tt,
                                             age_mo_round, xds_mph, ods_mph, tds_mph,
                                             prop_tds, n_spkrs_clip) %>%
                                             mutate(Sample = "Turn taking")
quantity.rand.minimum <- dplyr::select(quantity.rand,
                                       age_mo_round, xds_mph, ods_mph, tds_mph,
                                       prop_tds, n_spkrs_clip) %>%
                                       mutate(Sample = "Random")
quantity.rand_and_tt <- bind_rows(quantity.nonrand.tt.minimum, quantity.rand.minimum)

quantity.nonrand.sa.tt.minimum <- dplyr::select(quantity.nonrand.tt.sa,
                                             age_mo_round, prop_sa.tds, SpkrAge) %>%
                                             mutate(Sample = "Turn taking")
quantity.rand.sa.minimum <- dplyr::select(quantity.rand.sa,
                                       age_mo_round, prop_sa.tds, SpkrAge) %>%
                                       mutate(Sample = "Random")
quantity.sa.rand_and_tt <- bind_rows(quantity.nonrand.sa.tt.minimum, quantity.rand.sa.minimum)

# ODS min/hr
odsmph.segments.rand_and_tt.1 <- ggplot(quantity.rand,
                          aes(x = age_mo_round, y = ods_mph)) +
  geom_boxplot(aes(group = age_mo_round), fill = "gray50", color = "black",
               outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "black", color = "black", method = "lm") +
  ylab("ODS (min/hr)") + xlab("Child age (mo)")	+
  scale_y_continuous(limits=c(-10,80),
                     breaks=seq(0,80,20)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,80),xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

odsmph.segments.rand_and_tt.2 <- ggplot(quantity.nonrand.tt,
                          aes(x = age_mo_round, y = ods_mph)) +
  geom_boxplot(aes(group = age_mo_round), fill = "pink", color = "red",
               outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "red", color = "red", method = "lm") +
  ylab("ODS (min/hr)") + xlab("Child age (mo)")	+
  scale_y_continuous(limits=c(-10,80),
                     breaks=seq(0,80,20)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,80),xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

png(paste("diff_figs/","ods.1.png", sep=""),
    width=500, height=400,units="px", bg = "transparent")
print(odsmph.segments.rand_and_tt.1)
dev.off()
png(paste("diff_figs/","ods.2.png", sep=""),
    width=500, height=400,units="px", bg = "transparent")
print(odsmph.segments.rand_and_tt.2)
dev.off()

# TDS min/hr - zoomed in
tdsmph.segments.rand_and_tt.zoomedin.1 <- ggplot(quantity.rand,
                          aes(x = age_mo_round, y = tds_mph)) +
  geom_boxplot(aes(group = age_mo_round), fill = "gray50", color = "black",
               outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "black", color = "black", method = "lm") +
  ylab("TCDS (min/hr)") + xlab("Child age (mo)")	+
  scale_y_continuous(limits=c(0,40),
                     breaks=seq(0,40,10)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,40),xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

tdsmph.segments.rand_and_tt.zoomedin.2 <- ggplot(quantity.nonrand.tt,
                          aes(x = age_mo_round, y = tds_mph)) +
  geom_boxplot(aes(group = age_mo_round), fill = "pink", color = "red",
               outlier.shape = NA,
               lty = "solid", alpha = 0.4) +
  geom_smooth(fill = "red", color = "red", method = "lm") +
  ylab("TCDS (min/hr)") + xlab("Child age (mo)")	+
  scale_y_continuous(limits=c(0,40),
                     breaks=seq(0,40,10)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,40),xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

png(paste("diff_figs/","tcds.1.png", sep=""),
    width=500, height=400,units="px", bg = "transparent")
print(tdsmph.segments.rand_and_tt.zoomedin.1)
dev.off()
png(paste("diff_figs/","tcds.2.png", sep=""),
    width=500, height=400,units="px", bg = "transparent")
print(tdsmph.segments.rand_and_tt.zoomedin.2)
dev.off()

quantity.nonrand.tt$Sample <- "turn taking"
quantity.rand$Sample <- "random"

quantity.rand_and_tt.all <- bind_rows(quantity.nonrand.tt, quantity.rand)
tod.tcds <- ggplot(data = quantity.rand_and_tt.all) +
  geom_point(data = quantity.rand_and_tt.all,
             aes(y = round(tds_mph,0), x = start.hr, color = Sample,
                 group = interaction(as.factor(ifelse(age_mo_round < 13, -1, 1)), Sample))) +
  geom_smooth(data = quantity.rand_and_tt.all,
              aes(y = round(tds_mph,0), x = start.hr, color = Sample,
                  group = interaction(as.factor(ifelse(age_mo_round < 13, -1, 1)), Sample)),
              method = "lm", formula = y ~ poly(x, 2)) +
  facet_grid(~ as.factor(ifelse(age_mo_round < 13, "<13mo", "13+mo"))) +
  ylab("TCDS (min/hr)") + xlab("Time of day (hour)")	+
  scale_y_continuous(limits=c(-10,40),
                     breaks=c(0,10,20, 30, 40)) +
  scale_x_continuous(limits=c(8,18),
                     breaks=seq(8,18,2)) +
  coord_cartesian(ylim=c(0,40),xlim=c(8,18)) +
  scale_color_manual(values = viridis(3)) +
  scale_fill_manual(values = viridis(3)) +
  theme_apa() +
  theme(legend.position="right",
        axis.line = element_line(color="black", size = 0.4))

tod.tcds.1 <- ggplot(data = quantity.rand) +
  geom_point(data = quantity.rand, color = "black",
             aes(y = round(tds_mph,0), x = start.hr,
                 group = as.factor(ifelse(age_mo_round < 13, -1, 1)))) +
  geom_smooth(data = quantity.rand, color = "black", fill = "black",
              aes(y = round(tds_mph,0), x = start.hr,
                  group = as.factor(ifelse(age_mo_round < 13, -1, 1))),
              method = "lm", formula = y ~ poly(x, 2)) +
  facet_grid(~ as.factor(ifelse(age_mo_round < 13, "<13mo", "13+mo"))) +
  ylab("TCDS (min/hr)") + xlab("Time of day (hour)")	+
  scale_y_continuous(limits=c(-10,40),
                     breaks=c(0,10,20, 30, 40)) +
  scale_x_continuous(limits=c(8,18),
                     breaks=seq(8,18,2)) +
  coord_cartesian(ylim=c(0,40),xlim=c(8,18)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")


tod.tcds.2 <- ggplot(data = quantity.nonrand.tt) +
  geom_point(data = quantity.nonrand.tt, color = "red",
             aes(y = round(tds_mph,0), x = start.hr,
                 group = as.factor(ifelse(age_mo_round < 13, -1, 1)))) +
  geom_smooth(data = quantity.nonrand.tt, color = "red", fill = "red",
              aes(y = round(tds_mph,0), x = start.hr,
                  group = as.factor(ifelse(age_mo_round < 13, -1, 1))),
              method = "lm", formula = y ~ poly(x, 2)) +
  facet_grid(~ as.factor(ifelse(age_mo_round < 13, "<13mo", "13+mo"))) +
  ylab("TCDS (min/hr)") + xlab("Time of day (hour)")	+
  scale_y_continuous(limits=c(-10,40),
                     breaks=c(0,10,20, 30, 40)) +
  scale_x_continuous(limits=c(8,18),
                     breaks=seq(8,18,2)) +
  coord_cartesian(ylim=c(0,40),xlim=c(8,18)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

png(paste("diff_figs/","tcds-tod.1.png", sep=""),
    width=1000, height=400,units="px", bg = "transparent")
print(tod.tcds.1)
dev.off()
png(paste("diff_figs/","tcds-tod.2.png", sep=""),
    width=1000, height=400,units="px", bg = "transparent")
print(tod.tcds.2)
dev.off()

# All segment info
all.segments <- all.rand.segments %>%
  mutate(sample = "random", sample_type = "random") %>%
  bind_rows(all.nonrand.segments) %>%
  arrange(aclew_child_id, segment)

# Number of speakers per clip
spkrs.per.seg.all <- all.data %>%
  filter(speaker != "CHI" &
           !(grepl("@", tier))) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(n_spkrs_clip = length(unique(speaker))) %>%
  full_join(all.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(n_spkrs_clip = 0)) %>%
  arrange(aclew_child_id, segment)

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

# Interactional sequences:
# continuous sequences only including target-child vocalization and
# target-child-directed talk
turn.sequences <- tibble()
for (i in 1:nrow(all.segments)) {
  # retrieve all the target child vocs and xds == T vocalizations
  subdata.tcds <- all.data %>%
    filter(aclew_child_id == all.segments$aclew_child_id[i] &
             segment == all.segments$segment[i] &
             grepl('xds@', tier) &
             val == "T") %>%
    mutate(uttid = paste0(speaker, start))
  subdata.chi <- all.data %>%
    filter(aclew_child_id == all.segments$aclew_child_id[i] &
             segment == all.segments$segment[i] &
             tier == "CHI") %>%
    mutate(uttid = paste0(speaker, start))
  subdata <- bind_rows(subdata.tcds, subdata.chi) %>%
    arrange(start)
  # work through each target child voc in the clip
  if (nrow(subdata) > 0) {
    if (nrow(subdata.chi) > 0) {
      chi.vocs <- mutate(subdata.chi,
          seq.num = rep(NA,nrow(subdata.chi)),
          seq.start = rep(NA,nrow(subdata.chi)),
          seq.start.spkr = rep(NA,nrow(subdata.chi)),
          seq.stop = rep(NA,nrow(subdata.chi)),
          seq.stop.spkr = rep(NA,nrow(subdata.chi)))
      seq.num <- 1
      for (j in 1:nrow(chi.vocs)) {
        curr.start <- chi.vocs$start[j]
        curr.stop <- chi.vocs$stop[j]
        curr.spk <- "CHI"
        curr.utt <- chi.vocs$uttid[j]
        # First check if this chi voc is already in a sequence. If so, skip it
        if (seq.num > 1) {
          prev.seq <- which(chi.vocs$seq.num == seq.num - 1)[1]
          prev.seq.start <- chi.vocs$seq.start[prev.seq]
          prev.seq.stop <- chi.vocs$seq.stop[prev.seq]
          if (curr.start >= prev.seq.start & curr.stop <= prev.seq.stop) {
            chi.vocs$seq.start[j] <- chi.vocs$seq.start[prev.seq]
            chi.vocs$seq.start.spkr[j] <- chi.vocs$seq.start.spkr[prev.seq]
            chi.vocs$seq.stop[j] <- chi.vocs$seq.stop[prev.seq]
            chi.vocs$seq.stop.spkr[j] <- chi.vocs$seq.stop.spkr[prev.seq]
            chi.vocs$seq.num[j] <- chi.vocs$seq.num[prev.seq]
            next
          }
        }
        # We start a loop to look for related utterances to the LEFT
        stop.looking.left <- FALSE
        # Look first for prior turns from that STOP
        #    - earliest: within the maximum gap allowed before the curr utt begins
        #    - latest: up to the limit on vocal overlap or the end of
        #              the curr utt (whichever comes first)
        while (stop.looking.left == FALSE) {
          candidate.vocs <- subdata %>%
            filter(uttid != curr.utt &
                     start <= curr.start &
                     stop >= curr.start - allowed.gap &
                     stop <= min(curr.stop, (curr.start + allowed.overlap))) %>%
            arrange(start)
          if (nrow(candidate.vocs) > 0) {
            # continue with the earliest (first occurring) candidate
            curr.start <- candidate.vocs$start[1]
            curr.stop <- candidate.vocs$stop[1]
            curr.spk <- candidate.vocs$speaker[1]
            curr.utt <- candidate.vocs$uttid[1]
          } else {
            stop.looking.left <- TRUE
          }
        }
        seq.start <- curr.start
        seq.start.spkr <- curr.spk

        # We start a loop to look for related utterances to the RIGHT
        curr.start <- chi.vocs$start[j]
        curr.stop <- chi.vocs$stop[j]
        curr.spk <- "CHI"
        stop.looking.right <- FALSE
        # Look first for turn transitions that START
        #    - earliest: within the allowed vocal overlap over the current utt,
        #              up to its start time (whichever comes later)
        #    - latest: before the maximum allowed gap after the curr utt ends
        while(stop.looking.right == FALSE) {
          candidate.vocs <- subdata %>%
            filter(uttid != curr.utt &
                     stop >= curr.stop &
                     start >= max(curr.start, (curr.stop - allowed.overlap)) &
                     start <= curr.stop + allowed.gap) %>%
            arrange(-stop)
          if (nrow(candidate.vocs) > 0) {
            # continue with the earliest (first occurring) candidate
            curr.start <- candidate.vocs$start[1]
            curr.stop <- candidate.vocs$stop[1]
            curr.spk <- candidate.vocs$speaker[1]
            curr.utt <- candidate.vocs$uttid[1]
          } else {
            stop.looking.right <- TRUE
          }
        }
        seq.stop <- curr.stop
        seq.stop.spkr <- curr.spk

        # Check if the "sequence" is actually just 1+ CHI utterances
        if (seq.start == chi.vocs$start[j] & seq.stop == chi.vocs$stop[j]) {
          chi.vocs$seq.stop[j] <-  0
          chi.vocs$seq.stop.spkr[j] <- "NONE"
          chi.vocs$seq.start[j] <- 0
          chi.vocs$seq.start.spkr[j] <- "NONE"
        } else if (length(unique(subset(subdata, start >= seq.start & stop <= seq.stop)$speaker)) == 1) {
          chi.vocs$seq.stop[j] <-  0
          chi.vocs$seq.stop.spkr[j] <- "NONE"
          chi.vocs$seq.start[j] <- 0
          chi.vocs$seq.start.spkr[j] <- "NONE"
        } else {
          chi.vocs$seq.stop[j] <- seq.stop
          chi.vocs$seq.stop.spkr[j] <- seq.stop.spkr
          chi.vocs$seq.start[j] <- seq.start
          chi.vocs$seq.start.spkr[j] <- seq.start.spkr
          # Write the sequence number
          chi.vocs$seq.num[j] <- seq.num
          seq.num <- seq.num + 1
        }
      }
      turn.sequences <- bind_rows(turn.sequences, chi.vocs)
    }
  }
}

# Sequence duration summary
turn.sequences <- turn.sequences %>%
  filter(!(is.na(seq.num))) %>%
  mutate(seq.dur = (seq.stop - seq.start)/60000)
turn.sequences.overview <- turn.sequences %>%
  group_by(aclew_child_id, sample, segment, seq.num, seq.start, seq.stop, seq.dur) %>%
  summarise(n_cvcs_seq = n()) %>%
  left_join(ptcp.info, by = "aclew_child_id") %>%
  left_join(spkrs.per.seg.all, by = c("aclew_child_id", "segment", "sample")) %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName"))
turn.seq.by.sample <- turn.sequences.overview %>%
  group_by(sample) %>%
  summarise(mean.tsq_dur = mean(seq.dur),
            median.tsq_dur = median(seq.dur),
            min.tsq_dur = min(seq.dur),
            max.tsq_dur = max(seq.dur),
            mean.cvcs = mean(n_cvcs_seq),
            median.cvcs = median(n_cvcs_seq),
            min.cvcs = min(n_cvcs_seq),
            max.cvcs = max(n_cvcs_seq))
turn.seq.by.sample.by.child <- turn.sequences.overview %>%
  group_by(aclew_child_id, sample) %>%
  summarise(mean.tsq_dur = mean(seq.dur),
            median.tsq_dur = median(seq.dur),
            min.tsq_dur = min(seq.dur),
            max.tsq_dur = max(seq.dur),
            mean.cvcs = mean(n_cvcs_seq),
            median.cvcs = median(n_cvcs_seq),
            min.cvcs = min(n_cvcs_seq),
            max.cvcs = max(n_cvcs_seq))

# Delta of chi voc and TCDS minutes
cvc.ovc.per.seg.rand <- all.data %>%
  filter(sample == "random" & speaker == "CHI" & speaker == tier) %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(cvc_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 5, cvc_min = 0)) %>%
  mutate(cvc_mph = (cvc_min/segment_dur)*60) %>%
  left_join(dplyr::select(tds.per.seg.rand, c("aclew_child_id", "segment", "tds_mph")),
            by = c("aclew_child_id", "segment")) %>%
  left_join(ptcp.info, by = "aclew_child_id") %>%
  mutate(cvc.ovc.delta = cvc_mph-tds_mph,
         cvc.ovc.delta.norm = cvc.ovc.delta/(cvc_mph+tds_mph)) %>%
  left_join(spkrs.per.seg.all, by = c("aclew_child_id", "segment")) %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName")) %>%
  arrange(aclew_child_id, segment)

cvc.ovc.per.seg.nonrand <- all.data %>%
  filter(sample != "random" & speaker == "CHI" & speaker == tier) %>%
  group_by(aclew_child_id, segment, segment_dur) %>%
  summarise(cvc_min = round(sum(dur)/60000,3)) %>%
  full_join(all.nonrand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(segment_dur = 5, cvc_min = 0)) %>%
  mutate(cvc_mph = (cvc_min/segment_dur)*60) %>%
  left_join(dplyr::select(tds.per.seg.nonrand, c("aclew_child_id", "segment", "tds_mph")),
            by = c("aclew_child_id", "segment")) %>%
  left_join(ptcp.info, by = "aclew_child_id") %>%
  mutate(cvc.ovc.delta = cvc_mph-tds_mph,
         cvc.ovc.delta.norm = cvc.ovc.delta/(cvc_mph+tds_mph)) %>%
  left_join(spkrs.per.seg.all, by = c("aclew_child_id", "segment", "sample", "sample_type")) %>%
  left_join(dplyr::select(seg.info, c("aclew_id", "CodeName", "start.hr")),
            by = c("aclew_child_id" = "aclew_id", "segment" = "CodeName")) %>%
  arrange(aclew_child_id, segment)

# Subset the samples for analysis
turn.transitions.rand_and_tt <- filter(turn.transitions.overview,
                                       sample == "random" | sample == "turn-taking")
turn.sequences.rand_and_tt <- filter(turn.sequences.overview,
                                     sample == "random" | sample == "turn-taking")
cvc.ovc.per.seg.rand_and_tt <- bind_rows(mutate(cvc.ovc.per.seg.rand, sample = "random"),
                                         filter(cvc.ovc.per.seg.nonrand, sample == "turn-taking"))
cvc.ovc.per.seg.rand_and_tt.nonas <- filter(cvc.ovc.per.seg.rand_and_tt, !(is.na(cvc.ovc.delta.norm)))

## Prepare variables for modeling
# prepare predictors for modeling
turn.transitions.rand <- filter(turn.transitions.rand_and_tt, sample == "random")
turn.transitions.rand$child_sex <- as.factor(turn.transitions.rand$child_sex)
turn.transitions.rand$mat_ed <- as.factor(turn.transitions.rand$mat_ed)
nspkrs.m <- mean(turn.transitions.rand$n_spkrs_clip)
nspkrs.sd <- sd(turn.transitions.rand$n_spkrs_clip)
turn.transitions.rand <- turn.transitions.rand %>%
  mutate(
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.m)/nspkrs.sd),
    stthr.std = (start.hr - 12)/12)

turn.sequences.rand <- filter(turn.sequences.rand_and_tt, sample == "random")
turn.sequences.rand$child_sex <- as.factor(turn.sequences.rand$child_sex)
turn.sequences.rand$mat_ed <- as.factor(turn.sequences.rand$mat_ed)
nspkrs.m <- mean(turn.sequences.rand$n_spkrs_clip)
nspkrs.sd <- sd(turn.sequences.rand$n_spkrs_clip)
turn.sequences.rand <- turn.sequences.rand %>%
  mutate(
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.m)/nspkrs.sd),
    stthr.std = (start.hr - 12)/12)

cvc.ovc.per.seg.randnonas <- filter(cvc.ovc.per.seg.rand_and_tt.nonas, sample == "random")
cvc.ovc.per.seg.randnonas$child_sex <- as.factor(cvc.ovc.per.seg.randnonas$child_sex)
cvc.ovc.per.seg.randnonas$mat_ed <- as.factor(cvc.ovc.per.seg.randnonas$mat_ed)
nspkrs.m <- mean(turn.sequences.rand$n_spkrs_clip)
nspkrs.sd <- sd(turn.sequences.rand$n_spkrs_clip)
cvc.ovc.per.seg.randnonas <- cvc.ovc.per.seg.randnonas %>%
  mutate(
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.m)/nspkrs.sd),
    stthr.std = (start.hr - 12)/12)

cvc.ovc.per.seg.randnonas.bychild <- cvc.ovc.per.seg.randnonas %>%
  group_by(aclew_child_id) %>%
  summarise(mean.ratio = mean(cvc.ovc.delta.norm))

# tt clips
turn.transitions.tt <- filter(turn.transitions.rand_and_tt, sample == "turn-taking")
turn.transitions.tt$child_sex <- as.factor(turn.transitions.tt$child_sex)
turn.transitions.tt$mat_ed <- as.factor(turn.transitions.tt$mat_ed)
nspkrs.m <- mean(turn.transitions.tt$n_spkrs_clip)
nspkrs.sd <- sd(turn.transitions.tt$n_spkrs_clip)
turn.transitions.tt <- turn.transitions.tt %>%
  mutate(
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.m)/nspkrs.sd),
    stthr.std = (start.hr - 12)/12)

turn.sequences.tt <- filter(turn.sequences.rand_and_tt, sample == "turn-taking")
turn.sequences.tt$child_sex <- as.factor(turn.sequences.tt$child_sex)
turn.sequences.tt$mat_ed <- as.factor(turn.sequences.tt$mat_ed)
nspkrs.m <- mean(turn.sequences.tt$n_spkrs_clip)
nspkrs.sd <- sd(turn.sequences.tt$n_spkrs_clip)
turn.sequences.tt <- turn.sequences.tt %>%
  mutate(
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.m)/nspkrs.sd),
    stthr.std = (start.hr - 12)/12)

cvc.ovc.per.seg.ttnonas <- filter(cvc.ovc.per.seg.rand_and_tt.nonas, sample == "turn-taking")
cvc.ovc.per.seg.ttnonas$child_sex <- as.factor(cvc.ovc.per.seg.ttnonas$child_sex)
cvc.ovc.per.seg.ttnonas$mat_ed <- as.factor(cvc.ovc.per.seg.ttnonas$mat_ed)
nspkrs.m <- mean(cvc.ovc.per.seg.ttnonas$n_spkrs_clip)
nspkrs.sd <- sd(cvc.ovc.per.seg.ttnonas$n_spkrs_clip)
cvc.ovc.per.seg.ttnonas <- cvc.ovc.per.seg.ttnonas %>%
  mutate(
    tchiyr.std = ((age_mo_round - tchiyr.m)/tchiyr.sd),
    chisx.std = recode_factor(child_sex,
                              "M" = "M", "F" = "F"),
    mated.std = recode_factor(mat_ed,
                              "none" = "none", "primary" = "primary",
                              "secondary" = "secondary", "preparatory" = "preparatory"),
    mated.bin = recode_factor(mat_ed,
                              "none" = "0-5", "primary" = "0-5",
                              "secondary" = "6+", "preparatory" = "6+"),
    motyr.std = ((mother_age - motyr.m)/motyr.sd),
    nsb.std = ((number_older_sibs - nsb.m)/nsb.sd),
    hsz.std = ((household_size - hsz.m)/hsz.sd),
    nsk.std = ((n_spkrs_clip - nspkrs.m)/nspkrs.sd),
    stthr.std = (start.hr - 12)/12)

cvc.ovc.per.seg.ttnonas.bychild <- cvc.ovc.per.seg.ttnonas %>%
  group_by(aclew_child_id) %>%
  summarise(mean.ratio = mean(cvc.ovc.delta.norm))

# Graph the basic turn taking rate info
# CHI-OTH transitions per minute
chi.oth.tts.rand_and_tt.1 <- ggplot(turn.transitions.rand,
                          aes(x = age_mo_round, y = n.c_o.tpm)) +
  geom_boxplot(aes(group = age_mo_round), color = "black", fill = "gray50", outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "black", color = "black", method = "lm") +
  ylab("CHI-OTH tts/min") + xlab("")	+
  scale_y_continuous(limits=c(0,30),
                     breaks=seq(0,30,5)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,30),xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

chi.oth.tts.rand_and_tt.2 <- ggplot(turn.transitions.tt,
                          aes(x = age_mo_round, y = n.c_o.tpm)) +
  geom_boxplot(aes(group = age_mo_round), color = "red", fill = "pink", outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "red", color = "red", method = "lm") +
  ylab("CHI-OTH tts/min") + xlab("")	+
  scale_y_continuous(limits=c(0,30),
                     breaks=seq(0,30,5)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,30),xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

png(paste("diff_figs/","tc-o.1.png", sep=""),
    width=450, height=400,units="px", bg = "transparent")
print(chi.oth.tts.rand_and_tt.1)
dev.off()
png(paste("diff_figs/","tc-o.2.png", sep=""),
    width=450, height=400,units="px", bg = "transparent")
print(chi.oth.tts.rand_and_tt.2)
dev.off()




# OTH-CHI transitions per minute
oth.chi.tts.rand_and_tt.1 <- ggplot(turn.transitions.rand,
                          aes(x = age_mo_round, y = n.o_c.tpm)) +
  geom_boxplot(aes(group = age_mo_round), color = "black", fill = "gray50", outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "black", color = "black", method = "lm") +
  ylab("OTH-CHI tts/min") + xlab("Child age (mo)")	+
  scale_y_continuous(limits=c(0,30),
                     breaks=seq(0,30,5)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,30),xlim=c(0,38)) +
  scale_color_manual(values = viridis(3)) +
  scale_fill_manual(values = viridis(3)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

oth.chi.tts.rand_and_tt.2 <- ggplot(turn.transitions.tt,
                          aes(x = age_mo_round, y = n.o_c.tpm)) +
  geom_boxplot(aes(group = age_mo_round), color = "red", fill = "pink", outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "red", color = "red", method = "lm") +
  ylab("OTH-CHI tts/min") + xlab("Child age (mo)")	+
  scale_y_continuous(limits=c(0,30),
                     breaks=seq(0,30,5)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,30),xlim=c(0,38)) +
  scale_color_manual(values = viridis(3)) +
  scale_fill_manual(values = viridis(3)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

png(paste("diff_figs/","o-tc.1.png", sep=""),
    width=450, height=400,units="px", bg = "transparent")
print(oth.chi.tts.rand_and_tt.1)
dev.off()
png(paste("diff_figs/","o-tc.2.png", sep=""),
    width=450, height=400,units="px", bg = "transparent")
print(oth.chi.tts.rand_and_tt.2)
dev.off()



# Graph the basic sequence duration info
# plot per-clip averages so it's consistent with the rest
turn.sequences.rand_and_tt.byclip <- turn.sequences.rand_and_tt %>%
  group_by(aclew_child_id, age_mo_round, sample, segment) %>%
  summarise(m.seqdur.sec = mean(seq.dur*60))

seq.dur.rand_and_tt.1 <- ggplot(subset(turn.sequences.rand_and_tt.byclip, sample == "random"),
                          aes(x = age_mo_round, y = m.seqdur.sec)) +
  geom_boxplot(aes(group = age_mo_round), color = "black", fill = "gray50", outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "black", color = "black", method = "lm") +
  ylab("Seq. dur. (sec)") + xlab("")	+
  scale_y_continuous(limits=c(0,60),
                     breaks=seq(0,60,20)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,60),xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

seq.dur.rand_and_tt.2 <- ggplot(subset(turn.sequences.rand_and_tt.byclip, sample != "random"),
                          aes(x = age_mo_round, y = m.seqdur.sec)) +
  geom_boxplot(aes(group = age_mo_round), color = "red", fill = "pink", outlier.shape = NA,
               lty = "solid") +
  geom_smooth(fill = "red", color = "red", method = "lm") +
  ylab("Seq. dur. (sec)") + xlab("")	+
  scale_y_continuous(limits=c(0,60),
                     breaks=seq(0,60,20)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,60),xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.text.x = element_text(color="black"),
	  axis.title.x = element_text(color="black"),
	  axis.text.y = element_text(color="black"),
	  axis.title.y = element_text(color="black"),
	  strip.text = element_text(color="black"),
		axis.ticks = element_line(color = "black"),
		axis.line = element_line(color="black", size = 0.4),
		legend.position = "none")

png(paste("diff_figs/","seqdur.1.png", sep=""),
    width=450, height=400,units="px", bg = "transparent")
print(seq.dur.rand_and_tt.1)
dev.off()
png(paste("diff_figs/","seqdur.2.png", sep=""),
    width=450, height=400,units="px", bg = "transparent")
print(seq.dur.rand_and_tt.2)
dev.off()

## Vocal maturity
# all vocalization types
chi.vm.lx.utts <- all.data %>%
  filter((tier == "vcm@CHI" | tier == "lex@CHI" | tier == "mwu@CHI") & !is.na(val)) %>%
  mutate(voc.rating = ifelse(val == "M", 4,
                             ifelse((val == "1" | val == "W"), 3,
                                    ifelse((val == "0" | val == "C"), 2,
                                           ifelse(val == "N", 1, 0))))) %>%
  filter(voc.rating > 0) %>%
  group_by(aclew_child_id, segment, sample, start) %>%
  summarise(max_voc.rtg = max(voc.rating))
all.voc.types.per.child <- tibble(
  aclew_child_id = rep(ptcp.info$aclew_child_id, 4),
  max_voc.rtg = c(rep(1, length(ptcp.info$aclew_child_id)), rep(2, length(ptcp.info$aclew_child_id)),
                  rep(3, length(ptcp.info$aclew_child_id)), rep(4, length(ptcp.info$aclew_child_id)))
)
chi.nvocs <- chi.vm.lx.utts %>%
  group_by(aclew_child_id) %>%
  summarise(n_vocs = n())
chi.vm.lx.voc.type.props <- chi.vm.lx.utts %>%
  group_by(aclew_child_id, max_voc.rtg) %>%
  summarise(n_voc.type = n()) %>%
  full_join(all.voc.types.per.child, by = c("aclew_child_id", "max_voc.rtg")) %>%
  replace_na(list(n_voc.type = 0)) %>%
  full_join(chi.nvocs, by = "aclew_child_id") %>%
  mutate(prop_voc.type = round(n_voc.type/n_vocs, 3)) %>%
  arrange(aclew_child_id, max_voc.rtg) %>%
  full_join(ptcp.info, by = "aclew_child_id")

# speech-like vs. non-speech-like only, only under 19mo
chi.vm.lx.utts.all <- all.data %>%
  filter((tier == "vcm@CHI" | tier == "lex@CHI" | tier == "mwu@CHI") & !is.na(val) & age_mo_round < 19) %>%
  mutate(voc.rating = ifelse(val == "M", 1,
                             ifelse((val == "1" | val == "W"), 1,
                                    ifelse((val == "0" | val == "C"), 1, 0)))) %>%
  group_by(aclew_child_id, segment, sample, start) %>%
  summarise(max_voc.rtg = max(voc.rating))
all.voc.types.per.child.all <- tibble(
  aclew_child_id = rep(ptcp.info$aclew_child_id, 2),
  max_voc.rtg = c(rep(0, length(ptcp.info$aclew_child_id)),
                  rep(1, length(ptcp.info$aclew_child_id)))
)
chi.nvocs.all <- chi.vm.lx.utts.all %>%
  group_by(aclew_child_id) %>%
  summarise(n_vocs = n())
chi.vm.lx.voc.type.props.bin <- chi.vm.lx.utts.all %>%
  group_by(aclew_child_id, max_voc.rtg) %>%
  summarise(n_voc.type = n()) %>%
  left_join(all.voc.types.per.child.all, by = c("aclew_child_id", "max_voc.rtg")) %>%
  replace_na(list(n_voc.type = 0)) %>%
  left_join(chi.nvocs.all, by = "aclew_child_id") %>%
  mutate(prop_voc.type = round(n_voc.type/n_vocs, 3)) %>%
  arrange(aclew_child_id, max_voc.rtg) %>%
  left_join(ptcp.info, by = "aclew_child_id")

chi.vm.lx.voc.type.props <- chi.vm.lx.voc.type.props %>%
  mutate(voc.type = factor(as.factor(max_voc.rtg), labels = c("NCB", "CB", "SW", "MW")))

voc.mat.by.age.1 <- ggplot(
    data = subset(chi.vm.lx.voc.type.props, voc.type == "NCB"),
    aes(x = age_mo_round, y = prop_voc.type)) +
  geom_point(color = "red") +
  geom_smooth(color = "red", fill = "red", method = "loess") +
  ylab("Prop of linguistic vocs") + xlab("Child age (mo)") +
  scale_y_continuous(limits=c(-0.5,1.5),
                     breaks=seq(0,1,0.2)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,1), xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	axis.text.x = element_text(color="black"),
	axis.title.x = element_text(color="black"),
	axis.text.y = element_text(color="black"),
	axis.title.y = element_text(color="black"),
	strip.text = element_text(color="black"),
	axis.ticks = element_line(color = "black"),
	axis.line = element_line(color="black", size = 0.4),
	legend.position = "none")

voc.mat.by.age.2 <- ggplot(
    data = subset(chi.vm.lx.voc.type.props, voc.type == "CB"),
    aes(x = age_mo_round, y = prop_voc.type)) +
  geom_point(color = "orange") +
  geom_smooth(color = "orange", fill = "orange", method = "loess") +
  ylab("Prop of linguistic vocs") + xlab("Child age (mo)") +
  scale_y_continuous(limits=c(-0.5,1.5),
                     breaks=seq(0,1,0.2)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,1), xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	axis.text.x = element_text(color="black"),
	axis.title.x = element_text(color="black"),
	axis.text.y = element_text(color="black"),
	axis.title.y = element_text(color="black"),
	strip.text = element_text(color="black"),
	axis.ticks = element_line(color = "black"),
	axis.line = element_line(color="black", size = 0.4),
	legend.position = "none")

voc.mat.by.age.3 <- ggplot(
    data = subset(chi.vm.lx.voc.type.props, voc.type == "SW"),
    aes(x = age_mo_round, y = prop_voc.type)) +
  geom_point(color = "green") +
  geom_smooth(color = "green", fill = "green", method = "loess") +
  ylab("Prop of linguistic vocs") + xlab("Child age (mo)") +
  scale_y_continuous(limits=c(-0.5,1.5),
                     breaks=seq(0,1,0.2)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,1), xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	axis.text.x = element_text(color="black"),
	axis.title.x = element_text(color="black"),
	axis.text.y = element_text(color="black"),
	axis.title.y = element_text(color="black"),
	strip.text = element_text(color="black"),
	axis.ticks = element_line(color = "black"),
	axis.line = element_line(color="black", size = 0.4),
	legend.position = "none")

voc.mat.by.age.4 <- ggplot(
    data = subset(chi.vm.lx.voc.type.props, voc.type == "MW"),
    aes(x = age_mo_round, y = prop_voc.type)) +
  geom_point(color = "blue") +
  geom_smooth(color = "blue", fill = "blue", method = "loess") +
  ylab("Prop of linguistic vocs") + xlab("Child age (mo)") +
  scale_y_continuous(limits=c(-0.5,1.5),
                     breaks=seq(0,1,0.2)) +
  scale_x_continuous(limits=c(0,38),
                     breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,1), xlim=c(0,38)) +
  theme_apa() +
  basic.theme + theme(
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	axis.text.x = element_text(color="black"),
	axis.title.x = element_text(color="black"),
	axis.text.y = element_text(color="black"),
	axis.title.y = element_text(color="black"),
	strip.text = element_text(color="black"),
	axis.ticks = element_line(color = "black"),
	axis.line = element_line(color="black", size = 0.4),
	legend.position = "none")

png(paste("diff_figs/","vocmat.1.png", sep=""),
    width=600, height=400,units="px", bg = "transparent")
print(voc.mat.by.age.1)
dev.off()
png(paste("diff_figs/","vocmat.2.png", sep=""),
    width=600, height=400,units="px", bg = "transparent")
print(voc.mat.by.age.2)
dev.off()
png(paste("diff_figs/","vocmat.3.png", sep=""),
    width=600, height=400,units="px", bg = "transparent")
print(voc.mat.by.age.3)
dev.off()
png(paste("diff_figs/","vocmat.4.png", sep=""),
    width=600, height=400,units="px", bg = "transparent")
print(voc.mat.by.age.4)
dev.off()

