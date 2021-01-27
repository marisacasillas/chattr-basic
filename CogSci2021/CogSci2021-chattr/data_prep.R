# Load chattr code
source("../../tabularize-data.R")
source("../../transition-detectors.R")
source("../../chattr-helpers.R")
source("../../tabularize-data.R")
source("../../transition-detectors.R")
source("../../continuation-detectors.R")
source("../../intseq-detectors.R")
source("../../estimate-baseline.R")
source("../../fetch-chattr-info.R")
source("../../make-summaries.R")

input.data.path <- "../../../chattr-paper/annotated-data/CogSci-subset/"
metadata.path <- "../../../chattr-paper/metadata/CogSci-subset/"
reread.data <- TRUE  # only set to TRUE if you want to re-do
                      # the search for turn-taking behaviors
                      # set to FALSE for doing analysis with
                      # the already-run data
bigtable.filename.real <- "tt.bigtable.real.all.csv"
bigtable.filename.rand <- "tt.bigtable.rand.all.csv"
bigtable.filename.summary <- "tt.bigtable.summ.all.csv"
bigtable.recdata.filename <- "tt.bigtable.all.with.recdata.csv"
coded.regions.filename <- "coded.regions.csv"


# Read in metadata
rec.metadata <- read_csv(paste0(metadata.path, "recording-info.csv"))

# Read in files
if (reread.data == FALSE) {
  tt.bigtable <- read_csv(bigtable.filename.real)
  tt.bigtable.rand <- read_csv(bigtable.filename.rand)
  tt.bigtable.summary <- read_csv(bigtable.filename.summary)
} else {   # read in speech annotations
  tt.bigtable <- tibble()
  tt.bigtable.rand <- tibble()
  tt.bigtable.summary <- tibble()
  coded.regions.tbl <- tibble()
  
  # ITS files
  standard.code.areas.its <- tibble(
    start.ms = seq(34, 20*60, 30)*60000,
    stop.ms = start.ms + 60000,
    duration = 60000,
    value = as.character(1:(length(seq(34, 20*60, 30))))
    ) %>%
    mutate(
      speaker = paste0("annotated-", as.character(start.ms), "_",
                       as.character(stop.ms), "-", value)
    ) %>%
    select(speaker, start.ms, stop.ms, duration, value)
  its.files <- list.files(input.data.path, "*.its")
  if (length(its.files) > 0) {
    # NB: We're going to break up the normal fetch_chatter_LENA() function
    # so we can subset to the one-minute segments that were hand annotated
    for (file in its.files) {
      print(file)
      # Decisions for comparability:
      # allowed interactants: FA, MA, and CX
      # nearonly: yes (far speech in Praat data is on combo tiers)
      # lxonly: no (no lx info for CXN vocs)
      # min.dur: 600ms (min LENA annot dur)
      # max.ovlp: 0ms (no ovlp possible w/ LENA)
      
      # step 1. read in the file
      spchtbl <- read_spchtbl(filepath = paste0(input.data.path, file),
                              tbltype = "lena-its",
                              cliptier = ".alloneclip",
                              nearonly = TRUE,
                              lxonly = FALSE) %>%
        filter(!(grepl("annotated-", speaker)))
      # Manually add in the regions matching the one-minute annotations
      max.start <- max(spchtbl$start.ms)
      code.areas <- standard.code.areas.its %>%
        filter((start.ms <= max.start & stop.ms >= max.start) |
                 (start.ms <= max.start & stop.ms < max.start))
      spchtbl <- bind_rows(code.areas, spchtbl)
      # Save the full list of coded regions for later
      coded.regions.tbl <- bind_rows(coded.regions.tbl, code.areas %>%
                                   mutate(filename = file))

      # step 2. run the speech annotations through the tt behavior
      # detection pipeline
      ttinfotbls <- fetch_chattr_tttbl(
        spchtbl = spchtbl,
        interactants = "(FA)|(MA)|(CX)",
        min.utt.dur = 599,
        allowed.overlap = 0,
        target.ptcp = "CH",
        n.runs = 10)

      # step 3a. add real and random data to big tables
      tt.bigtable <- bind_rows(tt.bigtable,
                               ttinfotbls$real.tt.vals %>%
                                 mutate(filename = file))
      tt.bigtable.rand <- bind_rows(tt.bigtable.rand,
                                    ttinfotbls$random.tt.vals %>%
                                      mutate(filename = file))

      # step 3b. create a summary of the tt behavior by clip and overall,
      # incl. the random baseline and add it to the big tables
      tt.summary <- summarize_chattr(ttinfotbls)
      tt.bigtable.summary <- bind_rows(tt.bigtable.summary,
                                       tt.summary %>%
                                         mutate(filename = file))
    }
  }
  
  # BST TXT files
  bsttxt.files <- list.files(input.data.path, "C.*.txt")
  if (length(bsttxt.files) > 0) {
    for (file in bsttxt.files) {
      print(file)

      ttdata <- fetch_chatter_BST(
        paste0(input.data.path, file),
        cliptier = "code",
        target.ptcp = "CHI",
        # nearonly = TRUE, # No need to include since implicit in interactants
        lxonly = FALSE,
        interactants = "(FA)|(MA)|(CX)",
        min.utt.dur = 599,
        allowed.overlap = 0,
        n.runs = 10)

      tt.bigtable <- bind_rows(tt.bigtable,
                               ttdata$real.tt.vals %>%
                                 mutate(filename = file))
      tt.bigtable.rand <- bind_rows(tt.bigtable.rand,
                                    ttdata$random.tt.vals %>%
                                      mutate(filename = file))
      tt.bigtable.summary <- bind_rows(tt.bigtable.summary,
                                       ttdata$tt.summary %>%
                                         mutate(filename = file))
      
      # Save the full list of coded regions for later
      code.areas <- read_delim(paste0(input.data.path, file),
                          "\t", col_names =
                            c("tier", "speaker", "start.ms",
                              "stop.ms", "duration", "value")) %>%
        filter(tier == "code") %>%
        select(-tier) %>%
        mutate(
          speaker = paste0("annotated-", start.ms, "_", stop.ms, "-", value),
          value = as.character(value),
          filename = file)
      coded.regions.tbl <- bind_rows(coded.regions.tbl, code.areas)
    }
  }

  # AAS TXT files
  aastxt.files <- list.files(input.data.path, "\\d{4}.txt")
  if (length(aastxt.files) > 0) {
    for (file in aastxt.files) {
      print(file)

      ttdata <- fetch_chatter_AAS(
        paste0(input.data.path, file),
        cliptier = "code_num",
        addressee.tags = "[T]",
        n.runs = 10)
      tt.bigtable <- bind_rows(tt.bigtable,
                               ttdata$real.tt.vals %>%
                                 mutate(filename = file))
      tt.bigtable.rand <- bind_rows(tt.bigtable.rand,
                               ttdata$random.tt.vals %>%
                                 mutate(filename = file))
      tt.bigtable.summary <- bind_rows(tt.bigtable.summary,
                               ttdata$tt.summary %>%
                                 mutate(filename = file))
      
      # Save the full list of coded regions for later
      code.areas <- read_delim(paste0(input.data.path, file),
                               "\t", col_names =
                                 c("tier", "speaker", "start.ms",
                                   "stop.ms", "duration", "value")) %>%
        filter(tier == "code_num") %>%
        select(-tier) %>%
        mutate(
          speaker = paste0("annotated-", start.ms, "_", stop.ms, "-", value),
          value = as.character(value),
          filename = file)
      coded.regions.tbl <- bind_rows(coded.regions.tbl, code.areas)
    }
  }
  
  write_csv(left_join(tt.bigtable, rec.metadata,
                      by = c("filename" = "annot_filename")),
            bigtable.filename.real)
  write_csv(left_join(tt.bigtable.rand, rec.metadata,
                      by = c("filename" = "annot_filename")),
            bigtable.filename.rand)
  write_csv(left_join(tt.bigtable.summary, rec.metadata,
                      by = c("filename" = "annot_filename")),
            bigtable.filename.summary)
  
  write_csv(coded.regions.tbl %>%
              rename("annot.clip" = speaker, "annot.clip.dur.ms" = duration) %>%
              select(annot.clip, annot.clip.dur.ms, filename),
            coded.regions.filename)
}
# # merge rec info into turn-taking behavior
# tt.bigtable <- tt.bigtable %>%
#   left_join(rec.metadata, by = c("filename" = "annot_filename"))
# if (reread.data == FALSE) {
#   write_csv(tt.bigtable, bigtable.recdata.filename)
# }