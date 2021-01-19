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
reread.data <- FALSE  # only set to TRUE if you want to re-do
                      # the search for turn-taking behaviors
                      # set to FALSE for doing analysis with
                      # the already-run data
bigtable.filename.real <- "tt.bigtable.real.all.csv"
bigtable.filename.rand <- "tt.bigtable.rand.all.csv"
bigtable.filename.summary <- "tt.bigtable.summ.all.csv"
bigtable.recdata.filename <- "tt.bigtable.all.with.recdata.csv"


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
  
  # ITS files
  its.files <- list.files(input.data.path, "*.its")
  if (length(its.files) > 0) {
    for (file in its.files) {
      ttdata <- fetch_chatter_LENA(
        paste0(input.data.path, file),
        nearonly = TRUE,
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
    }
  }
  
  # TXT files
  txt.files <- list.files(input.data.path, "*.txt")
  if (length(txt.files) > 0) {
    for (file in txt.files) {
      ttdata <- fetch_chatter_AAS(
        paste0(input.data.path, file),
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
    }
  }
  write_csv(tt.bigtable, bigtable.filename.real)
  write_csv(tt.bigtable.rand, bigtable.filename.rand)
  write_csv(tt.bigtable.summary, bigtable.filename.summary)
}
# merge rec info into turn-taking behavior
tt.bigtable <- tt.bigtable %>%
  left_join(rec.metadata, by = c("filename" = "annot_filename"))
if (reread.data == FALSE) {
  write_csv(tt.bigtable, bigtable.recdata.filename)
}