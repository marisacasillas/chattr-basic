library(tidyverse)

ebtbl.colnames <- c("tier", "speaker", "start.ms", "stop.ms", "duration", "value")
ann.marker <- "annotated-" ## ALSO USED IN CHATTR HELPERS
spch.seg.ptrn <- ".*Segment spkr=\"([A-Z]{3}).*startTime=\"([A-Z0-9.]+)\" endTime=\"([A-Z0-9.]+)\".*"

# This must be in the rigid spchtbl format specified in the docs
read_spchtbl <- function(filepath, tbltype, cliptier) {
  if (tbltype == "aas-elan-txt") {
    spchtbl <- aas_to_spchtbl(filepath, cliptier)
  } else if (tbltype == "elan-basic-txt") {
    spchtbl <- elanbasic_to_spchtbl(filepath, cliptier)
  } else if (tbltype == "lena-its") {
    spchtbl <- its_to_spchtbl(filepath)
  } else {
    print("Sorry, that file type isn't available!")
  }
  return(spchtbl)
}

aas_to_spchtbl <- function(tbl, cliptier) {
  aastbl <- read_delim(file = tbl, delim = "\t",
    col_names = ebtbl.colnames, col_types = cols(
      tier = col_character(),
      speaker = col_character(),
      start.ms = col_integer(),
      stop.ms = col_integer(),
      duration = col_integer(),
      value = col_character()
    ))
  # extract the top-level utterance tiers
  wide.aastbl <- filter(aastbl, tier == speaker)
  if (nrow(wide.aastbl) == 0) {
    print("No utterances detected: no turn transitions")
  } else {
    # add in addressee information for each utterance
    xds.aastbl <- filter(aastbl, grepl('xds', tier)) %>%
      select(speaker, start.ms, value) %>%
      rename(addressee = value)
    # now add in vocal maturity data (to-do)
    #  vcm.aastbl <- filter(aastbl, speaker == "CHI" & speaker != tier) %>%
    #    spread(tier, value) %>%
    #    rename(vcm = 'vcm@CHI', lex = 'lex@CHI', mwu = 'mwu@CHI') %>%
    #    select(speaker, start.ms, vcm, lex, mwu)
    # add all info to wide table
    wide.aastbl <- left_join(wide.aastbl, xds.aastbl) %>%
      #    left_join(vcm.aastbl) %>%
      select(speaker, start.ms, stop.ms, addressee)
    # add in information about the annotated regions
    # (if no annotation, stop and tell the user)
    if (cliptier %in% unique(aastbl$tier)) {
      clip.tbl <- filter(aastbl, tier == cliptier) %>%
        mutate(speaker = paste0(ann.marker, start.ms, "-", value),
          addressee = NA) %>%
        select(speaker, start.ms, stop.ms, addressee)
      wide.aastbl <- bind_rows(clip.tbl, wide.aastbl)
      return(wide.aastbl)
    } else {
      print("Error: no rows from the clip tier found.")
    }
  }
}

elanbasic_to_spchtbl <- function(tbl, cliptier) {
  ebtbl <- read_delim(file = tbl, delim = "\t",
    col_names = ebtbl.colnames, col_types = cols(
      tier = col_character(),
      speaker = col_character(),
      start.ms = col_integer(),
      stop.ms = col_integer(),
      duration = col_integer(),
      value = col_character()
    )) %>%
    select(tier, start.ms, stop.ms, value) %>%
    rename(speaker = tier)
  # add in information about the annotated regions
  # (if no annotation, stop and tell the user)
  if (cliptier %in% unique(ebtbl$speaker)) {
    clip.tbl <- filter(ebtbl, speaker == cliptier) %>%
      mutate(speaker = paste0(ann.marker, start.ms, "-", value))
    ebtbl <- bind_rows(clip.tbl, ebtbl) %>%
      select(-value)
    return(ebtbl)
  } else {
    print("Error: no rows from the clip tier found.")
  }
}

its_to_spchtbl <- function(its.file) {
  
  # Extract the speaker segment lines from the .its file
  its.data <- read_lines(its.file)
  seg.spkr.lines <- which(grepl("Segment spkr=", its.data))
  its.spkr.data <- its.data[seg.spkr.lines]
  
  # Extract only the speaker, start, and stop times for each segment
  spchtbl <- stringr::str_match(
    its.spkr.data, spch.seg.ptrn)[,2:4]
  colnames(spchtbl) <- c("speaker", "start.LENA", "stop.LENA")
  
  # Reformat to a chattr spchtbl format
  spchtbl <- as_tibble(spchtbl) %>%
    mutate(
      start.ms = as.numeric(gsub("[A-Z]", "", start.LENA))*1000,
      stop.ms = as.numeric(gsub("[A-Z]", "", stop.LENA))*1000,
      duration = stop.ms - start.ms
    ) %>%
    select(speaker, start.ms, stop.ms, duration, value)
  min.rec <- min(spchtbl$start.ms)
  max.rec <- max(spchtbl$stop.ms)
  clip.tbl <- tibble(
    speaker = paste0(ann.marker, min.rec, "-", "FULL_RECORDING"),
    start.ms = min.rec,
    stop.ms = max.rec,
    duration = stop.ms - start.ms
  )
  spchtbl <- bind_rows(clip.tbl, spchtbl)
  return(spchtbl)
}
