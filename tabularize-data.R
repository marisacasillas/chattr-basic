library(tidyverse)

ebtbl.colnames <- c("tier", "speaker", "start.ms", "stop.ms", "duration", "value")
ann.marker <- "annotated-" ## ALSO USED IN CHATTR HELPERS
spch.seg.ptrn <- ".*Segment spkr=\"([A-Z]{3}).*startTime=\"([A-Z0-9.]+)\" endTime=\"([A-Z0-9.]+)\".*"
paraling.ptrn <- "^&=[a-z]+[.!?]$"

# This must be in the rigid spchtbl format specified in the docs
read_spchtbl <- function(filepath, tbltype, cliptier, lxonly) {
  if (tbltype == "aas-elan-txt") {
    spchtbl <- aas_to_spchtbl(filepath, cliptier, lxonly)
  } else if (tbltype == "elan-basic-txt") {
    spchtbl <- elanbasic_to_spchtbl(filepath, cliptier, lxonly)
  } else if (tbltype == "lena-its") {
    spchtbl <- its_to_spchtbl(filepath, lxonly)
  } else {
    print("Sorry, that file type isn't available!")
  }
  return(spchtbl)
}

aas_to_spchtbl <- function(tbl, cliptier, lxonly) {
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
  if (lxonly == TRUE) {
    wide.aastbl <- filter(aastbl, tier == "CHI" |
        (tier == speaker & !(grepl(paraling.ptrn, value))))
  } else {
    wide.aastbl <- filter(aastbl, tier == speaker)
  }
  if (nrow(wide.aastbl) == 0) {
    print("No utterances detected in file.")
  } else {
    # add in addressee information for each non-CHI utterance
    xds.aastbl <- filter(aastbl, grepl('xds', tier)) %>%
      select(speaker, start.ms, value) %>%
      rename(addressee = value)
    if (NA %in% unique(xds.aastbl$speaker)) {
      print("WARNING: You may have a speaker information; make sure the correct Participant is entered in the metadata for each ELAN tier before exporting.")
    }
    if (lxonly == TRUE) {
      # now add in vocal maturity data
      vcm.aastbl <- filter(aastbl, speaker == "CHI" & speaker != tier) %>%
        spread(tier, value) %>%
        rename(vcm = 'vcm@CHI', lex = 'lex@CHI', mwu = 'mwu@CHI') %>%
        mutate(non.lx = case_when(
          vcm == "Y" ~ 1,
          vcm == "L" ~ 1,
          vcm == NA & lex == 0 ~ 1,
          vcm == NA & lex == NA & mwu == NA ~ 1,
          TRUE ~ 0
        )) %>%
        # remove non-linguistic vocalizations
        filter(non.lx == 1) %>%
        select(speaker, start.ms)
      # add all info to wide table
      wide.aastbl <- anti_join(wide.aastbl, vcm.aastbl) %>%
        left_join(xds.aastbl) %>%
        select(speaker, start.ms, stop.ms, addressee)
    } else {
      wide.aastbl <- left_join(wide.aastbl, xds.aastbl) %>%
        select(speaker, start.ms, stop.ms, addressee)
    }
    # add in information about the annotated regions
    # (if no annotation, stop and tell the user)
    if (cliptier %in% unique(aastbl$tier)) {
      clip.tbl <- filter(aastbl, tier == cliptier) %>%
        mutate(speaker = paste0(ann.marker, start.ms, "_", stop.ms, "-", value),
          addressee = NA) %>%
        select(speaker, start.ms, stop.ms, addressee)
      wide.aastbl <- bind_rows(clip.tbl, wide.aastbl)
      return(wide.aastbl)
    } else {
      print("Error: no rows from the clip tier found.")
    }
  }
}

elanbasic_to_spchtbl <- function(tbl, cliptier, lxonly) {
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
  # subset to linguistic vocalizations if desired
  if (is.character(lxonly)) {
    ebtbl <- ebtbl %>%
      filter(grepl(lxonly, value))
  } else {
    print("Invalid value for lxonly parameter. Provide a pattern that matches linguistic vocalization annotations in the last column; see documentation for an example.")
  }
  # add in information about the annotated regions
  # (if no annotation, stop and tell the user)
  if (cliptier %in% unique(ebtbl$speaker)) {
    clip.tbl <- filter(ebtbl, speaker == cliptier) %>%
      mutate(speaker = paste0(ann.marker, start.ms, "_", stop.ms, "-", value))
    ebtbl <- bind_rows(clip.tbl, ebtbl) %>%
      select(-value)
    return(ebtbl)
  } else {
    print("Error: no rows from the clip tier found.")
  }
}

its_to_spchtbl <- function(its.file, lxonly) {
  
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
      start.ms = as.integer(round(as.numeric(gsub("[A-Z]", "", start.LENA))*1000)),
      stop.ms = as.integer(round(as.numeric(gsub("[A-Z]", "", stop.LENA))*1000)),
      duration = stop.ms - start.ms
    )
  if ("value" %in% colnames(spchtbl)) {
    spchtbl <- spchtbl %>%
      select(speaker, start.ms, stop.ms, duration, value)
  } else {
    spchtbl <- spchtbl %>%
      mutate(value = NA) %>%
      select(speaker, start.ms, stop.ms, duration, value)
  }
  min.rec <- min(spchtbl$start.ms)
  max.rec <- max(spchtbl$stop.ms)
  clip.tbl <- tibble(
    speaker = paste0(ann.marker, min.rec, "_", max.rec, "-", "FULL_RECORDING"),
    start.ms = min.rec,
    stop.ms = max.rec,
    duration = stop.ms - start.ms
  )
  spchtbl <- bind_rows(clip.tbl, spchtbl)
  return(spchtbl)
}
