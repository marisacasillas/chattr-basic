library(tidyverse)

ebtbl.colnames <- c("tier", "speaker", "start.ms", "stop.ms", "duration", "value")

# This must be in the rigid spchtbl format specified in the docs
read_spchtbl <- function(filepath, tbltype, cliptier) {
  if (tbltype == "aas-elan-txt") {
    spchtbl <- aas_to_spchtbl(filepath, cliptier)
  } else if (tbltype == "elan-basic-txt") {
    spchtbl <- elanbasic_to_spchtbl(filepath, cliptier)
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
      mutate(speaker = paste0("annotated-", start.ms, "-", value),
        addressee = NA) %>%
      select(speaker, start.ms, stop.ms, addressee)
    wide.aastbl <- bind_rows(clip.tbl, wide.aastbl)
    return(wide.aastbl)
  } else {
    print("Error: no rows from the clip tier found.")
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
      mutate(speaker = paste0("annotated-", start.ms, "-", value))
    ebtbl <- bind_rows(clip.tbl, ebtbl)
    return(ebtbl)
  } else {
    print("Error: no rows from the clip tier found.")
  }
}
