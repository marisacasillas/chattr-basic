library(tidyverse)

aastbl.classes <- paste0(c("character", "character",
                    "integer", "integer", "integer", "character"), collapse = "")
aastbl.colnames <- c("tier", "speaker", "start.ms", "stop.ms", "duration", "value")

# This must be in the rigid spchtbl format specified in the docs
read_spchtbl <- function(filepath, tbltype) {
  if (tbltype != "aas-elan-txt") {
    print("Sorry, that file type isn't available!")
    next()
  }
  spchtbl <- aas_to_spchtbl(filepath)
  return(spchtbl)
}

aas_to_spchtbl <- function(tbl) {
  aastbl <- read_delim(file = tbl, delim = "\t", col_names = FALSE)
  # check that the column types are correct
  col.types <- paste0(unlist(lapply(aastbl, class)), collapse = "")
  # add the standard column names
  names(aastbl) <- aastbl.colnames
  # start with just the utterances
  wide.aastbl <- filter(aastbl, tier == speaker)
  # add in addressee information
  xds.aastbl <- filter(aastbl, grepl('xds', tier)) %>%
    select(speaker, start.ms, value) %>%
    rename(addressee = value)
  # add in vocal maturity data
#  vcm.aastbl <- filter(aastbl, speaker == "CHI" & speaker != tier) %>%
#    spread(tier, value) %>%
#    rename(vcm = 'vcm@CHI', lex = 'lex@CHI', mwu = 'mwu@CHI') %>%
#    select(speaker, start.ms, vcm, lex, mwu)
  # add all info to wide table
  wide.aastbl <- left_join(wide.aastbl, xds.aastbl) %>%
#    left_join(vcm.aastbl) %>%
    select(speaker, start.ms, stop.ms, addressee)
  return(wide.aastbl)
}