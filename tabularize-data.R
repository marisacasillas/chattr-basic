library(tidyverse)

# This must be in the rigid spchtbl format specified in the docs
read_spchtbl <- function(spchtbl) {
  spchdata <- read_delim(file = spchtbl, delim = sep, col_names = headerTF)
  return(spchdata)
}

aas_to_spchtbl <- function()