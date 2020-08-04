# Default argument values
default.max.gap <- 2000 # allows up to this number of milliseconds of gap at transitions
default.max.overlap <- 1000 # allows up to this number of milliseconds of overlap at transitions
default.min.utt.dur <- 0 # assesses all utterances, regardless of duration
default.mode <- "strict" # uses strict mode when choosing between multiple transition types
default.interactants <- FALSE # all speaker tiers as interactants
default.addressee.tags <- FALSE # all utterances assessed, regardless of addressee
default.lxonly <- FALSE # all utterances assessed, regardless of linguistic content
default.cliptier <- ".alloneclip" # all utterances in the file are considered
default.output <- "intseq" # returns intseq data in addition to basic turn taking data
default.n.runs <- 0 # returns no random runs by default (b/c they are time consuming)
# NOTE:
# If something isn't set to a default below, it means the argument value is
# specific to that instance of the function call, e.g., file path, or
# specific to that fetch_chatter call, e.g., the nearonly val for LENA .its
# files and the specific focus.child settings for LENA and AAS files)


fetch_chattr_tttbl <- function(
  # requires user input:
  spchtbl, focus.child,
  # in case of independent tttbl processing with this function:
  cliptier = default.cliptier, lxonly = default.lxonly,
  # used by fetch_transitions() and fetch_intseqs() below:
  allowed.gap = default.max.gap, allowed.overlap = default.max.overlap,
  min.utt.dur = default.min.utt.dur, interactants = default.interactants,
  addressee.tags = default.addressee.tags, mode = default.mode,
  # fetch_randomruns() arguments used below:
  output = default.output, n.runs = default.n.runs) {
  
  # TO DO: modify input spchtbl to be in line with cliptier and lxonly args if needed

  # get tttbl and/or intseq data as desired
  if (output == "intseqtbl" | output == "tttbl") {
    print("Estimating turn transitions...")
    real.tbl <- fetch_transitions(spchtbl, allowed.gap, allowed.overlap,
                                    min.utt.dur, focus.child, interactants,
                                    addressee.tags, mode)
    if (output == "intseqtbl") {
      print("Estimating interactional sequences...")
      real.tbl <- fetch_intseqs(real.tttbl, allowed.gap)
    }
    if (n.runs > 0) {
      print("Estimating interactional sequences for randomized simulations...")
      all.tbls <- fetch_randomruns(
        spchtbl = spchtbl, n.runs = n.runs,
        allowed.gap = allowed.gap, allowed.overlap = allowed.overlap,
        min.utt.dur = min.utt.dur, focus.child = focus.child,
        interactants = interactants, addressee.tags = addressee.tags,
        mode = mode, output = output, input.tbl = real.tbl,
        return.real = FALSE)
    } else {
      all.tbls <- list(
        real.tt.vals = real.tbl,
        random.tt.vals = NA)
    }
    # return the results as a list of the real and random tables
    return(all.tbls)
  } else {
    print("Invalid type of output specified: the options are 'intseq' (default) or 'tttbl'.")
  }
}


fetch_chatter_LENA <- function(
  # read_spchtbl() arguments
  tbl, tbltype = "lena-its",
  cliptier = default.cliptier, lxonly = default.lxonly, nearonly = FALSE,
  # fetch_transitions() and fetch_intseqs() arguments
  allowed.gap = default.max.gap, allowed.overlap = default.max.overlap,
  min.utt.dur = default.min.utt.dur,
  focus.child = "CH", interactants = default.interactants,
  addressee.tags = default.addressee.tags, mode = default.mode,
  # estimate_baseline() arguments
  output = default.output, n.runs = default.n.runs) {

  # step 1. read in the file
  spchtbl <- read_spchtbl(filepath = tbl, tbltype = tbltype,
                          lxonly = lxonly, nearonly = nearonly)

  # step 2. run the speech annotations through the tt behavior detection pipeline
  ttinfotbls <- fetch_chattr_tttbl(
      tttbl = spchtbl, cliptier = cliptier, lxonly = lxonly,
      allowed.gap = allowed.gap, allowed.overlap = allowed.overlap,
      min.utt.dur = min.utt.dur,
      focus.child = focus.child, interactants = interactants,
      addressee.tags = addressee.tags,
      mode = mode, output == output, n.runs = n.runs)

  # step 3. create a summary of the tt behavior by clip and overall, incl. the random baseline
  # TO DO
  
  #... return... TO DO
  return("... AHEM. TO DO")
}



# tbl <- "../chattr-paper/annotated-data/raw/123522-1904.its"
# 
# # user optional
# lxonly <- FALSE
# nearonly <- FALSE
# allowed.gap <- 2000
# allowed.overlap <- 1000
# min.utt.dur <- 300
# focus.child <- "CX"
# interactants <- c("FA", "MA")
# addressee.tags <- "none"
# mode <- "stretch"
# output <- "tttbl"
# n.runs <- 2
# 
# # other optional
# tbltype <- "lena-its"
# cliptier <- ".alloneclip"
