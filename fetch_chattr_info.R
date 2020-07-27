# Default argument values
default.max.gap <- 2000
default.max.overlap <- 1000
default.min.utt.dur <- 0
default.mode <- "strict"
default.interactants <- FALSE # (all speaker tiers as interactants)
default.addressee.tags <- FALSE
default.lxonly <- FALSE
default.cliptier <- ".alloneclip"
default.output <- "intseq"
default.n.runs <- 0
# NOTE:
# If something isn't set to a default below, it means the argument value is
# specific to that instance of the function call, e.g., file path, or
# specific to that fetch_chatter call, e.g., the nearonly val for LENA .its
# files and the specific focus.child settings for LENA and AAS files)


fetch_chattr_tttbl <- function(
  # independent tttbl processing
  tttbl, cliptier = default.cliptier, lxonly = default.lxonly,
  # fetch_transitions() and fetch_intseqs() arguments
  allowed.gap = default.max.gap, allowed.overlap = default.max.overlap,
  min.utt.dur = default.min.utt.dur,
  focus.child, interactants = default.interactants,
  addressee.tags = default.addressee.tags, mode = default.mode,
  # estimate_baseline() arguments
  output = default.output, n.runs = default.n.runs) {
  # TO DO: modify input spchtbl to be in line with cliptier and lxonly args if needed

  # get tttbl and/or intseq data as desired
  if (output == "intseq" | output == "tttbl") {
    print("Estimating turn transitions...")
    real.tttbl <- fetch_transitions(tttbl, allowed.gap, allowed.overlap,
                                    min.utt.dur, focus.child, interactants,
                                    addressee.tags, mode)
    if (output == "intseq") {
      print("Estimating interactional sequences...")
      real.intseqtbl <- fetch_intseqs(real.tttbl, allowed.gap)
      if (n.runs > 0) {
        print("Estimating interactional sequences for randomized simulations...")
        random.tbls <- fetch_randomruns(
          spchtbl = tttbl, n.runs = n.runs,
          allowed.gap = allowed.gap, allowed.overlap = allowed.overlap,
          min.utt.dur = min.utt.dur, focus.child = focus.child,
          interactants = interactants, addressee.tags = addressee.tags,
          mode = mode, output = output, input.tttbl = real.intseqtbl,
          return.real = FALSE)
      }
    } else {
      if (n.runs > 0) {
        print("Estimating interactional sequences for randomized simulations...")
        random.tbls <- fetch_randomruns(
          spchtbl = tttbl, n.runs = n.runs,
          allowed.gap = allowed.gap, allowed.overlap = allowed.overlap,
          min.utt.dur = min.utt.dur, focus.child = focus.child,
          interactants = interactants, addressee.tags = addressee.tags,
          mode = mode, output = output, input.tttbl = real.tttbl,
          return.real = FALSE)
      }
    }
    # return the results as a list of the real and random tables
    return("... AHEM. TO DO")
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



tbl <- "../chattr-paper/annotated-data/raw/123522-1904.its"

# user optional
lxonly <- FALSE
nearonly <- FALSE
allowed.gap <- 2000
allowed.overlap <- 1000
min.utt.dur <- 300
focus.child <- "CX"
interactants <- c("FA", "MA")
addressee.tags <- "none"
mode <- "stretch"
output <- "tttbl"
n.runs <- 2

# other optional
tbltype <- "lena-its"
cliptier <- ".alloneclip"
