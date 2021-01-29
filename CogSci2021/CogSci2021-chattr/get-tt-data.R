library(chattr)

ttdata <- fetch_chatter_BST(
  "txt-input/tab-delimited-input.txt",
  cliptier = "code",
  target.ptcp = "CHI",
  lxonly = FALSE,
  interactants = "(FA)|(MA)|(UC)",
  min.utt.dur = 599,
  allowed.overlap = 0)

# contingency information for each `target.ptcp` emission
ttdata$real.tt.vals

# summary overview of turn-taking behavior for each clip in `cliptier`
ttdata$tt.summary

# randomized contingency information for each `target.ptcp` emission
# (this is an optional random baseline)
ttdata$real.tt.vals
