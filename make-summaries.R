summarize_chattr <- function(tttbl.list) {
  # Real data summary
  if (nrow(tttbl.list$real.tt.vals) > 0) {
    tttbl.real <- summarize_tttbl(tttbl.list$real.tt.vals, "real data")    
  } else {
    tttbl.real <- tibble::tibble()
  }
  # Random data summary
  if (nrow(tttbl.list$random.tt.vals) > 0) {
    tttbl.rand <- summarize_tttbl(tttbl.list$random.tt.vals, "random data")    
  } else {
    tttbl.rand <- tibble::tibble()
  }
  chattr.summary <- dplyr::bind_rows(tttbl.real, tttbl.rand)
  return(chattr.summary)
}

summarize_tttbl <- function(tttbl, data.type) {
  tttbl <- tttbl %>%
    dplyr::filter(!is.na(prompt.spkr)|!is.na(response.spkr))
  if (nrow(tttbl) > 0) {
    tttbl <- tttbl %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        clip.start.msec = as.numeric(unlist(stringr::str_split(annot.clip, "[-_]"))[2]),
        clip.end.msec = as.numeric(unlist(stringr::str_split(annot.clip, "[-_]"))[3]),
        clip.duration.msec = clip.end.msec - clip.start.msec,
        clip.duration.min = clip.duration.msec/60000)
    tttbl <- tttbl %>%
      dplyr::mutate(
        pmt = ifelse(is.na(prompt.spkr), 0, 1),
        pmt.timing = start.ms - prompt.stop.ms,
        rsp = ifelse(is.na(response.spkr), 0, 1),
        rsp.timing = response.start.ms - stop.ms)
    tt.summary <- summarize_tts(tttbl, data.type)
  } else {
    if ("intseq.num" %in% names(tttbl)) {
      by.clip.summary <- tidyr::expand_grid(
        tt.summary, intseq.summary.empty)
    } else {
      annot.clips <- tttbl %>%
        dplyr::mutate(
          clip.start.msec = as.numeric(unlist(stringr::tr_split(annot.clip, "[-_]"))[2]),
          clip.end.msec = as.numeric(unlist(stringr::str_split(annot.clip, "[-_]"))[3]),
          clip.duration.msec = clip.end.msec - clip.start.msec) %>%
        dplyr::distinct(annot.clip, clip.start.msec, clip.duration.msec) %>%
        dplyr::mutate(data.type = data.type) %>%
        dplyr::select(data.type, annot.clip, clip.start.msec, clip.duration.msec)
      tt.summary <- tidyr::expand_grid(annot.clips, tt.summary.empty)
      by.clip.summary <- tidyr::expand_grid(
        tt.summary, intseq.summary.empty)
    }
  }
  # check for intseqs before summarizing them
  if ("intseq.num" %in% names(tttbl)) {
    tttbl.is <- tttbl %>%
      dplyr::filter(!is.na(intseq.num))
    if (nrow(tttbl.is) > 0) {
      intseq.summary <- summarize_intseqs(tttbl.is)
      by.clip.summary <- tt.summary %>%
        dplyr::left_join(intseq.summary, by = c("annot.clip"))
      } else {
        by.clip.summary <- tidyr::expand_grid(
          tt.summary, intseq.summary.empty)
      }
  }
  return(by.clip.summary)
}

summarize_tts <- function(tttbl, data.type) {
  tt.timings <- c(tttbl$pmt.timing, tttbl$rsp.timing)
  tt.summary <- tttbl %>%
    dplyr::group_by(annot.clip, clip.start.msec, clip.end.msec,
      clip.duration.msec, clip.duration.min) %>%
    dplyr::summarize(
      `.groups` = "drop",
      num.prompts = sum(pmt),
      num.responses = sum(rsp),
      num.transitions = num.prompts + num.responses,
      prompt.latency.msec.mean = mean(pmt.timing, na.rm = TRUE),
      prompt.latency.msec.median = median(pmt.timing, na.rm = TRUE),
      prompt.latency.msec.min = ifelse(sum(!(is.na(pmt.timing))) > 0,
                                       min(pmt.timing, na.rm = TRUE), NA),
      prompt.latency.msec.max = ifelse(sum(!(is.na(pmt.timing))) > 0,
                                       max(pmt.timing, na.rm = TRUE), NA),
      prompt.latency.msec.sd = sd(pmt.timing, na.rm = TRUE),
      response.latency.msec.mean = mean(rsp.timing, na.rm = TRUE),
      response.latency.msec.median = median(rsp.timing, na.rm = TRUE),
      response.latency.msec.min = ifelse(sum(!(is.na(rsp.timing))) > 0,
                                       min(rsp.timing, na.rm = TRUE), NA),
      response.latency.msec.max = ifelse(sum(!(is.na(rsp.timing))) > 0,
                                       max(rsp.timing, na.rm = TRUE), NA),
      response.latency.msec.sd = sd(rsp.timing, na.rm = TRUE)) %>%
    dplyr::mutate(
      prompts.per.min = num.prompts/clip.duration.min,
      responses.per.min = num.responses/clip.duration.min,
      transitions.per.min = num.transitions/clip.duration.min,
      transition.latency.msec.mean = mean(tt.timings, na.rm = TRUE),
      transition.latency.msec.median = median(tt.timings, na.rm = TRUE),
      transition.latency.msec.min = min(tt.timings, na.rm = TRUE),
      transition.latency.msec.max = max(tt.timings, na.rm = TRUE),
      transition.latency.msec.sd = sd(tt.timings, na.rm = TRUE),
      data.type = data.type) %>%
    dplyr::select(
      data.type, annot.clip, clip.start.msec, clip.duration.msec,
      num.prompts, prompts.per.min,
      prompt.latency.msec.mean, prompt.latency.msec.median,
      prompt.latency.msec.min, prompt.latency.msec.max,
      prompt.latency.msec.sd,
      num.responses, responses.per.min,
      response.latency.msec.mean, response.latency.msec.median,
      response.latency.msec.min, response.latency.msec.max,
      response.latency.msec.sd,
      num.transitions, transitions.per.min,
      transition.latency.msec.mean, transition.latency.msec.median,
      transition.latency.msec.min, transition.latency.msec.max,
      transition.latency.msec.sd)
  return(tt.summary)
}

summarize_intseqs <- function(tttbl.is) {
  tttbl.is <- tttbl.is %>%
    tidyr::unite("prompt.unique", c(prompt.spkr, prompt.start.ms, prompt.n.increments)) %>%
    tidyr::unite("response.unique", c(response.spkr, response.start.ms, response.n.increments))
  focal.spkr <- tttbl.is$speaker[1]
  intseq.summary.by.is <- tttbl.is %>%
    dplyr::group_by(annot.clip, clip.duration.min,
      intseq.num, intseq.start.spkr, intseq.start.ms, intseq.stop.ms) %>%
    dplyr::summarize(
      `.groups` = "drop",
      num.focus.vocs = dplyr::n(),
      num.prompts = sum(pmt),
      num.responses = sum(rsp),
      num.transitions = num.prompts + num.responses) %>%
    dplyr::mutate(
      focus.initiated = ifelse(intseq.start.spkr == focal.spkr, 1, 0),
      other.initiated = ifelse(focus.initiated == 1, 0, 1),
      intseq.start.msec = intseq.start.ms,
      intseq.stop.msec = intseq.stop.ms,
      intseq.duration.msec = intseq.stop.msec - intseq.start.msec,
      intseq.duration.min = intseq.duration.msec/60000,
      num.other.vocs = NA,
      REMOVEME = NA)
  for (is in unique(intseq.summary.by.is$intseq.num)) {
    is.idx <- which(tttbl.is$intseq.num == is)
    is.other.vocs <- unique(c(tttbl.is$prompt.unique[is.idx],
      tttbl.is$response.unique[is.idx]))
    non.na.vocs <- which(!(grepl("NA_NA_NA", is.other.vocs)))
    is.other.vocs <- is.other.vocs[non.na.vocs]
    if (length(is.other.vocs) > 0) {
      n.other.vocs <- sum(as.numeric(
        stringr::str_extract(is.other.vocs, "\\d+$")), na.rm = TRUE)
      is.other.vocs <- paste(is.other.vocs, collapse = ", ")
    } else {
      n.other.vocs <- 0
      is.other.vocs <- ""
    }
    intseq.summary.by.is$num.other.vocs[which(
      intseq.summary.by.is$intseq.num == is)] = n.other.vocs
    intseq.summary.by.is$REMOVEME[which(
      intseq.summary.by.is$intseq.num == is)] = is.other.vocs
  }
  intseq.summary <- intseq.summary.by.is %>%
    dplyr::group_by(annot.clip, clip.duration.min) %>%
    dplyr::summarize(
      `.groups` = "drop",
      num.intseqs = dplyr::n(),
      intseq.duration.msec.mean = mean(intseq.duration.msec, na.rm = TRUE),
      intseq.duration.msec.median = median(intseq.duration.msec, na.rm = TRUE),
      intseq.duration.msec.min = min(intseq.duration.msec, na.rm = TRUE),
      intseq.duration.msec.max = max(intseq.duration.msec, na.rm = TRUE),
      intseq.duration.msec.sd = sd(intseq.duration.msec, na.rm = TRUE),
      num.focus.initiated.intseqs = sum(focus.initiated, na.rm = TRUE),
      num.other.initiated.intseqs = sum(other.initiated, na.rm = TRUE),
      num.focus.vocs.per.intseq.mean = mean(num.focus.vocs, na.rm = TRUE),
      num.focus.vocs.per.intseq.median = median(num.focus.vocs, na.rm = TRUE),
      num.focus.vocs.per.intseq.min = min(num.focus.vocs, na.rm = TRUE),
      num.focus.vocs.per.intseq.max = max(num.focus.vocs, na.rm = TRUE),
      num.focus.vocs.per.intseq.sd = sd(num.focus.vocs, na.rm = TRUE),
      num.other.vocs.per.intseq.mean = mean(num.other.vocs, na.rm = TRUE),
      num.other.vocs.per.intseq.median = median(num.other.vocs, na.rm = TRUE),
      num.other.vocs.per.intseq.min = min(num.other.vocs, na.rm = TRUE),
      num.other.vocs.per.intseq.max = max(num.other.vocs, na.rm = TRUE),
      num.other.vocs.per.intseq.sd = sd(num.other.vocs, na.rm = TRUE),
      num.prompts.per.intseq.mean = mean(num.prompts, na.rm = TRUE),
      num.prompts.per.intseq.median = median(num.prompts, na.rm = TRUE),
      num.prompts.per.intseq.min = min(num.prompts, na.rm = TRUE),
      num.prompts.per.intseq.max = max(num.prompts, na.rm = TRUE),
      num.prompts.per.intseq.sd = sd(num.prompts, na.rm = TRUE),
      num.responses.per.intseq.mean = mean(num.responses, na.rm = TRUE),
      num.responses.per.intseq.median = median(num.responses, na.rm = TRUE),
      num.responses.per.intseq.min = min(num.responses, na.rm = TRUE),
      num.responses.per.intseq.max = max(num.responses, na.rm = TRUE),
      num.responses.per.intseq.sd = sd(num.responses, na.rm = TRUE),
      num.transitions.per.intseq.mean = mean(num.transitions, na.rm = TRUE),
      num.transitions.per.intseq.median = median(num.transitions, na.rm = TRUE),
      num.transitions.per.intseq.min = min(num.transitions, na.rm = TRUE),
      num.transitions.per.intseq.max = max(num.transitions, na.rm = TRUE),
      num.transitions.per.intseq.sd = sd(num.transitions, na.rm = TRUE)) %>%
    dplyr::mutate(intseqs.per.min = num.intseqs/clip.duration.min)
  return(intseq.summary)
}

intseq.summary.empty <- tibble::tibble(
  num.intseqs = 0,                      
  intseq.duration.msec.mean = NA,        
  intseq.duration.msec.median = NA,      
  intseq.duration.msec.min = NA,         
  intseq.duration.msec.max = NA,         
  intseq.duration.msec.sd = NA,          
  num.focus.initiated.intseqs = NA,      
  num.other.initiated.intseqs = NA,      
  num.focus.vocs.per.intseq.mean = NA,              
  num.focus.vocs.per.intseq.median = NA, 
  num.focus.vocs.per.intseq.min = NA,    
  num.focus.vocs.per.intseq.max = NA,    
  num.focus.vocs.per.intseq.sd = NA,     
  num.other.vocs.per.intseq.mean = NA,   
  num.other.vocs.per.intseq.median = NA, 
  num.other.vocs.per.intseq.min = NA,    
  num.other.vocs.per.intseq.max = NA,    
  num.other.vocs.per.intseq.sd = NA,     
  num.prompts.per.intseq.mean = NA,      
  num.prompts.per.intseq.median = NA,    
  num.prompts.per.intseq.min = NA,       
  num.prompts.per.intseq.max = NA,       
  num.prompts.per.intseq.sd = NA,        
  num.responses.per.intseq.mean = NA,    
  num.responses.per.intseq.median = NA,  
  num.responses.per.intseq.min = NA,     
  num.responses.per.intseq.max = NA,     
  num.responses.per.intseq.sd = NA,      
  num.transitions.per.intseq.mean = NA,  
  num.transitions.per.intseq.median = NA,
  num.transitions.per.intseq.min = NA,   
  num.transitions.per.intseq.max = NA,   
  num.transitions.per.intseq.sd = NA,    
  intseqs.per.min = 0       
)

tt.summary.empty <- tibble::tibble(
  num.prompts = 0,                   
  prompts.per.min = 0,               
  prompt.latency.msec.mean = NA,      
  prompt.latency.msec.median = NA,    
  prompt.latency.msec.min = NA,       
  prompt.latency.msec.max = NA,       
  prompt.latency.msec.sd = NA,        
  num.responses = 0,                 
  responses.per.min = 0,             
  response.latency.msec.mean = NA,    
  response.latency.msec.median = NA,  
  response.latency.msec.min = NA,     
  response.latency.msec.max = NA,     
  response.latency.msec.sd = NA,      
  num.transitions = 0,               
  transitions.per.min = NA,           
  transition.latency.msec.mean = NA,  
  transition.latency.msec.median = NA,
  transition.latency.msec.min = NA,   
  transition.latency.msec.max = NA,   
  transition.latency.msec.sd = NA
)
