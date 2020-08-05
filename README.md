# chattr
*chattr is an R package for extracting turn-taking measures from tabular utterance data.*

### A new tool for measuring turn-taking

Researchers who are interested in studying naturalistic turn-taking behavior have traditionally been limited to manual analysis of their data. More recently, [the LENA Foundation](https://www.lena.org/) has provided an alternative approach: an integrated and automated recording-to-annotations system that includes two relevant measures for turn-taking behavior (Conversational Turn Count and Conversational Block). The traditional, manual-annotation approach results in highly variable methods and analyses across studies, while the LENA approach is both proprietary and uses a theoretically unusual basis for measuring these interactional behaviors (e.g., allowing up to 5s of silence between turns). The chattr package aims to bridge this gap, providing a set of simple functions in R that are flexible to individual researchers' needs, yet are unified in their core methods for detecting and measuring basic turn-taking behaviors.

To skip to an introduction of the core functions, click [here](#quick-start-guide).

### What does chattr do?

The chattr package identifies turn transitions (i.e., when one speaker stops and another starts) in annotated interactional data and, on the basis of those transitions, computes a number of summary measures about turn taking behaviors. It can take input annotations from:

* .its files exported from LENA,
* .txt files (e.g., exported from [ELAN](https://archive.mpi.nl/tla/elan) or other sources), or
* .rttm files (NIST Rich Transcription Time Marked files).

To detect turn transitions, chattr scans a temporal window around each utterance  produced by a target interactant, e.g., the focal child, signer, etc. For each utterance (t<sub>0</sub>), it scans for potential prompts (t<sub>-1</sub>) and responses (t<sub>+1</sub>) to that utterance (Figure 1). If the annotated data contains information about addressee (i.e., to whom an utterance is directed), users can also limit their search to just those utterances with the right 'addressee' value.

Once chattr has scanned a file, it produces a table with one row for each target utterance, including the onset, offset, and speaker information for any detected prompts and responses. If the target utterance, its prompt, or its response is part of a chain of adjacent utterances by the same speaker (e.g., if the response has multiple distinct sub-units; Figure 1), the onset and offset information for each chain is also provided in the table. By default, chattr also checks which turn transitions link together into larger interactional sequences (e.g., child-parent-child-parent-child).

In many circumstances, it it also helpful to know what the baseline rate of turn taking would be, given the utterances (i.e., how often do turn transitions occur by chance?). The chattr package makes it easy to run random simulations of the utterance data to estimate the baseline chance rate of the measured turn-taking behaviors.

The resulting output has three parts:

1. A summary overview of the turn-taking stats for that file
2. A table with turn-relevant information for each target utterance
3. A table similar to 2. above, but derived from randomly shuffled utterances for as many runs as is specified by the user (0 runs by default)

The temporal detail given by chattr can be used to compute any number of interactional measures. Here are a few ideas:

* Rate of turn-taking (i.e., rate of child--other and other--child transitions)
* Frequency and duration of interactional sequences
* Speed of turn transitions
* Hourly and daily trends in interactional bursts
* Ratio of vocalization by the child and others during turn exchanges
* Differences in turn-taking between the child and different family members

A few code examples are provided in the [**Quick start guide**](#quick-start-guide) section below.

### What is a turn transition?

Turn transitions are at the heart of the chattr package. A turn transition occurs when one speaker's turn stops and another speaker's turn begins. Every turn transition has a pre-transition speaker and a post-transition speaker—these must be different speakers.

* The turn transition _begins_ when the first turn ends.
* The turn transition _ends_ when the second turn starts.

If the second turn starts before the first turn ends, the transition time is negative; this is referred to as an instance of 'transitional overlap'. If the second turn starts after the first turn ends, the transition time is positive; referred to as an instance of 'transitional gap'. Sometimes speakers produce a chain of utterances before or after the transition (e.g., a sequence of related utterances forming one larger turn at talk). We refer to these chains by the same speaker as 'multi-unit' turns (Figure 1).

### What is an interactional sequence?

Interactional sequences, as defined in chatter, are unbroken sequences of turn taking between the target interactant and one or more of their interactional partners. Interactional sequences are akin to conversational bouts and may thereby reflect more structurally complex, engaged interactional behaviors than single turn transitions do.

As with turn transitions, interactional sequences in chattr can include multi-unit turns. That is, transitions between speakers in an interactional sequence may be separated by multiple turn units from each speaker (Figure 1).

![Example of a few seconds of dyadic talk:](chattr-turn-transitions.png)
*Figure 1. An example of a brief dyadic interaction between two speakers: A (green) and B (orange). The speakers here use both single- and multi-unit turns. There are 6 turns at talk (3 from each speaker), 4 turn transitions (two each from B to A and vice versa; black arrows), and one interactional sequence (the contiguous block of speaker continuation/transition marked with green/orange arrows; the other speech ('but-. what's it for?') has no turn transitions and so is not an interactional sequence):*


#### An important caveat

Any analyst looking manually at interactional data would always first check that the speakers are indeed mutually engaged before labeling and/or measuring an observed interactional phenomenon for further analysis (e.g., child-to-other turn transition). Unfortunately, this rich criterion for _semantic_ contingency between turns—not just _temporal_ contingency—is beyond what chattr can do. Consider, for example, a case where four co-present speakers engage in two simultaneous conversations. Because chattr only looks for temporal contingencies, it may detect transitions across conversations, and at the expense of catching within-conversation transitions. 

It is important to remember that chattr only detects temporal patterns that *look like* turn-taking behavior. You as the analyst are responsible for checking how reliably the detected turn-taking behavior alighns with true conversational interaction behavior. To overcome this limitation, consider adding addressee coding when your data features simultaneous interactions and/or highly variable interactional contexts.


## Quick start guide
The chattr package is designed to be straightforward to use, even for those who are just starting out with R. It has one core function for each type of input (.its, .txt, .rttm), which users can customize as needed. It also has many smaller functions that run specific tasks relating to turn-taking data. These will be comprehensively summarized elsewhere. <!--TO DO!!-->

For all core functions, the default settings are as follows (see [Customizations](#customizations) for changes to these defaults):

* The analysis window is to allow up to 2000 ms of transitional gap and up to 1000 ms of transitional overlap.
* Turn transitions can occur with utterances of any duration, of any type, and between any potential interactional partner and the target interactant.
* When there are multiple potential prompts/responses, chattr picks the one closest to the target utterance (i.e., 'strict' mode)
* The output returns a summary and table of turn-taking and interactional sequence data for each target utterance in the input, but by default does _not_ conduct any random baseline runs.


### LENA user?

The core function for LENA .its files is `fetch_chatter_LENA()`.

This function requires one argument: a path to an ..its file, such as `fetch_chatter_LENA("myfile.its")`

By default, the target interactant is "CH" (the key child) and turn transitions can take place with any other hearable human utterance (i.e., "FA", "MA", and "OC").

**QUICK TIPS:** You can exclusively scan speech-like CH vocalizations (`lxonly = TRUE`) and/or "nearby" speech (`nearonly = TRUE`). For example, the following command will approximate the Conversational Turn measure calculated by LENA:

```
fetch_chatter_LENA("myfile.its", lxonly = TRUE, nearonly = TRUE)`
```

### ELAN or other basic speech table user?

The core function for utterances that are stored in a tabular format is `fetch_chatter_BST()`

This function requires two arguments: a path to a .txt in the expected format (see below) and the label used for the target interactant, such as `fetch_chatter_BST("myfile.txt", target.ptcp = "Gail")`.

**QUICK TIPS:** If you want to limit the search for turn transitions to certain interactant types or utterance content, you can use a regular expression pattern to specify your constraints. For example, given the example Table 2 below, the following command will only search for turn transitions between the target child "CHI" and any adult speaker, where utterances from all speakers must contain some transcribed linguistic content and the adult utterance must be marked with a "CHI" as addressee:

```
fetch_chatter_BST("myfile.txt", target.ptcp = "CHI",
                  interactants = "^[MFU]A\\d$", addressee.tags = "CHI", lxonly = "^&=[a-z]+[.!?]")
```

If you have coded specific sub-sections of your media file, make sure those sections are included in your file, with the start and stop time of all coded sections, and with the "speaker" name of your choice (see "coded.clip" in the example below). 

```
fetch_chatter_BST("myfile.txt", target.ptcp = "CHI",
                  interactants = "^[MFU]A\\d$", addressee.tags = "CHI", lxonly = "^&=[a-z]+[.!?]",
                  cliptier = "coded.clip")
```

If you have followed the [ACLEW Annotation Scheme standards](https://osf.io/b2jep/wiki/home/), there is a shortcut function already assembled that assumes the target interactant is "CHI", that the addressee tags can be "C" or "T", and that the cliptier is called "code":

```
fetch_chatter_AAS("myAASfile.txt", target.ptcp = "CHI", interactants = "^[MFU]A\\d$")
```

#### .txt format requirements

The input .txt should be a plain-text, tab-separated file on which each utterance of the interaction is demarcated on its own row, with a header row that contains the column names. The three columns must be `speaker` (who is producing the utterance?), `start.ms` (what is the start time of the utterance in whole milliseconds?), `stop.ms` (what is the stop time of the utterance in whole milliseconds?). You may _optionally_ include two more tiers: `addressee` (information about who the speech is addressed to, as a character string of your choice) and `value` (information about the content of the utterance, as a character string of your choice). Some examples are shown below.

*Table 1. The bare minimum for a .txt file is speaker, start, and stop information for each utterance.*

| speaker    | start.ms | stop.ms |
|------------|----------|---------|
| coded.clip | 0        | 10000   |
| coded.clip | 50000    | 60000   |
| CHI        | 450      | 1080    |
| CHI        | 2274     | 3500    |
| CHI        | 5251     | 5789    |
| MA1        | 210      | 1260    |
| MA1        | 4910     | 5256    |
| MA1        | 5288     | 5909    |
| FC1        | 3393     | 4971    |

*Table 2. This version of table 1 has two additional optional columns added.*

| speaker    | start.ms | stop.ms | addressee | val           |
|------------|----------|---------|-----------|---------------|
| coded.clip | 0        | 10000   | <NA>      | clip1         |
| coded.clip | 50000    | 60000   | <NA>      | clip2         |
| CHI        | 450      | 1080    | MA1       | &=laughs.     |
| CHI        | 2274     | 3500    | MA1       | no.           |
| CHI        | 5251     | 5789    | FC1       | no.           |
| MA1        | 210      | 1260    | FC1       | &=laughs.     |
| MA1        | 4910     | 5256    | CHI       | oh you!       |
| MA1        | 5288     | 5909    | FC1       | hey &=laughs. |
| FC1        | 3393     | 4971    | CHI       | now this one. |


### RTTM user?

The core function for utterances that are stored in a tabular format is `fetch_chatter_RTTM()`

This function requires two arguments: a path to a file in the expected .rttm format (see below) and the label used for the target interactant, such as `fetch_chatter_RTTM("myfile.rttm", target.ptcp = "Gail")`.

These files are plain-text, space-delimited files containing up to ten fields. The chattr package also accepts tab-delimited .rttm files to accommodate orthographic transcriptions in the sixth field ([reference](https://github.com/nryant/dscore/blob/master/README.md)). Note that there should be _no_ header row in your file:

| Type   | FileID        | Channel | Onset (s) | Dur (s) | Orth | SpkrType | SpkrID | ConfScore | Lookahead |
|--------|----------------|---------|-----------|---------|------|----------|----------|-----------|-----------|
|SPEAKER | mediabasename | 1       | 0.4       | 0.5     | <NA> | FA       | <NA>   | <NA>      | <NA>      |
|SPEAKER | mediabasename | 1       | 1.1       | 0.4     | <NA> | KCHI     | <NA>   | <NA>      | <NA>      |
|SPEAKER | mediabasename | 1       | 1.8       | 1.0     | <NA> | MA       | <NA>   | <NA>      | <NA>      |
|SPEAKER | mediabasename | 1       | 2.8       | 0.6     | <NA> | KCHI     | <NA>   | <NA>      | <NA>      |


### Customizations

You can customize these function calls by changing any of the default argument settings:

`allowed.gap` = the maximum amount of time, in milliseconds, that is allowed to pass during a speaker transition. Set to 2000 by default.

`allowed.overlap` = the maximum amount of time, in milliseconds, during which overlap is allowed allowed to occur at the turn transition. Set to 1000 by default.

`min.utt.dur` = the minimum duration of an utterance allowed, in milliseconds. Utterances shorter than this threshold are not included in the search for turn transitions. Set to 0 by default (i.e., include all utterances).

`interactants` = the interactants who can be considered to be potential turn-takers with the focal interactant. By default, this argument is set to `FALSE`, which means it searches all possible interactant tiers for turn transitions with the focus interactant (the exception is LENA .its files, where it searches all near and far human interactant voices). Pass a regular expression pattern 

`addressee.tags` = the type of addressee tag(s) that indicates an utterance is relevant to taking turns with the target interactant. Set to `FALSE` by default (i.e., all utterances are considered relevant for turn taking).

`lx.only` = the type of utterance content that indicates an utterance has linguistic content. Set to `FALSE` by default (i.e., all utterances are considered relevant for turn taking). Note that this variable subsets utterances by any match in content, e.g., it could be used to restrict utterances to those containing the word "red" or having a "?" at the end, etc.

`mode` = the strategy to use when deciding between multiple candidate prompts or responses to a given focal speaker utterance. Current options are "strict", "stretch", "qulr" (quick-uptake, late-response), and "luqr" (late-uptake, quick-response). Strict picks candidates that minimize the duration of the turn transition. Stretch picks candidates that maximize it. The other two modes mix these options: qulr picks prompts that minimize transition duration but responses that maximize it, and luqr vice versa. Set to `strict` by default.

`output` = the type of turn-taking data to provide in the output table. Current options are "intseqtbl" and "tttbl"; the latter option restricts the analysis to just turn transition data and does not attempt to identify interactional sequences. The default setting is ``intseqtbl`, which returns both turn transition and interactional sequence outcomes.

`cliptier` = the annotation tier indicating which periods of the original media file have been annotated with utterances. Set to `.alloneclip` by default, which assumes that the entire media file is annotated, and uses the start of the first utterance and the end of the last utterance to estimate the total annotated time.

`n.runs` = the number of random simulations to run; that is, the number of times to randomly shuffle the placement of each utterance for each speaker within each annotated clip and then compute the same turn-taking analysis as run with the real data. This output can be used to estimate the baseline rate of turn taking behaviors that would be expected on the basis of the vocalization rates alone. Set to 0 by default (i.e., do not conduct any random runs) to minimize processing time for those who do not use this function.

## How to understand the results

If you run one of the core functions over your data, you will get a list object back that has three parts:

* **yourlist$summary**: this table is a descriptive overview of the detailed turn taking data in the second part. _Note that this part is still under development and will this always appear empty in your output._
* **yourlist$real.tt.data**: this table contains the detailed turn taking data detected by chattr, including both turn transitions and interactional sequences by default (to change, see above)
* **yourlist$random.tt.data**: this table contains the detailed turn taking data detected by chattr on any random simulations you ran, similar to the second part but with an additional column called "random.run.num". By default this will be empty, but if you do run random simulations, summary data will be included in the first part

### Output column types (alphabetical)
<!--TO DO: Add whatever cols end up in the summary table-->

`addressee` = the addressee value for the target utterance. If there is no addressee value, this is NA.

`annot.clip` = the label for the annotated clip in which the target utterance was made.

`intseq.num` = the interactional sequence number in which the target utterance can be found. This number increments from 1, starting with the first sequence detected. If the utterance/turn transition is not part of an interactional sequence, this is NA.
                      
`intseq.start.ms` = the start time of the interactional sequence in msec from the beginning of the file. If the utterance/turn transition is not part of an interactional sequence, this is NA.

`intseq.stop.ms` = the stop time of the interactional sequence in msec from the beginning of the file. If the utterance/turn transition is not part of an interactional sequence, this is NA.

`intseq.start.spkr` = the interactant who produced the first utterance in the interactional sequence (i.e., beginning at `intseq.start.ms`). If the utterance/turn transition is not part of an interactional sequence, this is NA.

`intseq.stop.spkr` = the interactant who produced the last utterance in the interactional sequence (i.e., ending at `intseq.stop.ms`). If the utterance/turn transition is not part of an interactional sequence, this is NA.

`prompt.n.increments` = assuming a prompt, the number of same-interactant increments that group with the prompt utterance. If no prompt was found or no same-interactant pre-prompt increments were found, this is NA.

`prompt.prev.increment.start` = assuming a prompt, the start time of the earliest same-interactant increment before the prompt utterance, in msec from the beginning of the file. If no prompt was found or no same-interactant pre-prompt increments were found, this is NA.

`prompt.prev.increment.stop` = assuming a prompt, the stop time of the earliest same-interactant increment before the prompt utterance, in msec from the beginning of the file. If no prompt was found or no same-interactant pre-prompt increments were found, this is NA. 

`prompt.spkr` = assuming a "prompt" (i.e., a turn after which there is a transition to the target interactant), the name of the interactant who provided the prompt. If no prompt was found, this is NA.

`prompt.start.ms` = assuming a prompt, the start time of the prompt utterance in msec from the beginning of the file. If no prompt was found, this is NA.

`prompt.stop.ms` = assuming a prompt, the stop time of the prompt utterance in msec from the beginning of the file. If no prompt was found, this is NA.         

`random.run.num` = indicates the number of the random simulation run associated with the target utterance. When at least one random run is conducted, this number increments from 1 and goes up to the number of random runs specified by the user.

`response.n.increments` = assuming a response, the number of same-interactant increments that group with the response utterance. If no response was found or no same-interactant post-response increments were found, this is NA.

`response.post.increment.start` = assuming a response, the start time of the latest same-interactant increment after the response utterance, in msec from the beginning of the file. If no response was found or no same-interactant post-response increments were found, this is NA.

`response.post.increment.stop` = assuming a response, the stop time of the latest same-interactant increment after the response utterance, in msec from the beginning of the file. If no response was found or no same-interactant post-response increments were found, this is NA.

`response.spkr` = assuming a "response" (i.e., a turn that is preceded by a transition from the focal speaker), the name of the interactant who provided the response. If no response was found, this is NA.

`response.start.ms` = assuming a response, the start time of the response utterance in msec from the beginning of the file. If no response was found, this is NA.           

`response.stop.ms` = assuming a response, the stop time of the response utterance in msec from the beginning of the file. If no response was found, this is NA.           

`speaker` = the label given for the target interactant.

`speaker.n.increments` = the number of target interactant increments that group with the target utterance. If no same-interactant increments were found before or after turn transition, this is NA.

`spkr.post.increment.start` = the start time of the latest target interactant increment after the target utterance, in msec from the beginning of the file. If no target interactant post-utterance increments were found, this is NA.

`spkr.post.increment.stop` = the stop time of the latest target interactant increment after the target utterance, in msec from the beginning of the file. If no target interactant post-utterance increments were found, this is NA.

`spkr.prev.increment.start` = the start time of the earliest target interactant increment before the target utterance, in msec from the beginning of the file. If no target interactant pre-utterance increments were found, this is NA.

`spkr.prev.increment.stop` = the stop time of the earliest target interactant increment before the target utterance, in msec from the beginning of the file. If no target interactant pre-utterance increments were found, this is NA.

`start.ms` = the start time of the target utterance in msec from the beginning of the file.

`stop.ms` = the stop time of the target utterance in msec from the beginning of the file.

`vocseq.num` = the vocalization sequence number in which the target utterance can be found. This number increments from 1, starting with the first vocalization sequence detected (that is, a sequence of vocalizations by the target interactant with no turn transitions). If the utterance is not part of a vocalization sequence, this is NA.
                      
`vocseq.start.ms` = the start time of the vocalization sequence in msec from the beginning of the file. If the utterance transition is not part of a vocalization sequence, this is NA.

`vocseq.stop.ms` = the stop time of the vocalization sequence in msec from the beginning of the file. If the utterance transition is not part of a vocalization sequence, this is NA.

`vocseq.start.spkr` = the interactant who produced the first utterance in the vocalization sequence (i.e., beginning at `vocseq.start.ms`). If the utterance transition is not part of a vocalization sequence, this is NA.

`vocseq.stop.spkr` = the interactant who produced the last utterance in the vocalization sequence (i.e., ending at `vocseq.stop.ms`). If the utterance is not part of a vocalization sequence, this is NA.

## Examples

To be added!

## Final comments

### chattr isn't limited to child language data... or even to speech data!
In principle, you can use chattr functions to detect temporal contingencies between any 2+ data streams that feature a binary state (i.e., "happening now" vs. "not happening now"), so long as you format the data as one of the readable inputs below. Some examples might include:

* Measuring the turn-transition rate between all speakers in a multi-party setting with addressee coding by running the transition detector once for each speaker with that person as 'focal' and the others as 'interactants'
* Detecting contingencies between vocalizations and ambient music
* Identifying turn-taking behavior in annotated data from non-human animals

### If you use chattr, please cite it
The following reference is the most current one for use in your academic output involving the chattr package: Casillas, M. (in preparation). The chattr package.

### Find a problem? Have a feature request? Please let us know!
Please submit your issue with a detailed description and, if you have found a bug, a way to replicate it to our github page: [https://github.com/marisacasillas/chattr-basic]()


## References
VanDam, M., Warlaumont, A. S., Bergelson, E., Cristia, A., Soderstrom,
M., Palma, P. D., & MacWhinney, B. (2016). HomeBank: An online
repository of daylong child-centered audio recordings. _Seminars in
Speech and Language, 37_(2), 128-142. <doi:10.1055/s-0036-1580745>

VanDam, Mark (2018). VanDam Public 5-minute HomeBank Corpus. <doi:10.21415/T5388S>
