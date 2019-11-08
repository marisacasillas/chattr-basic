# chattr
R package for extracting turn-interactive measures from tabular conversation data.

**NONE OF THIS WORKS RIGHT NOW—COME BACK LATER :D**

## measures

#### turn transitions
gather\_tts = returns a table of the turn transitions detected in the speech data

n\_tts = returns the number of turn transitions in the speech data

tts\_ph = returns the rate of turn transitions per hour in the speech data

dur\_tts = returns the mean, median, standard deviation, and range of turn transition duration in the speech data

#### interactional sequences
gather\_intseq = returns a table of the interactional sequences detected in the speech data

n\_intseq = returns the number of interactional sequences in the speech data

intseq\_ph = returns the rate of interactional sequences per hour in the speech data

dur\_intseq = returns the mean, median, standard deviation, and range of interactional sequence duration in the speech data

#### LENA CTC replicator
get\_CTC = returns a conversational turn count using LENA's definition with the option of having that count over the entire file, every 5 minutes, or 1 hour.

## file input

Example of a few seconds of multi-party interaction:
![Example of a few seconds of multi-party interaction:](example-5s-interaction.png)

Canonically we might consider this brief exchange to have the following turn transitions: CHI -> MA1 and FC1 -> CHI.


Input data for the core turn-measuring functions should take one of two forms:

#### Utterance timing and addressee information
| speaker | start.ms | stop.ms | addressee |
|---------|----------|---------|-----------|
| CHI     | 450      | 1080    | MA1       |
| CHI     | 2274     | 3500    | MA1       |
| CHI     | 5251     | 5789    | FC1       |
| MA1     | 210      | 1260    | FC1       |
| MA1     | 4910     | 5256    | CHI       |
| MA1     | 5288     | 5909    | FC1       |
| FC1     | 3393     | 4971    | CHI       |

#### Utterance timing information only
| speaker | start.ms | stop.ms |
|---------|----------|---------|
| CHI     | 450      | 1080    |
| CHI     | 2274     | 3500    |
| CHI     | 5251     | 5789    |
| MA1     | 210      | 1260    |
| MA1     | 4910     | 5256    |
| MA1     | 5288     | 5909    |
| FC1     | 3393     | 4971    |

### References
VanDam, M., Warlaumont, A. S., Bergelson, E., Cristia, A., Soderstrom,
M., Palma, P. D., & MacWhinney, B. (2016). HomeBank: An online
repository of daylong child-centered audio recordings. _Seminars in
Speech and Language, 37_(2), 128-142. <doi:10.1055/s-0036-1580745>

VanDam, Mark (2018). VanDam Public 5-minute HomeBank Corpus. <doi:10.21415/T5388S>
