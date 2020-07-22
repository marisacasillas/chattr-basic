Source: [https://rdrr.io/github/HomeBankCode/DarcleITS/man/rlena.html]()

# Description
The Language Environment ANalysis (LENA) system makes automatic annotations of audio recordings of children's sound environment. Its annotations can be exported as .its files, that contain an xml structure. The rlena package makes it easy to import and work with LENA .its files in R. It does so by creating and producing tidy data frames for further analysis.

# Details
Each .its file corresponds to one audio file and contains a hierarchical structure of annotations: Short segments are labeled by speaker type and grouped into vocalization activity blocks (pauses or conversations). These blocks are again grouped into recordings (uninterrupted recording sessions).

# Recordings
A single file can contain multiple recordings. A recording corresponds to one uninterrupted recording session. When the "Pause" button on the recorder is pressed during a LENA recording session, this will create a new recording in the .its file. The gather_recordings function extracts the recording information from the file, including their start time, end time and timezone information.

# Segments
On the lowest level, the annotations that LENA makes are a continuous, non-overlapping sequence of labeled segments. Each segment corresponds to a different type of speakers or sound. Each segment has one of 10 labels:

CHN - Key Child

CXN - Other Child

FAN - Female Adult

MAN - Male Adult

OLN - Overlapping Vocals

TVN - TV / Electronic Media

NON - Noise

SIL - Silence

FUZ - Uncertain / Fuzzy

The CHN segments are processed further to provide estimates of the number of child vocalizations. The FAN and MAN segments are processed further to provide estimates of the number of adult words. Nonspeech vocalizations and vegetative sounds are excluded from both of these counts.

The segments can be obtained with the gather_segments function. Among other things, it returns information on the start and end time of each segment, the label, and (if applicable) the estimated number of adult words and child utterances.

# Vocalization Activity Blocks
In the .its file all segments are grouped into larger, non-overlapping vocalization activity blocks. There are two block types: pauses and conversations. Conversations can have different types, depending on which type of speaker initiates the converation and which types of speakers participate:

## Blocks initiated by the key child

* CM - Key Child Monologue

* CIC - Key Child with Adult

* CIOCX - Key Child with Other Child

* CIOCAX - Key Child with Adult and Other Child

## Blocks initiated by a female adult

* AMF - Female Ault Monologue

* AICF - Female Adult with Key Child

* AIOCF - Female Adult with Other Child

* AIOCCXF - Female Adult with Key Child and Other Child

## Blocks initiated by a male adult

* AMM - Male Ault Monologue

* AICM - Male Adult with Key Child

* AIOCM - Male Adult with Other Child

* AIOCCXM - Male Adult with Key Child and Other Child

## Blocks initiated by other child

* XM - Other Child Monologue

* XIOCC - Other Child with Key Child

* XIOCA - Other Child with Adult

* XIC - Other Child with Key Child and Adult (Turns)

* XIOCAC - Other Child with Key Child and Adult (No Turns)

The blocks can be obtained with gather_blocks. To get just the pause blocks, one can use gather_pauses. The get just the conversation blocks, one can use gather_conversations.

# Turn Taking Information
Some segments within Vocalization Activity Blocks have special functions related to turn taking and are marked acordingly. For example, vocalizations in a conversation can be of different types:

* FI - Floor Initiation (speaker is speaking for the first time in this block)

* FH - Floor Holding (speaker has spoken before)

When the speaker changes from one segment to the other, this will (under certain circumstances) be counted as a strong conversational turn. Conversational turns can be of different types, too:

* TIFI/TIMI - Turn Initiation with Female / Male Adult

* TIFR/TIMR - Turn Response with Female / Male Adult

* TIFE/TIME - Turn End with Female / Male Adult

* NT - Other Child with Key Child and Adult (Turns)

For more information see the "Quick Reference Sheet". It is accessible from the help menu in the ADEX software.