TypingTesterCruncher
====================

R scripts for cleaning and combining TypingTester log output

##File Structure
  -TypingTesterCruncher
    - COMPILE.R
    - CRUNCH.R
    - charClass.R
    - log.csv
    - Participant Data (nothing in here other than the subfolders)
      - iPad
      - iPhone
      - PC

##COMPILE.R

Script loops through each device data folder, loops through each participant's raw log file and applies the data CRUNCHER function, then combines each participant and each device.  Script also merges participant performance data wit the log.csv file that contains device ordering for each participant

##CRUNCH.R

Cleans a single log file and computes a summary by string of time and errors (entry and levenshtein distance) and 
individal character type times

##charClass.R

Helper function that takes a length 1 character input and classifies as a letter, number or a symbol

##log.csv

Example log file that contains participant numbers and device presentation order
