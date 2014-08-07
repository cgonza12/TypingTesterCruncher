#######################################
#### Script to compile data across devices 
#### and participants
#### Christian Gonzalez
#### Fors Marsh Group
#### August, 2014 
#######################################

# load data cruncher function
source("YOURDIRECTORYHERE/CRUNCH.R") #CHANGE THIS TO YOUR LOCATION!!!!!
source("YOURDIRECTORYHERE/charClass.R") #CHANGE THIS TO YOUR LOCATION!!!!!
# point to the directory with the three folders (only!)
setwd("YOURDIRECTORYHERE/Participant Data/") #CHANGE THIS TO YOUR LOCATION!!!!!

#load helper package
install.packages(plyr)
library(plyr)

# get folder names
folderlist = list.files()

# create empty list where each device's data will combine into

outlist1=list()

for(i in 1:length(folderlist)){
  # loop through each device folder
  # CHANGE THIS TO YOUR LOCATION!!!!!
  setwd(paste0("YOURDIRECTORYHERE/Participant Data/",folderlist[i]))
  
  # get file names out of device i's folder
  filelist = list.files()
  # only grab the raw files, not summary
  filelist = filelist[grep("raw",filelist)]
  # create empty list to combine crunched data into
  outlist2 = list()
  
  for(j in 1:length(filelist)){
    # loop through each participant's raw file in the device folder
    
    # save the device name for later
    device = folderlist[i]
    # save the participant number for later
    p = substr(filelist[j],2,4)
  
    # guts of the operation, run the data crunching function on the file
    out = CRUNCH(filelist[j],device)
    
    # repeat the device name and stick it onto the output
    out$device = rep(device,nrow(out))
    # repeat the participant number and stick it onto the output
    out$pid = rep(p,nrow(out))
    
    # put the completed file into the outlist for the device
    outlist2[[j]] = out
    # print out what iteration we're on lazy progress bar :)
    print(j)
  }
  
  # take the combined device list and stick it into the main output list
  outlist1[[i]] = do.call(rbind,outlist2)
  
  # print out what device we're on lazier progress bar :)
  print(paste0(folderlist[i]," Done!"))
}

# stick the whole thing together
out =  do.call(rbind,outlist1)

# add in SubPhase trial numbers
out = ddply(out,.(pid,SubPhase,target,device),mutate,
             SubPhasetrial = seq(1,length(SubPhase),1)
             
             )

# sort by pid
out = out[order(out$pid,out$device,out$trial),]
# reset the working directory
setwd("YOURDIRECTORYHERE/Participant Data/") #CHANGE THIS TO YOUR LOCATION!!!!!


# read in and transform log

### Below is the code to incorporate additional participant data, like pw set and order
### not neccessary for pw performance analysis 

log = read.csv("YOURDIRECTORYHERE/log.csv") #CHANGE THIS TO YOUR LOCATION!!!!!
# recode devices to numeric values
out$devnumeric = out$device 
out$devnumeric[out$devnumeric=="PC"] <- 1
out$devnumeric[out$devnumeric=="iPhone"] <- 2
out$devnumeric[out$devnumeric=="iPad"] <- 3

# add order to out
out= merge(out,log)
# match up the serial position of the device in order save that as devorder
out = ddply(out,.(pid,device),mutate,
            devorder2 = regexpr(as.character(devnumeric[1]),
                               as.character(devorder))
            )
out$devnumeric <- NULL

# reset working directory
setwd("YOURDIRECTORYHERE")#CHANGE THIS TO YOUR LOCATION!!!!!

# write a csv file of the complete output
write.csv(out,"out.csv")
