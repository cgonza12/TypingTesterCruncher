#######################################
#### Function to analyze raw log data 
#### Christian Gonzalez
#### Fors Marsh Group
#### August, 2014 
#######################################

CRUNCH <- function(x,device){
  # NOTE Function is split between PC and iOS devices due to log discrepancies 
  if(device=="PC"){
  #read data
  dat = read.delim(x)
  #remove ambiguous or uninteresting phases
  dat = dat[! dat$Phase %in% c("Unknown","Instruction","ThankYou"),]
  #remove transition phases
  dat = dat[! dat$Current.Value %in% c("Subphase change","Phase Start","Phase End"),]
  
  #remove weird hiccups
  dat = dat[dat$Time!="",]
  
  #get time by converting hh:mm:ssss into seconds since start
  dat$h = as.numeric(substr(dat$Time.Since.Session.Start,1,2))*3600
  dat$m = as.numeric(substr(dat$Time.Since.Session.Start,4,5))*60
  dat$s = as.numeric(substr(dat$Time.Since.Session.Start,7,12))
  
  
  #add em up
  dat$TimeSS = dat$h+dat$m+dat$s
  
  dat$d = NULL #rm
  dat$h = NULL#rm
  dat$m = NULL#rm
  dat$s = NULL#rm
  
  
  #loop to identify sections of data where Pp is actually entering text, also creates an input number id
  # this tells us how many explicit keystrokes have occured within that string
  
  dat$keep = c()#within active text typing or not
  dat$inputnum = c()#input number id
  counter=0#counter for input number
  enter = F#has the control been activated?
  exit = F#has a correct response been made?
  
  
  for(i in 1:nrow(dat)){
    
    if(dat$Event[i]=="ControlActivated"){enter = T;exit=F} 
    
    if(enter & !exit){
      
      dat$keep[i] = 1 #keep this row
      counter = counter+1 # increment counter
      dat$inputnum[i] = counter # save current counter number as input number
    } else{
      
      dat$keep[i] = 0 # don't keep this row
      counter = 0 # reset counter
      dat$inputnum[i] = counter # save current counter number as input number
    }
    
    # different ways to exit control based on Phase, code below reflects multiple exits
    switch(as.character(dat$Phase[i]),
           Proficiency={
  if(grepl("Correct",dat$Current.Value[i]) | grepl("Incorrect",dat$Current.Value[i])){exit = T;enter=F}
           },
           Memorize ={
  if(grepl("Correct",dat$Current.Value[i])){exit = T;enter=F}          
           },
           Entry={
  if(grepl("Correct",dat$Current.Value[i]) | grepl("Incorrect",dat$Current.Value[i])){exit = T;enter=F}           
           },
           Recall={
    if(grepl("Text Box Lost Focus Active",dat$Current.Value[i])){exit = T;enter=F}           
  }
           )

  }
  
  # reduce to those with active fields
  dat = dat[dat$keep==1,]
  
  # create a target.string id 
  dat$stringnum = c(1)
  stringcount=1
  for(i in 2:nrow(dat)){
    
    if(dat$inputnum[i]>dat$inputnum[i-1]){
      # if input number is bigger than the previous, must be the same string
      dat$stringnum[i] = stringcount
    } else{
      # if not it must be different so update string count
      stringcount=stringcount+1
      dat$stringnum[i] = stringcount
    }
    
    
  }
  
  # cut out recall phase, deal with that later...
  
  recall.dat = dat[dat$Phase=="Recall",]
  
  dat = dat[dat$Phase!="Recall",]
  
  
  #set string num within Suphase
  dat$SubPhasestringnum = c(1)
  stringcount=1
  for(i in 2:nrow(dat)){
    
    if(dat$SubPhase[i]!=dat$SubPhase[i-1]){
      stringcount=0
      if(dat$inputnum[i]>dat$inputnum[i-1]){
        
        dat$SubPhasestringnum[i] = stringcount
      } else{
        
        stringcount=stringcount+1
        dat$SubPhasestringnum[i] = stringcount
      }
    } else{
      
      if(dat$inputnum[i]>dat$inputnum[i-1]){
        
        dat$SubPhasestringnum[i] = stringcount
      } else{
        
        stringcount=stringcount+1
        dat$SubPhasestringnum[i] = stringcount
      }
      
    }
    
  }
  
  #repeat target string
  Target.String2 = list()
  for(i in 1:length(unique(dat$stringnum))){
    
    x = dat[dat$stringnum==unique(dat$stringnum)[i],]
    
    w = rep(as.character(x$Target.String[as.character(x$Target.String)!=""])[1],nrow(x))
    Target.String2[[i]] = w
  }
  
  dat$Target.String = unlist(Target.String2)
  
  
  
  # compute average lev distance per target, first isolate data for only key presses
  dat.kp = dat[dat$Event=="KeyPress",]
  # remove blanks
  dat.kp = dat.kp[dat.kp$Current.Value!="",]
  
  # empty lev distance vector
  dat.kp$distvec = c()
  # empty lev distance vector without cases
  dat.kp$distvec.nocase = c()
  for(i in 1:nrow(dat.kp)){
    
    # computes levenshtien distance with partial matching
    # e.g. "CA" would be dist 0 from "CAT" 
    # this is to account for incrememntal input
    
    # case
    dat.kp$distvec[i] = adist(as.character(dat.kp$Current.Value[i]),
                            dat.kp$Target.String[i],partial=T)
    
    # no case
    dat.kp$distvec.nocase[i] = adist(as.character(dat.kp$Current.Value[i]),
                                     dat.kp$Target.String[i],partial=T,ignore.case=T)
  }
  
  # compute char input times
  dat.kp2 = dat[dat$Event=="KeyPress" | dat$Current.Value=="Next button pressed"
                |dat$Key %in% c("Captial","ShiftKey","Back") ,]
  
  dat.kp2$charTime = c()
  
  for(i in 1:nrow(dat.kp2)){
  
  if(i==1){
    dat.kp2$charTime[i] = dat.kp2$TimeSS[i]
    
  }else{
    
    dat.kp2$charTime[i] = dat.kp2$TimeSS[i] - dat.kp2$TimeSS[i-1]
    
  }
  }
  
 
  # use custom character identification function to label inputs as numbers, letters or symbols
  dat.kp2$char.Class = unlist(lapply(as.character(dat.kp2$Key),charClass))
  
  dat.charTime = dat.kp2[c("stringnum", "char.Class","charTime","Key")]
  
  # summarize time by string
  datsum.charTime = ddply(dat.charTime,.(stringnum),here(summarize),
                          ttc = sum(charTime,na.rm=T),
                          letTime = sum(charTime[char.Class=="letter"],na.rm=T),
                          numTime = sum(charTime[char.Class=="number"],na.rm=T),
                          symTime = sum(charTime[char.Class=="symbol"],na.rm=T),
                          delTime = sum(charTime[grep("Back",Key)],na.rm=T),
                          kbcTime = NA,
                          shiftTime = sum(charTime[grep("Shift",Key)],na.rm=T)
                          
                          )
  # summarize lev by string
  datsum.kp = ddply(dat.kp,.(stringnum),here(summarize),
                    avglev = mean(distvec,na.rm=T),
                    totallev = sum(distvec,na.rm=T),
                    avglev.nocase = mean(distvec.nocase,na.rm=T),
                    totallev.nocase = sum(distvec.nocase,na.rm=T)
                    
  )
  
  # summarize completion times by target strings
  datsum = (ddply(dat,.(stringnum),here(summarize),
                         Phase = Phase[1],
                         SubPhase = SubPhase[1],
                         trial = stringnum[1],
                         SubPhasetrial = SubPhasestringnum[1],
                         target = Target.String[1],
                         corr = sum(grepl("Correct",Current.Value)),
                         nIn = sum(Event=="KeyPress"),
                         nDel = sum(grepl("Back",Key)),
                         nKBc = NA,
                         nShift = sum(grepl("ShiftKey",Key) | grepl("Capital",Key)),
                         nNext =  sum(grepl("Next",Current.Value)),
                         nBack =  sum(grepl("Back",Current.Value))
                
  ))
 
  # combine lev, char time and completion time data frames
  datsum = merge(datsum,datsum.kp,all.x=T)
  datsum = merge(datsum,datsum.charTime,all.x=T)
  
  # fix weird subphase trial counter
  datsum = ddply(datsum,.(Phase,SubPhase),here(mutate),
                 SubPhasetrial = seq(1,length(corr),1)
  )
  
  ## Deal with recall data

  # break up recall strings
  recall.dat$stringnum = c()
  switcher=T
  for(i in 1:length((recall.dat$inputnum))){
    if(recall.dat$Current.Value[i]=="Text Box Got Focus : Field1"){switcher = F}
    
    if(switcher){recall.dat$stringnum[i]=max(datsum$trial)+1
    }else{
      recall.dat$stringnum[i]=max(datsum$trial)+2}
  }
  pws = unique(datsum$target[datsum$SubPhase=="ForcedPractice"])
  
  pws[1] = substr(pws[1],0,nchar(pws)-1)
  pws[2] = substr(pws[2],0,nchar(pws)-1)
  
  # summarize recal data
  datsum.recall =  (ddply(recall.dat,.(stringnum),here(summarize),
                          Phase = Phase[1],
                          SubPhase = NA,
                          trial = stringnum[1],
                          SubPhasetrial = NA,
                          target = NA,
                          corr = as.numeric(any(Current.Value %in% pws)),
                          nIn = sum(Event=="KeyPress"),
                          nDel = sum(grepl("Back",Key)),
                          nKBc = NA,
                          nShift = sum(grepl("ShiftKey",Key) | grepl("Capital",Key)),
                          nNext = NA,
                          nBack = NA,
                          avglev = NA,
                          totallev=NA,
                          avglev.nocase = NA,
                          totallev.nocase = NA,
                          ttc = TimeSS[length(stringnum)] - TimeSS[1],
                          letTime = NA,
                          numTime = NA,
                          symTime = NA,
                          delTime = NA,
                          kbcTime = NA,
                          shiftTime = NA
  ))
  
  # combine recall with other summary data frame
  datsum = rbind(datsum,datsum.recall)
  
  datsum$pworder = c()
  datsum$pwset = c()
  
  # add in pw set and pw presentation order
  for(i in 1:nrow(datsum)){
    
    if(!datsum$Phase[i] %in% c("Memorize","Entry")){
      datsum$pworder[i] = NA;datsum$pwset[i]=NA
    } else{
      
      if(datsum$target[i] %in% c("Rmofpaf2207#)^", "q80<U/C2mv")){
        datsum$pwset[i]=1
        
      }else{
        datsum$pwset[i]=2
        
      }
      
      if(datsum$target[i]==datsum$target[1]){
        datsum$pworder[i] = 1
      }else{datsum$pworder[i] = 2}
    }
    
    
  }
  
  
  
  return(datsum)
  ################################################################################################
  ####################################### same for iphone and ipad ###############################
  ################################################################################################
  }else{
    #read data
    dat = read.delim(x)
    
    dat$Phase = gsub(" Phase", "", dat$Phase)
    
    dat$SubPhase = gsub(" ","",dat$SubPhase)
    
    # remove ambigious phases
    dat = dat[dat$Phase %in% c("Memorize","Proficiency","Entry","Recall"),]
    dat = dat[dat$Event!="Sub Phase Change",]
    dat = dat[!grepl("User elected", dat$Notes),]
    dat = dat[!grepl("Leaving", dat$Notes),]
    dat[dat=="(null)"]<-""
    #remove weird hiccups
    dat = dat[dat$Time!="",]
    
    # convert to seconds
    dat$TimeSS = dat$Time.Since.Session.Start*1000
    
    # match the phase names with PC
    
    dat$Phase = gsub(" Phase", "", dat$Phase)
    
    dat$SubPhase = gsub(" ","",dat$SubPhase)
    
    dat$SubPhase = gsub("UnknownSubPhase","None",dat$SubPhase)
    
    #loop to eliminate potpurri at end of log
    
    dat$keep = c()
    enter = F#has the control been activated?
    for(i in 1:nrow(dat)){
      if(dat$Phase[i]=="Recall"){enter=T}

      
        if(!enter){
          dat$keep[i] = 1
        }else{
          if(dat$Phase[i]=="Recall"){
            dat$keep[i] = 1
          }else{
            
            if(dat$Phase[i]!="Recall" & enter){dat$keep[i] = 0}
          }
        }
      
    }
    
    dat = dat[dat$keep==1,]
    
    #loop to identify sections of data where Pp is actually entering text, also creates an input number id
    # this tells us how many explicit keystrokes have occured within that string
    
    dat$keep = c()#within active text typing or not
    dat$inputnum = c()#input number id
    counter=0#counter for input number
    enter = F#has the control been activated?
    exit = F#has a correct response been made?
    
    
    for(i in 1:nrow(dat)){
      
      if(dat$Event[i]=="Control Activated"){enter = T;exit=F} 
      
      if(enter & !exit){
        
        dat$keep[i] = 1
        counter = counter+1
        dat$inputnum[i] = counter
      } else{
        
        dat$keep[i] = 0
        counter = 0 
        dat$inputnum[i] = counter
      }
      
      switch(as.character(dat$Phase[i]),
             Proficiency={
               if(grepl("Correct",dat$Event[i]) | grepl("Incorrect",dat$Event[i])){exit = T;enter=F}
             },
             Memorize ={
               if(grepl("Correct",dat$Event[i])){exit = T;enter=F}          
             },
             Entry={
               if(grepl("Correct",dat$Event[i]) | grepl("Incorrect",dat$Event[i])){exit = T;enter=F}           
             },
             Recall={
               if(grepl("TextField No Longer Active",dat$Notes[i])){exit = T;enter=F}           
             }
      
      )
      
      
      
    }
    
    #reduce to those with active fields
    dat = dat[dat$keep==1,]
    
    #create a target.string id 
    dat$stringnum = c(1)
    stringcount=1
    for(i in 2:nrow(dat)){
      
      if(dat$inputnum[i]>dat$inputnum[i-1]){
        
        dat$stringnum[i] = stringcount
      } else{
        
        stringcount=stringcount+1
        dat$stringnum[i] = stringcount
      }
      
      
    }
    
    # cut out recall phase and save for later
    
    recall.dat = dat[dat$Phase=="Recall",]
    dat = dat[dat$Phase!="Recall",]
    
    #set string num within Suphase
    dat$SubPhasestringnum = c(1)
    stringcount=1
    for(i in 2:nrow(dat)){
      
      if(dat$SubPhase[i]!=dat$SubPhase[i-1]){
        stringcount=0
        if(dat$inputnum[i]>dat$inputnum[i-1]){
          
          dat$SubPhasestringnum[i] = stringcount
        } else{
          
          stringcount=stringcount+1
          dat$SubPhasestringnum[i] = stringcount
        }
      } else{
        
        if(dat$inputnum[i]>dat$inputnum[i-1]){
          
          dat$SubPhasestringnum[i] = stringcount
        } else{
          
          stringcount=stringcount+1
          dat$SubPhasestringnum[i] = stringcount
        }
        
      }
      
    }
    
    #rep target string
    Target.String2 = list()
    for(i in 1:length(unique(dat$stringnum))){
      
      x = dat[dat$stringnum==unique(dat$stringnum)[i],]
      
      w = rep(as.character(x$Target.String[as.character(x$Target.String)!=""])[1],nrow(x))
      Target.String2[[i]] = w
    }
    
    dat$Target.String = unlist(Target.String2)
    
    
    #compute average lev distance per target
    dat.kp = dat[dat$Event=="Input",]
    dat.kp = dat.kp[as.character(dat.kp$Current.Value)!="",]
    
    dat.kp$distvec = c()
    dat.kp$distvec.nocase = c()
    for(i in 1:nrow(dat.kp)){
      
    
      dat.kp$distvec[i] = adist(as.character(dat.kp$Current.Value[i]),
                               dat.kp$Target.String[i],partial=T)
        

      dat.kp$distvec.nocase[i] = adist(as.character(dat.kp$Current.Value[i]),
                                       dat.kp$Target.String[i],partial=T,ignore.case=T)


    }
    
    dat.kp2 = dat[dat$Event %in% c("Input","Special Key Pressed") | dat$Notes=="Next button pressed" ,]
    
    dat.kp2$charTime = c()
    
    for(i in 1:nrow(dat.kp2)){
      
      if(i==1){
        dat.kp2$charTime[i] = dat.kp2$TimeSS[i]
        
      }else{
        
        dat.kp2$charTime[i] = dat.kp2$TimeSS[i] - dat.kp2$TimeSS[i-1]
        
      }
    }
    
dat.kp2 = dat.kp2[dat.kp2$Notes!="",]
in.l = strsplit(as.character(dat.kp2$Notes),",")

f2 = function(x){

  x[[1]]
}

    dat.kp2$input = unlist(lapply(in.l,f2))


    dat.kp2$input = gsub(pattern=" entered",replacement="",x= dat.kp2$input)
    dat.kp2$char.Class = unlist(lapply(as.character(dat.kp2$input),charClass))
    
    dat.charTime = dat.kp2[c("stringnum", "char.Class","charTime","input")]
    
    datsum.charTime = ddply(dat.charTime,.(stringnum),here(summarize),
                            ttc = sum(charTime,na.rm=T),
                            letTime = sum(charTime[char.Class=="letter"],na.rm=T),
                            numTime = sum(charTime[char.Class=="number"],na.rm=T),
                            symTime = sum(charTime[char.Class=="symbol"],na.rm=T),
                            delTime = sum(charTime[grep("Delete",input)],na.rm=T),
                            kbcTime = sum(charTime[grep(" Keyboard Change",input)],na.rm=T),
                            shiftTime = sum(charTime[grep(" Shift",input)],na.rm=T)
                            
    )
    
    datsum.kp = ddply(dat.kp,.(stringnum),here(summarize),
                      avglev = mean(distvec,na.rm=T),
                      totallev = sum(distvec,na.rm=T),
                      avglev.nocase = mean(distvec.nocase,na.rm=T),
                      totallev.nocase = sum(distvec.nocase,na.rm=T)
                      
    )
    #summarize completion times by target strings
    datsum = ddply(dat,.(stringnum),here(summarize),
                    Phase = Phase[1],
                    SubPhase = SubPhase[1],
                    trial = stringnum[1],
                    SubPhasetrial = SubPhasestringnum[1],
                    target = Target.String[1],
                    corr = sum(grepl("Correct",Event)),
                    nIn = sum(Event=="Input"),
                    nDel = sum(Event=="Special Key Pressed" & grepl("Delete",Notes)),
                    nKBc = sum(Event=="Special Key Pressed" & grepl("Keyboard",Notes)),
                    nShift = sum(Event=="Special Key Pressed" & grepl("Shift",Notes)),
                    nNext =  sum(grepl("Next",Notes)),
                    nBack =  sum(grepl("Back",Notes))
                  
                    
    )
    
    datsum = merge(datsum,datsum.kp,all.x=T)
    datsum = merge(datsum,datsum.charTime,all.x=T)
    
    #fix weird subphase trial counter
    datsum = ddply(datsum,.(Phase,SubPhase),here(mutate),
                   SubPhasetrial = seq(1,length(corr),1)
    )
    
    #break up recall strings
    recall.dat$stringnum = c()
    switcher=T
    for(i in 1:length((recall.dat$inputnum))){
      if(as.character(recall.dat$Notes[i])=="TextField Became Active : Field 1"){switcher = F}
      
      if(switcher){recall.dat$stringnum[i]=max(datsum$trial)+1
      }else{
        recall.dat$stringnum[i]=max(datsum$trial)+2}
    }
    pws = unique(datsum$target[datsum$SubPhase=="ForcedPractice"])
    
    pws[1] = substr(pws[1],0,nchar(pws)-1)
    pws[2] = substr(pws[2],0,nchar(pws)-1)
    datsum.recall =  (ddply(recall.dat,.(stringnum),here(summarize),
                            Phase = Phase[1],
                            SubPhase = NA,
                            trial = stringnum[1],
                            SubPhasetrial = NA,
                            target = NA,
                            corr = as.numeric(any(Current.Value %in% pws)),
                            nIn = sum(Event=="Input"),
                            nDel = sum(Event=="Special Key Pressed" & grepl("Delete",Notes)),
                            nKBc = sum(Event=="Special Key Pressed" & grepl("Keyboard",Notes)),
                            nShift = sum(Event=="Special Key Pressed" & grepl("Shift",Notes)),
                            nNext =  NA,
                            nBack =  NA,
                            avglev = NA,
                            totallev=NA,
                            avglev.nocase = NA,
                            totallev.nocase = NA,
                            ttc = TimeSS[length(stringnum)] - TimeSS[1],
                            letTime = NA,
                            numTime = NA,
                            symTime = NA,
                            delTime = NA,
                            kbcTime = NA,
                            shiftTime = NA
    ))
    datsum = rbind(datsum,datsum.recall)
    
datsum$pworder = c()
datsum$pwset = c()


for(i in 1:nrow(datsum)){

  if(!datsum$Phase[i] %in% c("Memorize","Entry")){
    datsum$pworder[i] = NA;datsum$pwset[i]=NA
  } else{
    
    if(datsum$target[i] %in% c("Rmofpaf2207#)^", "q80<U/C2mv")){
      datsum$pwset[i]=1

    }else{
      datsum$pwset[i]=2

    }
    
    if(datsum$target[i]==datsum$target[1]){
      datsum$pworder[i] = 1
    }else{datsum$pworder[i] = 2}
  }
  
  
}
    
    return(datsum)
  }
  
}