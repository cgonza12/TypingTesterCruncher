#########################################################################
#### Function to classify Length 1 strings as numbers, letters or symbols
#### Christian Gonzalez
#### Fors Marsh Group
#### August, 2014 
#########################################################################
charClass = function(x){
  
  if(nchar(as.character(x))>1){
    
    return("nonCharKey")
    
  }else{
    if(x==""| x==" " | x=="\b"){
      
      return("space")
    }else{
    
  if(tolower(x) %in% letters){
    return("letter")
    
  }else{
    if(suppressWarnings(!is.na(as.numeric(x)) & is.numeric(as.numeric(x)))){
      
      return("number")
    }else{
      
      return("symbol")
    }
      
  }
    
  }
   
  }
  
}

