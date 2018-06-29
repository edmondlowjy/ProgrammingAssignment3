rankhospital=function(state,outcome,num='best'){
  mortality=read.csv(file='outcome-of-care-measures.csv',stringsAsFactors = F)
  mortality=mortality[,c((1:10),seq(from=11,to=28,by=6))]
  mortality[,(11:13)]=suppressWarnings(apply(mortality[,(11:13)],2,FUN=function(x){as.numeric(x)}))
  outcome.list=c('heart attack','heart failure','pneumonia')
  if(state %in% mortality$State){
    if(outcome %in% outcome.list){
      mortality=mortality[(mortality$State==state),]#filter the rows with chosen state
      mortality=mortality[(order(mortality[,(which(outcome==outcome.list)+10)],
                                 mortality$Hospital.Name)),]#sort in increasing mortality followed by name
      mortality=mortality[complete.cases(mortality[,(which(outcome==outcome.list)+10)]),]#remove NAs for that outcome
      if(num=='best'){
        return(mortality$Hospital.Name[1])
      }
      else if(num=='worst'){
        return(tail(mortality$Hospital.Name,1))
      }
      else if(!is.numeric(num)){
        stop("invalid rank")
      }
      else{
        return(mortality$Hospital.Name[num])
      }
    }
    else{
      stop("invalid outcome")
    }
  }
  else{
    stop("invalid state")
  }
}


###Test Cases###

# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)


