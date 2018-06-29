best=function(state,outcome){
  mortality=read.csv(file='outcome-of-care-measures.csv',stringsAsFactors = F)
  mortality=mortality[,c((1:10),seq(from=11,to=28,by=6))]
  mortality[,(11:13)]=suppressWarnings(apply(mortality[,(11:13)],2,FUN=function(x){as.numeric(x)}))
  outcome.list=c('heart attack','heart failure','pneumonia')
  if(state %in% mortality$State){
    if(outcome %in% outcome.list){
      mortality=mortality[(mortality$State==state),]#filter the rows with chosen state
      return(mortality[(which.min(mortality[,(which(outcome==outcome.list)+10)])),2])
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
# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")
# best("BB", "heart attack")
# best("NY", "hert attack")


