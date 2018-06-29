rankall=function(outcome,num='best'){
  mortality=read.csv(file='outcome-of-care-measures.csv',stringsAsFactors = F)
  mortality=mortality[,c((1:10),seq(from=11,to=28,by=6))]
  mortality[,(11:13)]=suppressWarnings(apply(mortality[,(11:13)],2,FUN=function(x){as.numeric(x)}))
  mortality=split(mortality,mortality$State)
  outcome.list=c('heart attack','heart failure','pneumonia')
  rankstate=function(df,index){
    df=df[(order(df[,index],df$Hospital.Name)),]#sort in increasing mortality followed by name
    df=df[complete.cases(df[,index]),]#remove NAs for that outcome
    return(df)
  }
  if(outcome %in% outcome.list){
    df.index=which(outcome==outcome.list)+10
    mortality=lapply(mortality,FUN=function(x){rankstate(x,df.index)})
    if(num=='best'){
      #retrieve best in each df
      hospital=sapply(mortality,FUN=function(x){head(x$Hospital.Name,1)})
      state=sapply(mortality,FUN=function(x){head(x$State,1)})
      return(data.frame(cbind(hospital,state)))
    }
    else if(num=='worst'){
      #retrieve worst in each df
      hospital=sapply(mortality,FUN=function(x){tail(x$Hospital.Name,1)})
      state=sapply(mortality,FUN=function(x){tail(x$State,1)})
      return(data.frame(cbind(hospital,state)))
    }
    else if(!is.numeric(num)){
      stop('invalid rank')
    }
    else{
      #retrieve num in each df
      hospital=sapply(mortality,FUN=function(x){x$Hospital.Name[num]})
      state=sapply(mortality,FUN=function(x){head(x$State,1)})
      return(data.frame(cbind(hospital,state)))
    }
  }
  else{
    stop("invalid outcome")
  }
}


## Test Cases##
# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)


