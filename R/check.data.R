check.data=function(data,cov.n,pred.val,int){
  data=as.data.frame(data)
  name=c("match","z","m","y",paste0("x",1:cov.n))
  
  if (length(data)==4+cov.n){ data=data[order(data[,1]),]
  if (cov.n>0){ colnames(data)=name; data=data[,name]
  } else if (cov.n==0){ colnames(data)=name[1:4]; data=data[,name[1:4]]}
  
  } else if (length(data)==3+cov.n){
    if (cov.n>0){ colnames(data)=name[-1]; data=data[,name[-1]]
    } else if (cov.n==0){ colnames(data)=name[2:4]; data=data[,name[2:4]]} 
    
  } else {stop(paste0("The number of data columns should be ", 3+cov.n ," for non-matching data or ", 4+cov.n, " for matching data."))}
  
  if (sum(data$z %in% c(1,0)) != dim(data)[1]){stop("Exposure should be 0/1.")}
  if (sum(data$m %in% c(1,0)) != dim(data)[1]){stop("Mediator should be 0/1.")}
  if (sum(data$y %in% c(1,0)) != dim(data)[1]){stop("Outcome should be 0/1.")}
  
  data=data[complete.cases(data),]
}
