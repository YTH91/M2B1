#' Apply mediation meta analysis for binary outcome with one binary mediator
#' 
#' function: M2B1 for mediation analysis, M2B1.meta for mediation meta analysis 
#' @param data name of dataset. With matching data, the order of data columns: matching no, exposure, mediator, outcome, covariates. With non-matching data, the order of data columns: exposure, mediator, outcome, covariates
#' @param  b0.ZM intercept of modeling the mediator as a dependent variable, and exposure as an independent variable (for conditional logistic regression)
#' @param  b0.ZMY intercept of modeling the outcome as a dependent variable, exposure, and mediator as independent variables (for conditional logistic regression)
#' @param  cov.n number of covariates
#' @param  pred.val covariates values (default is median)
#' @param  int interaction of exposure and mediator, TRUE or FALSE
#' @param  scale arithmetic scale. "RD" for risk difference, "RR" for risk ratio, "OR" for odds ratio
#' @param  boots.n number of bootstrapping
#' @param  parallel parallel computing, TRUE or FALSE
#' @keywords Mediation analysis, Mediation meta analysis
#' @import survival
#' @import foreach
#' @import snow
#' @import doSNOW
#' @export
#' @examples 
#' # matching data
#' M2B1.meta(data=data,b0.ZM=0.01,b0.ZMY=0.02,cov.n=2,pred.val=c(1,35),int=T,scale="OR",boots.n=100,parallel=T)
#' @examples
#' # non-matching data
#' M2B1.meta(data=data,cov.n=2,pred.val=c(1,35),int=T,scale="OR",boots.n=100,parallel=F)

M2B1.meta=function(data,b0.ZM=0,b0.ZMY=0,cov.n=0,pred.val=NULL,int=F,scale,boots.n=1000,parallel=F){
  if (round(dim(data)[1]/2000)<=1){ output=M2B1(data,b0.ZM,b0.ZMY,cov.n,pred.val,int,scale,boots.n,parallel)
  } else { N=dim(data)[1]/2000 
  
  if (length(data)==3+cov.n){ 
    data.meta=split(data, sample(1:round(N), nrow(data), replace=T))
  } else if (length(data)==4+cov.n){
    data.meta=split(data, rep(1:round(N),each=dim(data)[1]/round(N),len=dim(data)[1]))
  }
  
  sub=rep(list(NA),round(N));sub.path=rep(list(matrix(NA,nr=round(N),nc=3)),4)
  for (i in 1:round(N)){
    cat(paste("\n", "Progress: ", i, "/", round(N), "\n", sep=""))
    sub[[i]]=as.matrix(M2B1(data.meta[[i]],b0.ZM,b0.ZMY,cov.n,pred.val,int,scale,boots.n,parallel))[,1:3]
    sub.path[[1]][i,]=sub[[i]][1,]; sub.path[[2]][i,]=sub[[i]][2,]
    sub.path[[3]][i,]=sub[[i]][3,]; sub.path[[4]][i,]=sub[[i]][4,]
  }
  output=meta(sub.path,scale)
  }
  cat("\n")
  if (output[4,1]<0){message("Notice: User should not report the proportion of mediation if it is less than 0.")
  } else if (!is.na(output[4,2]) & (output[4,2]<0 | output[4,3]>1)){message("Notice: 95% CI of the proportion of mediation should be modified as 0 and 1.")}
  return(output)
}