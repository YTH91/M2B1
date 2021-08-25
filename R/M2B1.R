#' Apply mediation analysis for binary outcome with one binary mediator
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
#' M2B1(data=data,b0.ZM=0.01,b0.ZMY=0.02,cov.n=2,pred.val=c(1,35),int=T,scale="OR",boots.n=100,parallel=T)
#' @examples
#' # non-matching data
#' M2B1(data=data,cov.n=2,pred.val=c(1,35),int=T,scale="OR",boots.n=100,parallel=F)

M2B1=function(data,b0.ZM=0,b0.ZMY=0,cov.n=0,pred.val=NULL,int=F,scale,boots.n=1000,parallel=F){
  data=check.data(data,cov.n,pred.val,int)
  if (is.null(pred.val)) {pred.val=apply(data[,((length(data)-cov.n+1):length(data))],2,function(x){median(x,na.rm=T)})}
  
  dt=tryCatch(list(Coef(data,b0.ZM,b0.ZMY,"m",cov.n,int)), 
              error = function(msg){return(list(msg = msg))}, 
              warning = function(msg){suppressWarnings(list(Coef(data,b0.ZM,b0.ZMY,"m",cov.n,int)))} )
  af=tryCatch(list(Coef(data,b0.ZM,b0.ZMY,"y",cov.n,int)), 
              error = function(msg){return(list(msg = msg))}, 
              warning = function(msg){suppressWarnings(list(Coef(data,b0.ZM,b0.ZMY,"y",cov.n,int)))})
  
  point=path.point(m,za,zb,af[[1]],dt[[1]],pred.val,scale)
  boot.out=bootstrap(data,b0.ZM,b0.ZMY,cov.n,pred.val,int,scale,boots.n,parallel)
  
  out=as.data.frame(cbind(point,boot.out))
  colnames(out)=c(scale,c("lb","ub","pval"))
  rownames(out)=c("Z->Y","Z->M->Y","TE","prop")
  
  return(out)
}
