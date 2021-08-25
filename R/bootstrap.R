#' @import survival
#' @import foreach
#' @import snow
#' @import doSNOW

bootstrap=function(data,b0.ZM,b0.ZMY,cov.n,pred.val,int,scale,boots.n,parallel){
  boot=matrix(NA,nr=boots.n,nc=4)
  core.n=parallel::detectCores()
  
  if (parallel==T & core.n>1) {
    cr=snow::makeCluster(round(core.n*3/4))
    my_functions=c("Coef","tau","path.point")
    snow::clusterExport(cr, my_functions,envir=environment())
    doSNOW::registerDoSNOW(cr)
    
    pb=txtProgressBar(max=boots.n, style=3)
    progress=function(n){setTxtProgressBar(pb,n)}
    opts=list(progress=progress)
    
    point.boot=foreach(b=1:boots.n, .options.snow=opts, .combine=rbind, .packages="survival") %dopar% {
      set.seed(9*b)
      if(sum(data$z %in% c(1,0)) == dim(data)[1]){
        data.boot0=as.data.frame(data[data$z==0,][sample(1:nrow(data[data$z==0,]), replace=T),])
        data.boot1=as.data.frame(data[data$z==1,][sample(1:nrow(data[data$z==1,]), replace=T),])
        data.boot=rbind(data.boot0,data.boot1)
      } else {
        data.boot=as.data.frame(data[sample(1:nrow(data), replace=T),])
      }
      
      dt.boot=suppressWarnings(tryCatch(list(Coef(data.boot,b0.ZM,b0.ZMY,"m",cov.n,int)), 
                                        error = function(msg){return(list(msg = msg))}))
      af.boot=suppressWarnings(tryCatch(list(Coef(data.boot,b0.ZM,b0.ZMY,"y",cov.n,int)), 
                                        error = function(msg){return(list(msg = msg))}))
      
      if(!is.null(dt.boot$msg) | !is.null(af.boot$msg) | sum(af.boot[[1]] %in% NA)>0){
        point.boot=list(NA)
      } else {
        point.boot=path.point(m,za,zb,af.boot[[1]],dt.boot[[1]],pred.val,scale)
        return(list(point.boot))
      }
    }  
    stopCluster(cr)
    
    for(b in 1:boots.n){
      boot[b,]=point.boot[[b]]
    }
  } else {
    for(b in 1:boots.n){
      set.seed(9*b)
      if(sum(data$z %in% c(1,0)) == dim(data)[1]){
        data.boot0=as.data.frame(data[data$z==0,][sample(1:nrow(data[data$z==0,]), replace=T),])
        data.boot1=as.data.frame(data[data$z==1,][sample(1:nrow(data[data$z==1,]), replace=T),])
        data.boot=rbind(data.boot0,data.boot1)
      } else {
        data.boot=as.data.frame(data[sample(1:nrow(data), replace=T),])
      }
      
      dt.boot=suppressWarnings(tryCatch(list(Coef(data.boot,b0.ZM,b0.ZMY,"m",cov.n,int)), 
                                        error = function(msg){return(list(msg = msg))}))
      af.boot=suppressWarnings(tryCatch(list(Coef(data.boot,b0.ZM,b0.ZMY,"y",cov.n,int)), 
                                        error = function(msg){return(list(msg = msg))}))
      
      if(!is.null(dt.boot$msg) | !is.null(af.boot$msg) | sum(af.boot[[1]] %in% NA)>0){
        boot[b,]=NA
      } else {
        point.boot=path.point(m,za,zb,af.boot[[1]],dt.boot[[1]],pred.val,scale)
        boot[b,]=point.boot
      }
    }
  }
  
  pval.mean=apply(boot,2,function(x){mean(x,na.rm=T)})
  pval.var=apply(boot,2,function(x){var(x,na.rm=T)})
  pval=matrix(NA,nr=10^6,nc=4)
  for (i in 1:ncol(boot)){ 
    set.seed(20210629)
    pval.rnorm=suppressWarnings(rnorm(10^6,pval.mean[i],sqrt(pval.var[i])))
    pval[,i]=pval.rnorm
  }
  if (scale=="RD"){h0=0} else {h0=1}
  pval.boot=apply(pval,2,function(x){2*min((sum(x>h0)/length(x)),(1-(sum(x>h0)/length(x))))})
  ci.boot=t(apply(boot,2,function(x){quantile(x,probs=c(.025,.975),na.rm=T)}))
  boot.out=cbind(ci.boot,pval.boot)
}
