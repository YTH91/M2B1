meta=function(sub.path,scale){
  output=matrix(NA,nr=4,nc=4)
  for (m in 1:3){
    if (sum(is.na(sub.path[[m]][,1]))>0){sub.path[[m]]=sub.path[[m]][-which(is.na(sub.path[[m]][,1])),]}
    if (scale!="RD"){ sub.path[[m]]=log(sub.path[[m]])}
    se=(sub.path[[m]][,3]-sub.path[[m]][,2])/3.92
    w.IV=1/se^2
    w.IV[which(w.IV=="Inf")]=10^(-6)
    theta.IV=sum(sub.path[[m]][,1]*w.IV)/sum(w.IV)
    
    Q=sum(w.IV*(sub.path[[m]][,1]-theta.IV)^2)
    tau2=(Q-(dim(sub.path[[m]])[1]-1))/(sum(w.IV)-(sum(w.IV^2)/sum(w.IV)))
    tau2=ifelse(tau2<0,0,tau2)
    
    w.DL=1/(se^2+tau2^2)
    theta.DL=sum(sub.path[[m]][,1]*w.DL)/sum(w.DL)
    lb=theta.DL-1.96*sqrt(1/sum(w.DL))
    ub=theta.DL+1.96*sqrt(1/sum(w.DL))
    pval.meta=2*pnorm(-abs(theta.DL/sqrt(1/sum(w.DL))))
    
    if (scale=="RD"){ 
      output[m,]=c(theta.DL,lb,ub,pval.meta)
      prop.meta=output[2,1]/(output[1,1]+output[2,1])
    } else {
      output[m,]=c(exp(theta.DL),exp(lb),exp(ub),pval.meta)
      prop.meta=log(output[2,1])/log(output[1,1]*output[2,1])
    }
  }  
  
  output[4,]=c(prop.meta,NA,NA,NA)
  colnames(output)=c(scale,c("lb","ub","pval"))
  rownames(output)=c("Z->Y","Z->M->Y","TE","prop")
  return(output)
}