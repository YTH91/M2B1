tau=function(m,za,zb,af,dt,pred.val){
  if (substr(names(af)[length(af)-1],1,1)=="x"){
    if (substr(names(af)[length(af)],1,1)=="z"){
      ((exp(af%*%c(1,za,m,pred.val,za*m)))/(1+exp(af%*%c(1,za,m,pred.val,za*m))))*((exp(dt%*%c(1,zb,pred.val)*m))/(1+exp(dt%*%c(1,zb,pred.val))))
    } else {
      ((exp(af%*%c(1,za,m,pred.val)))/(1+exp(af%*%c(1,za,m,pred.val))))*((exp(dt%*%c(1,zb,pred.val)*m))/(1+exp(dt%*%c(1,zb,pred.val))))
    }
  } else {
    if (substr(names(af)[length(af)],1,1)=="z"){
      ((exp(af%*%c(1,za,m,za*m)))/(1+exp(af%*%c(1,za,m,za*m))))*((exp(dt%*%c(1,zb)*m))/(1+exp(dt%*%c(1,zb))))
    } else {
      ((exp(af%*%c(1,za,m)))/(1+exp(af%*%c(1,za,m))))*((exp(dt%*%c(1,zb)*m))/(1+exp(dt%*%c(1,zb))))
    }
  }
}
