#' @import survival

Coef=function(data,b0.ZM,b0.ZMY,outcome,cov.n,int){
  if (cov.n>0){cov=paste(paste0("x",1:cov.n),collapse="+")}
  
  if (names(data)[1]=="match" & outcome=="m"){  
    c(b0.ZM,coef(coxph(eval(parse(text=paste0("Surv(rep(1,length(m)),m)~strata(match)+z+",cov))),data=data,method="exact")))
  } else if (names(data)[1]=="match" & outcome=="y"){
    if (int==T){
      c(b0.ZMY,coef(coxph(eval(parse(text=paste0("Surv(rep(1,length(y)),y)~strata(match)+z*m+",cov))),data=data,method="exact")))
    } else {
      c(b0.ZMY,coef(coxph(eval(parse(text=paste0("Surv(rep(1,length(y)),y)~strata(match)+z+m+",cov))),data=data,method="exact")))
    }
  } else if (names(data)[1]=="z" & outcome=="m"){
    coef(glm(eval(parse(text=paste0("m~z+",cov))),data=data,family=binomial(link="logit")))
  } else if (names(data)[1]=="z" & outcome=="y"){
    if (int==T){
      coef(glm(eval(parse(text=paste0("y~z*m+",cov))),data=data,family=binomial(link="logit")))
    } else {
      coef(glm(eval(parse(text=paste0("y~z+m+",cov))),data=data,family=binomial(link="logit")))
    }
  }  
}
