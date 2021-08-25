path.point=function(m,za,zb,af,dt,pred.val,scale){
  t10=tau(0,1,0,af,dt,pred.val)+tau(1,1,0,af,dt,pred.val)
  t00=tau(0,0,0,af,dt,pred.val)+tau(1,0,0,af,dt,pred.val)
  t11=tau(0,1,1,af,dt,pred.val)+tau(1,1,1,af,dt,pred.val)
  t10=tau(0,1,0,af,dt,pred.val)+tau(1,1,0,af,dt,pred.val)
  
  if (scale=="RD"){ ZY=t10-t00; ZMY=t11-t10; TE=t11-t00; prop=ZMY/(ZY+ZMY)
  } else if (scale=="RR"){ ZY=t10/t00; ZMY=t11/t10; TE=t11/t00; prop=log(ZMY)/log(ZY*ZMY)
  } else if (scale=="OR"){ ZY=(t10/(1-t10))/(t00/(1-t00)); ZMY=(t11/(1-t11))/(t10/(1-t10)); TE=(t11/(1-t11))/(t00/(1-t00)); prop=log(ZMY)/log(ZY*ZMY)
  }
  return(c(ZY,ZMY,TE,prop))
}
