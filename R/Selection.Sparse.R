
Selection.Sparse=function(obj){
  BI=obj$burn.in
  
  GS.beta = obj$posterior$GS.G
  GS.beta = GS.beta[-c(1:BI),]
  
  coef_G = c()
  
  for(j in 1:ncol(GS.beta))
  {
    t12 = GS.beta[,j]
    t12[t12!=0]=1; t12[t12==0]=0
    q_beta = mpm(t12)
    coef_G = c(coef_G,q_beta)
  }
 
  
  names(coef_G) = names(obj$coefficient$G)
  
  Main.G=coef_G
  method = paste("posterior inclusion Proportion", sep = "")
  
  out = list(method=method, Main.G=Main.G)
  class(out) = "Selection"
  out
}

