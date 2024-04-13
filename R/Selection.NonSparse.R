
Selection.NonSparse=function(obj, prob=0.95){
  
  BI=obj$burn.in
  
  GS.beta = obj$posterior$GS.G
  GS.beta = GS.beta[-c(1:BI),]
  
  coef_G = c()
  
  for(j in 1:ncol(GS.beta))
  {
    t12 = GS.beta[,j]
    coef_G = c(coef_G,quanfun(t12,prob))
  }
  
  
  names(coef_G) = names(obj$coefficient$G)
  
  Main.G=coef_G
  method = paste(prob*100,"% credible interval", sep = "")
  
  out = list(method=method, Main.G=Main.G)
  class(out) = "Selection"
  out
  
}
