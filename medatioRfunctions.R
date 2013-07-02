region.significance = function(dependent,predictor,moderator,z){
  mod = lm(dependent~predictor + moderator + predictor:moderator)
  gam1.v = vcov(mod)["predictor","predictor"]
  gam3.v = vcov(mod)["predictor:moderator","predictor:moderator"]
  gam1gam3.cv = vcov(mod)["predictor","moderator"]
  df = length(dependent)-length(coef(mod))
  se.w1 = sqrt(gam1.v + 2*z*gam1gam3.cv + z^2*gam3.v)
  w1.hat = coef(mod)["predictor"]+coef(mod)["predictor:moderator"]*z
  p.value = (1-pt(w1.hat/se.w1,df=df))*2 
  w1.tab = cbind(w1.hat,se.w1,z,p.value)
  rownames(w1.tab) = "Moderator"
  colnames(w1.tab) = c("Est","SE","Z","p-value")
  w1.tab
}


johnson.neyman.reg = function(dependent,predictor,moderator){
  mod = lm(dependent~predictor + moderator + predictor:moderator)
  gam1.v = vcov(mod)["predictor","predictor"]
  gam3.v = vcov(mod)["predictor:moderator","predictor:moderator"]
  gam1gam3.cv = vcov(mod)["predictor","moderator"]
  df = length(dependent)-length(coef(mod))
  t = qt(.975,df)
  z = seq(min(moderator),max(moderator),by=.01)
  se.w1 = sqrt(gam1.v + 2*z*gam1gam3.cv + z^2*gam3.v)
  w1.hat = coef(mod)["predictor"]+coef(mod)["predictor:moderator"]*z
  z.tab = cbind(z,t<abs(w1.hat/se.w1))  
  ci.low = w1.hat - t*se.w1
  ci.upp = w1.hat + t*se.w1
  w1.tab = data.frame(w1.hat,z=z.tab[,1],z.tab[,2],ci.low,ci.upp)
  colnames(w1.tab) = c("Est","Z","Significant","95_LB", "95_UB")
  w1.tab[,3] = ifelse(w1.tab[,3]=="1","Yes","No")
  w1.tab
}