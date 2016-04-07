require("DEoptim")
require("ggplot2")

source("miniXSLdesign.R") # human data, trial orders, and plotting functions
source("trueswell2012.R")

# try fitting by condition


fit_exp1 <- function(par, cond='all') {
  if(cond!='all') {
    hum = subset(exp1hum, overlap==cond)
  } else {
    hum = exp1hum
  }
  mod = get_model_prediction_exp1(par, cond)
  sse = sum((mod$perf - hum$perf)^2)
  return(sse)
}

fit_exp2 <- function(par, cond='all') {
  if(cond!='all') {
    hum = subset(exp2hum, condition==cond)
  } else {
    hum = exp2hum
  }
  mod = get_model_prediction_exp2(par, cond)
  sse = sum((mod$perf - hum$perf)^2)
  return(sse)
}


# try the guess-and-test and trueswell2012 models (need to run 1000 times per evaluation)
get_model_prediction_exp1 <- function(par, cond='all', Nsubj=2000) {
  hp = matrix(0, nrow=Nsubj, ncol=6)
  mp = matrix(0, nrow=Nsubj, ncol=7)
  lp = matrix(0, nrow=Nsubj, ncol=8)
  for(i in 1:Nsubj) {
    mh = model(par, ord=high_olap) # reps and test_noise
    hp[i,] = mh$perf
    mmm = mh$matrix
    hp[i,2:4] = rowSums(mmm[2:4,2:4]) / (rowSums(mmm[2:4,])+1e-9) # don't need to normalize since there's at most 1...(and otherwise 0)
    
    mm = model(par, ord=med_olap) # reps and test_noise
    mp[i,] = mm$perf
    mmm = mm$matrix
    mp[i,2:3] = rowSums(mmm[2:3,2:3]) #/ (rowSums(mmm[2:3,])+1e-9)
    mp[i,4:5] = rowSums(mmm[4:5,4:5]) #/ (rowSums(mmm[4:5,])+1e-9)
    mp[i,6:7] = rowSums(mmm[6:7,6:7]) #/ (rowSums(mmm[6:7,])+1e-9)
    
    ml = model(par, ord=low_olap)
    lp[i,] = ml$perf
    mmm = ml$matrix
    lp[i,3:5] = rowSums(mmm[3:5,3:5]) #/ (rowSums(mmm[3:5,])+1e-9)
    lp[i,6:8] = rowSums(mmm[6:8,6:8]) #/ (rowSums(mmm[6:8,])+1e-9)
  }
  hp = colSums(hp) / Nsubj
  mp = colSums(mp)/ Nsubj
  lp = colSums(lp) / Nsubj
  
  mod_hi = data.frame(overlap=rep("High",3), type=c("Repeated","Backward","Forward"), perf=c(mean(hp[2:4]), hp[5], hp[6]))
  mod_m = data.frame(overlap=rep("Medium",3), type=c("Repeated","Backward","Forward"), perf=c(mean(mp[2:3]), mean(mp[4:5]), mean(mp[6:7])))
  mod_lo = data.frame(overlap=rep("Low",3), type=c("Repeated","Backward","Forward"), perf=c(lp[2], mean(lp[3:5]), mean(lp[6:8])))
  
  if(cond=="High") {
    return(mod_hi)
  } else if(cond=="Medium") {
    return(mod_m)
  } else if(cond=="Low") {
    return(mod_lo)
  }
  exp1mod = rbind(mod_lo, mod_m, mod_hi)
  return(exp1mod)
}

get_model_prediction_exp2 <- function(par, cond='all', Nsubj=2000) {
  hp = matrix(0, nrow=Nsubj, ncol=6)
  mp = matrix(0, nrow=Nsubj, ncol=6)
  lp = matrix(0, nrow=Nsubj, ncol=9)
  for(i in 1:Nsubj) {
    mh = model(par, ord=high_olap) # reps and test_noise
    hp[i,] = mh$perf
    mmm = mh$matrix
    hp[i,2:4] = rowSums(mmm[2:4,2:4]) #/ (rowSums(mmm[2:4,])+1e-9)
    
    mm = model(par, ord=fam_con) # familiar context
    mp[i,] = mm$perf
    mmm = mm$matrix
    mp[i,2:4] = rowSums(mmm[2:4,2:4]) #/ (rowSums(mmm[2:4,])+1e-9)
    
    ml = model(par, ord=nov_con)
    lp[i,] = ml$perf
    mmm = ml$matrix
    lp[i,2:4] = rowSums(mmm[2:4,2:4]) #/ (rowSums(mmm[2:4,])+1e-9)
    lp[i,7:9] = rowSums(mmm[7:9,7:9]) #/ (rowSums(mmm[7:9,])+1e-9)
  }
  hp = colSums(hp) / Nsubj
  mp = colSums(mp)/ Nsubj
  lp = colSums(lp) / Nsubj
  
  mod_hi = data.frame(condition=rep("High Overlap",3), type=c("Repeated","Backward","Forward"), perf=c(mean(hp[2:4]), hp[5], hp[6]))
  mod_m = data.frame(condition=rep("Familiar Context",3), type=c("Repeated","Backward","Forward"), perf=c(mean(mp[2:4]), mp[5], mp[6])) 
  mod_lo = data.frame(condition=rep("Novel Context",4), type=c("Repeated","Backward","Forward", "Novel"), perf=c(mean(lp[2:4]), lp[5], lp[6], mean(lp[7:9])))
  
  if(cond=="High Overlap") {
    return(mod_hi)
  } else if(cond=="Familiar Context") {
    return(mod_m)
  } else if(cond=="Novel Context") {
    return(mod_lo)
  }
  exp2mod = rbind(mod_hi, mod_m, mod_lo)
  return(exp2mod)
}

fit_hypoth_model_exp1_cond <- function(modelname, lower, upper) {
  source(paste(modelname,".R",sep=''))
  optpar = DEoptim.control(reltol=.001, steptol=50, itermax=300, trace=10)
  
  best_lo = DEoptim(fn=fit_exp1, lower=lower, upper=upper, optpar, cond='Low')
  print(paste("low olap SSE:",best_lo$optim$bestval,"parms:")) 
  print(best_lo$optim$bestmem)
  best_med = DEoptim(fn=fit_exp1, lower=lower, upper=upper, optpar, cond='Medium')
  print(paste("med olap SSE:",best_med$optim$bestval,"parms:")) 
  print(best_med$optim$bestmem)      
  best_hi = DEoptim(fn=fit_exp1, lower=lower, upper=upper, optpar, cond='High')
  print(paste("high olap SSE:",best_hi$optim$bestval,"parms:"))
  print(best_hi$optim$bestmem) 
  
  exp1mod_lo = get_model_prediction_exp1(best_lo$optim$bestmem)
  exp1mod_med = get_model_prediction_exp1(best_med$optim$bestmem)
  exp1mod_hi = get_model_prediction_exp1(best_hi$optim$bestmem)
  
  exp1mod = rbind(subset(exp1mod_lo, overlap=="Low"), subset(exp1mod_med, overlap=="Medium"), subset(exp1mod_hi, overlap=="High"))
  plot_miniXSL(exp1mod, exp1hum, paste("miniXSL_Exp1_",modelname,"_fit_by_cond.pdf",sep=''), best_lo$optim$bestval + best_med$optim$bestval + best_hi$optim$bestval)
  
  plot_miniXSL(exp1mod_lo, exp1hum, paste("miniXSL_Exp1_",modelname,"_fit_low_olap.pdf",sep=''), best_lo$optim$bestval)
  plot_miniXSL(exp1mod_med, exp1hum, paste("miniXSL_Exp1_",modelname,"_fit_med_olap.pdf",sep=''), best_med$optim$bestval)
  plot_miniXSL(exp1mod_hi, exp1hum, paste("miniXSL_Exp1_",modelname,"_fit_high_olap.pdf",sep=''), best_hi$optim$bestval)
  
}

fit_hypoth_model_exp2_cond <- function(plot_fname, lower, upper) {
  source(paste(modelname,".R",sep=''))
  optpar = DEoptim.control(reltol=.001, steptol=80, itermax=300, trace=10)
  best_fam = DEoptim(fn=fit_exp2, lower=lower, upper=upper, cond='Familiar Context', optpar) 
  print(paste("fam con SSE:",best_fam$optim$bestval))
  print(best_fam$optim$bestmem)
  best_nov = DEoptim(fn=fit_exp2, lower=lower, upper=upper, cond='Novel Context', optpar)
  print(paste("nov con SSE:",best_nov$optim$bestval)) 
  print(best_nov$optim$bestmem)
  best_hi = DEoptim(fn=fit_exp2, lower=lower, upper=upper, cond='High Overlap', optpar)
  print(paste("high olap SSE:",best_hi$optim$bestval)) 
  print(best_hi$optim$bestmem)
  
  exp2mod_fam = get_model_prediction_exp2(best_fam$optim$bestmem)
  exp2mod_nov = get_model_prediction_exp2(best_nov$optim$bestmem)
  exp2mod_hi = get_model_prediction_exp2(best_hi$optim$bestmem)
  
  exp2mod = rbind(subset(exp2mod_fam, condition=="Familiar Context"), subset(exp2mod_nov, condition=="Novel Context"), subset(exp2mod_hi, condition=="High Overlap"))
  plot_miniXSL_Exp2(exp2mod, exp2hum, paste("miniXSL_Exp2_",modelname,"_fit_by_cond.pdf",sep=''), best_hi$optim$bestval + best_fam$optim$bestval + best_nov$optim$bestval)
  
  plot_miniXSL_Exp2(exp2mod_fam, exp2hum, paste("miniXSL_Exp2_",modelname,"_fit_Fam_Con.pdf",sep=''), best_fam$optim$bestval)
  plot_miniXSL_Exp2(exp2mod_nov, exp2hum, paste("miniXSL_Exp2_",modelname,"_fit_Nov_Con.pdf",sep=''), best_nov$optim$bestval)
  plot_miniXSL_Exp2(exp2mod_hi, exp2hum, paste("miniXSL_Exp2_",modelname,"_fit_high_olap.pdf",sep=''), best_hi$optim$bestval)
  return(exp2mod)
}



tru_exp1_fit = fit_hypoth_model_exp1_cond("trueswell2012", lower=c(.001,.001), upper=c(1,1))
# low olap  SSE: 0.0183  c(0.769, 0.573)
# med olap  SSE: 0.00024 c(0.879, 0.222)
# high olap SSE: 0.0125  c(0.692, 0.816) total by cond SSE: .031
# get_model_prediction_exp1(c(.826, .147)) # SSE = .04 (Exp 1 all together)


tru_exp2_fit = fit_hypoth_model_exp2_cond("trueswell2012", lower=c(.001,.001), upper=c(1,1))
# fam con  SSE: 0.074  c(0.668, 0.162)
# nov con  SSE: 0.053  c(0.747, 0.067)
# hi olap  SSe: 0.073  c(0.938, 0.089)  total by cond SSE: .2
# get_model_prediction_exp2(c(.843, .018)) # SSE = .233 (Exp 2 all together)
