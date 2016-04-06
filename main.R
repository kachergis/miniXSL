# final mini-CSL modeling
require("DEoptim")
require("ggplot2")

# just let the fam vs. uncertainty and decay parameters vary
fit_exp1_fixed_learn_decision_parm <- function(par, regularize=T) {
  # par = [l_h, m_h,   l_f, m_f,   l_n, m_n,  x, tao]
  sse = 0
  conds = unique(exp1hum$overlap)
  parm = matrix(par[1:6], nrow=3, byrow=T)
  chi = par[7]
  tao = par[8]
  for(i in 1:length(conds)) {
    cond = conds[i]
    hum = subset(exp1hum, overlap==cond)
    mod = get_model_prediction_exp1(c(chi, parm[i,], tao), cond)
    sse = sse + sum((mod$perf - hum$perf)^2)
  }
  # regularize? punish for widely-varying parameters
  if(regularize) sse = sse + .004*(var(parm[,1]) + var(parm[,2]))
  return(sse)
}


fit_exp2_fixed_learn_decision_parm <- function(par, regularize=T) {
  # par = [l_h, m_h, l_f, m_f, l_n, m_n,  chi, tao]
  sse = 0
  conds = unique(exp2hum$condition)
  parm = matrix(par[1:6], nrow=3, byrow=T)
  chi = par[7]
  tao = par[8]
  for(i in 1:length(conds)) {
    cond = conds[i]
    hum = subset(exp2hum, condition==cond)
    mod = get_model_prediction_exp2(c(chi, parm[i,], tao), cond)
    sse = sse + sum((mod$perf - hum$perf)^2)
  }
  # regularize? punish for widely-varying parameters
  if(regularize) 
    sse = sse + .004*(var(parm[,1]) + var(parm[,2]))
  return(sse)
}


get_exp1_human_perf <- function() {
  overlap = c("Low","Low","Low", "Medium","Medium","Medium", "High","High","High")
  type = rep(c("Repeated", "Backward", "Forward"), 3)
  perf = c(.56,.56,.63, .68,.47,.58, .74,.25,.50)
  dat = data.frame(cbind(overlap, type, perf))
  dat$perf = as.numeric(as.character(dat$perf))
  dat$overlap = as.character(dat$overlap)
  return(dat)
}

get_exp2_human_perf <- function() {
  # could also try the high and low memory split...
  condition = c("High Overlap","High Overlap","High Overlap", "Familiar Context","Familiar Context","Familiar Context", 
                "Novel Context","Novel Context","Novel Context","Novel Context")
  type = c(rep(c("Repeated", "Backward", "Forward"), 3), "Novel")
  perf = c(.84,.33,.71, .79,.47,.64, .68,.65,.46, .58)
  Low = c(.77,.12,.62,  .80,.48,.52, .65,.58,.44, .54)
  High = c(.91,.54,.79, .79,.45,.77, .72,.73,.47, .62)
  dat = data.frame(cbind(condition, type, perf, Low, High))
  dat$Low = as.numeric(as.character(dat$Low))
  dat$High = as.numeric(as.character(dat$High))
  dat$perf = as.numeric(as.character(dat$perf))
  return(dat)
}


exp1hum = get_exp1_human_perf()
exp2hum = get_exp2_human_perf()


# Exp1 trial orderings
# 1 is a new object, 2 is repeated (A), 5 is backward (D), 6 is forward (E) (6 and 7 in med olap, 6,7,8 in low olap)
high_olap = matrix(c(2,3,4,5, 2,3,4,6), nrow=2, ncol=4, byrow=T) # 1=novel, 2,3,4=repeated, 5=backward, 6=forward
med_olap  = matrix(c(2,3,4,5, 2,3,6,7), nrow=2, ncol=4, byrow=T) # 1=novel, 2,3=repeated, 4,5=backward, 6,7=forward
low_olap  = matrix(c(2,3,4,5, 2,6,7,8), nrow=2, ncol=4, byrow=T) # 1=novel, 2=repeated, 3,4,5=backward, 6,7,8=forward

#high_olap_test = c(1,2,3,4,5,6) # 1 is novel
#med_olap_test = c(1,2,3,4,5,6,7) 
#low_olap_test = c(1,2,3,4,5,6,7,8) 

# Exp 2 uses high_olap again, and then either adds as 3rd trial either: 
# 1st trial agaain (familiar context) or a novel context
fam_con = rbind(high_olap, high_olap[1,]) 
nov_con = rbind(high_olap, c(5,7,8,9)) # 7,8,9 all novel, 5 is a backward pair (from trial 1)

get_model_prediction_exp1 <- function(par, cond='all') {
  mh = model(par, ord=high_olap) 
  mmm = mh$matrix
  high_repeated = mean(rowSums(mmm[2:4,2:4]) / rowSums(mmm[2:4,]))
  hp = as.vector(mh$perf)
  #high_olap_type = c("Novel", "Repeated","Repeated","Repeated", "Backward", "Forward")
  #mh_perf = data.frame(item=1:length(hp), type=high_olap_type, hp)
  mod_hi = data.frame(overlap=rep("High",3), type=c("Repeated","Backward","Forward"), perf=c(high_repeated, hp[5], hp[6]))
  # sum or mean? or do we need to go back to the matrix...
  
  mm = model(par, ord=med_olap) # reps and test_noise
  mmm = mm$matrix
  mp = as.vector(mm$perf)
  med_repeated = mean(rowSums(mmm[2:3,2:3]) / rowSums(mmm[2:3,]))
  med_backward = mean(rowSums(mmm[4:5,4:5]) / rowSums(mmm[4:5,]))
  med_forward = mean(rowSums(mmm[6:7,6:7]) / rowSums(mmm[6:7,]))
  #med_olap_type = c("Novel", "Repeated","Repeated", "Backward","Backward", "Forward","Forward")
  #mm_perf = data.frame(item=1:length(mp), type=med_olap_type, mp)
  #mod_m = data.frame(overlap=rep("Medium",3), type=c("Repeated","Backward","Forward"), perf=c(mean(mp[2:3]), mean(mp[4:5]), mean(mp[6:7])))
  mod_m = data.frame(overlap=rep("Medium",3), type=c("Repeated","Backward","Forward"), perf=c(med_repeated, med_backward, med_forward))
  
  ml = model(par, ord=low_olap, test_noise=.01)
  mmm = ml$matrix
  lp = as.vector(ml$perf)
  low_backward = mean(rowSums(mmm[3:5,3:5]) / rowSums(mmm[3:5,]))
  low_forward = mean(rowSums(mmm[6:8,6:8]) / rowSums(mmm[6:8,]))
  #low_olap_type = c("Novel", "Repeated", "Backward","Backward","Backward", "Forward","Forward","Forward")
  #ml_perf = data.frame(item=1:length(lp), type=low_olap_type, lp)
  mod_lo = data.frame(overlap=rep("Low",3), type=c("Repeated","Backward","Forward"), perf=c(lp[2], low_backward, low_forward))
  
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



plot_miniXSL <- function(modeldat, humandat, fname, sse) {
  modeldat$Model = modeldat$perf
  modeldat$perf = NULL
  alldat = merge(modeldat, humandat, by=c("type","overlap"))
  g <- ggplot() + geom_bar(data=alldat, aes(x=type, y=Model, fill=overlap, ymin=0.0, ymax=1.0), stat="identity", position=position_dodge(width=1.0)) + 
    geom_point(data=alldat, aes(x=type, y=perf, fill=overlap, ymin=0.0, ymax=1.0), position=position_dodge(width=1.0)) + 
    xlab("Item Type") + ylab("Proportion Correct") + theme_bw() + labs(title=paste("SSE =",round(sse, 3)))
  ggsave(fname, width=5, height=5)
}


plot_miniXSL_Exp2 <- function(modeldat, humandat, fname, sse) {
  modeldat$Model = modeldat$perf
  modeldat$perf = NULL
  alldat = merge(modeldat, humandat, by=c("type","condition"))
  g <- ggplot() + geom_bar(data=alldat, aes(x=type, y=Model, fill=condition, ymin=0.0, ymax=1.0), stat="identity", position=position_dodge(width=1.0)) + 
    geom_point(data=alldat, aes(x=type, y=perf, fill=condition, ymin=0.0, ymax=1.0), position=position_dodge(width=1.0)) + 
    xlab("Item Type") + ylab("Proportion Correct") + theme_bw() + labs(title=paste("SSE =",round(sse, 3)))
  ggsave(fname, width=5, height=5)
}


get_model_prediction_exp2 <- function(par, cond='all') {
  mh = model(par, ord=high_olap, test_noise=.01) # reps and test_noise
  hp = as.vector(mh$perf)
  mmm = mh$matrix
  high_repeated = mean(rowSums(mmm[2:4,2:4]) / rowSums(mmm[2:4,])) # for each word, need sum of prob of choosing either possible ref
  mod_hi = data.frame(condition=rep("High Overlap",3), type=c("Repeated","Backward","Forward"), perf=c(sum(hp[2:4]), hp[5], hp[6]))
  
  mm = model(par, ord=fam_con, test_noise=.01) # familiar context
  mp = as.vector(mm$perf)
  mmm = mm$matrix
  med_repeated = mean(rowSums(mmm[2:4,2:4]) / rowSums(mmm[2:4,])) 
  mod_m = data.frame(condition=rep("Familiar Context",3), type=c("Repeated","Backward","Forward"), perf=c(med_repeated, mp[5], mp[6])) 
  
  ml = model(par, ord=nov_con, test_noise=.01)
  mmm = ml$matrix
  lp = as.vector(ml$perf)
  low_repeated = mean(rowSums(mmm[2:4,2:4]) / rowSums(mmm[2:4,]))
  low_novel = mean(rowSums(mmm[7:9,7:9]) / rowSums(mmm[7:9,]))
  mod_lo = data.frame(condition=rep("Novel Context",4), type=c("Repeated","Backward","Forward", "Novel"), perf=c(low_repeated, lp[5], lp[6], low_novel))
  
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


hier_fit_2fixed_kachergis_model_exp1 <- function(modelname, lower, upper) {
  source(paste(modelname,".R",sep=''))
  bestSSE = 100
  best = NA
  seeds = c(1234, 5678, 928798, 1532, 29873, 983701, 10983, 447278916, 18783, 1984)
  for(s in seeds) {
    set.seed(s)
    tmp = DEoptim(fn=fit_exp1_fixed_learn_decision_parm, lower=lower, upper=upper, 
                  DEoptim.control(reltol=.0001, steptol=90, itermax=300, trace=20)) 
    print(paste("SSE:",tmp$optim$bestval,"parms:"))
    if(tmp$optim$bestval<bestSSE) {
      bestSSE = tmp$optim$bestval
      best = tmp
    }
  }
  bestp = best$optim$bestmem
  print(bestp)
  chi = bestp[7]
  tao = bestp[8]
  exp1mod_lo = get_model_prediction_exp1(c(chi,bestp[1:2],tao))
  exp1mod_med = get_model_prediction_exp1(c(chi,bestp[3:4],tao))
  exp1mod_hi = get_model_prediction_exp1(c(chi,bestp[5:6],tao))
  
  exp1mod = rbind(subset(exp1mod_lo, overlap=="Low"), 
                  subset(exp1mod_med, overlap=="Medium"), 
                  subset(exp1mod_hi, overlap=="High"))
  plot_miniXSL(exp1mod, exp1hum, paste("miniXSL_Exp1_",modelname,"_hier_2fixed_fit.pdf",sep=''), best$optim$bestval)
  return(best$optim)
}


hier_fit_2fixed_kachergis_model_exp2 <- function(modelname, lower, upper) {
  source(paste(modelname,".R",sep=''))
  bestSSE = 100
  best = NA
  seeds = c(1234, 5678, 928798, 1532, 29873, 983701, 10983, 447278916, 18783, 1984)
  for(s in seeds) {
    set.seed(s)
    tmp = DEoptim(fn=fit_exp2_fixed_learn_decision_parm, lower=lower, upper=upper, 
                  DEoptim.control(reltol=.0001, steptol=90, itermax=300, trace=20)) # , trace=10
    print(paste("SSE:",tmp$optim$bestval,"parms:"))
    if(tmp$optim$bestval<bestSSE) {
      bestSSE = tmp$optim$bestval
      best = tmp
    }
  }
  
  bestp = best$optim$bestmem
  print(bestp)
  chi = bestp[7]
  tao = bestp[8]
  exp2mod_fam = get_model_prediction_exp2(c(chi,bestp[3:4],tao))
  exp2mod_nov = get_model_prediction_exp2(c(chi,bestp[5:6],tao))
  exp2mod_hi = get_model_prediction_exp2(c(chi,bestp[1:2],tao))
  
  exp2mod = rbind(subset(exp2mod_fam, condition=="Familiar Context"), 
                  subset(exp2mod_hi, condition=="High Overlap"),
                  subset(exp2mod_nov, condition=="Novel Context"))
  plot_miniXSL_Exp2(exp2mod, exp2hum, paste("miniXSL_Exp2_",modelname,"_hier_2fixed_fit.pdf",sep=''), best$optim$bestval)
  
  return(best$optim)
}


source("kachergis_exp.R")

# 2 fixed parameters (learning rate and decision parm)
hier_fit_2fixed_kachergis_model_exp1("kachergis_exp", lower=c(rep(c(.01,.5),3), .00001, .05), upper=c(rep(c(20,1),3), 15, 1000))
#  SSE=0.025 with regularization, .021 without 
fit_exp1_fixed_learn_decision_parm(c(2.418412, 1.0, 
                                     4.142746, 1.0, 
                                     4.327943, 0.50,  15.00, 1.268072), regularize=F) 

hier_fit_2fixed_kachergis_model_exp2("kachergis_exp", lower=c(rep(c(.01,.5),3), .001, .05), upper=c(rep(c(12,1),3), 15, 1000))
# SSE=.119 with regularization, .093 without
fit_exp2_fixed_learn_decision_parm(c(6.627942, 1.0, 
                                     6.011371, 0.50, 
                                     1.951481, 0.8057, 15.00,  1.272883), regularize=F) 

