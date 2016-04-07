
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

