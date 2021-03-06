# Associative Uncertainty- (Entropy) & Familiarity-Biased Model
# George Kachergis  george.kachergis@gmail.com

shannon.entropy <- function(p) {
	if (min(p) < 0 || sum(p) <= 0)
		return(NA)
	p.norm <- p[p>0]/sum(p)
	-sum(log2(p.norm)*p.norm)
	}

update_known <- function(m, tr_w, tr_o) {
  startval = .01
  
  for(i in tr_w) {
    for(c in 1:dim(m)[2]) {
      if(sum(m[,c]>0) & m[i,c]==0) {
        m[i,c] = startval
        if(c<nrow(m)) m[c,i] = startval
      }
    }
    for(j in tr_o) {
      if(m[i,j]==0) m[i,j] = startval
      if(j<nrow(m) && m[j,i]==0) m[j,i] = startval
    }
  }
  return(m)
}


model <- function(params, ord=c(), reps=1, test_noise=0) {
	X <- params[1] # associative weight to distribute
	B <- params[2] # weighting of uncertainty vs. familiarity
	C <- params[3] # decay
	tao = params[4] # softmax parameter
	inds = unique(as.vector(unlist(ord)))
	voc_sz = max(unlist(ord), na.rm=TRUE) # vocabulary size
	ref_sz = max(unlist(ord), na.rm=TRUE) # number of objects
	traj = list()
	m <- matrix(0, voc_sz, ref_sz) # association matrix
	perf = matrix(0, reps, voc_sz) # a row for each block
	# training
	for(rep in 1:reps) { # for trajectory experiments, train multiple times
	  for(t in 1:nrow(ord)) { 
		#print(format(m, digits=3))
		
		tr_w = ord[t,]
		tr_o = ord[t,]
		m = update_known(m, tr_w, tr_o) # what's been seen so far?
		ent_w = c() # more entropy = more dispersive
		for(w in tr_w) { ent_w = c(ent_w, shannon.entropy(m[w,])) }
		ent_w = exp(B*ent_w)
		
		ent_o = c() # more entropy = more dispersive
		for(o in tr_o) { ent_o = c(ent_o, shannon.entropy(m[,o])) }
		ent_o = exp(B*ent_o)
		
		nent = (ent_w %*% t(ent_o))
		#nent = nent / sum(nent)
		# get all current w,o strengths and normalize to distr X
		assocs = m[tr_w,tr_o]
		denom = sum(assocs * nent)
		m = m*C # decay everything
		# update associations on this trial
		m[tr_w,tr_o] = m[tr_w,tr_o] + (X * assocs * (ent_w %*% t(ent_o))) / denom 

		index = (rep-1)*nrow(ord) + t # index for learning trajectory
		traj[[index]] = m
	  }
	  m_test = m+test_noise # test noise constant k
	  perf[rep,] = diag(m_test) / rowSums(m_test)
	  #perf[rep,] = exp(diag(m_test)/tao) / exp(rowSums(m_test)/tao)
	}
	want = list(perf=perf, matrix=exp(m/tao), traj=traj)
	return(want)
	}

