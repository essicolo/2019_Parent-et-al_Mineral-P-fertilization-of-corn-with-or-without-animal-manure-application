ilrNA = function(comp, sbp, bal) {
  # comp: la composition
  # sbp: sequential binary partition
  # bal: ilr
  
  if(class(comp) == 'acomp') {
    comp = unclass(comp)
  }
  if(class(bal) == 'rmult') {
    bal = unclass(bal)
  }
  bal = unclass(bal)
  for (n in 1:nrow(comp)){
    for (p in 1:ncol(comp)) {
      for (q in 1:ncol(bal)) {
        if (sbp[q,p] != 0 & is.na(comp[n,p])) bal[n,q] <- NA
      }
    }
  }
  return(bal)
}