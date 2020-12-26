
# Time complexity simulation for change in mean:
one.simu <- function(n, type = "sample", func = "OP", beta = 1)
{
  changetype = 'mean'
  m = sample(n/10)
  if(type == "sample"){v <- sample(m)}else{v <- m:1}
  w = sample(m)
  x=rep(v,w*n/sum(w))+runif(length(rep(v,w*n/sum(w))))
  f=get(func)
  t <- system.time(f(x, beta, changetype='mean'))[[1]]
  return(t)
}


# Time complexity simulation for change in both mean and variance:
var.simu <- function(n, m, func = "OP", beta = 1)
{
  changetype = 'meanvar'
  a = floor((n-30)/60)
  v = sample(1:a, m, replace = FALSE) - runif(m)/2
  cps = sort(round(v*60, 0))
  ngroup = diff(c(0,cps,n))
  x=unlist(lapply(ngroup, function(x) rnorm(x,0,rlnorm(1,0,log(10)/2))))
  f=get(func)
  t <- system.time(f(x, beta, changetype='mean'))[[1]]
  return(t)
}
