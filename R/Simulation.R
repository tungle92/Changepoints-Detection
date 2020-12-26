
# Time complexity simulation for change in mean:
mean.simu <- function(n, m, type = "sample", func = "OP", beta = 1)
{
  changetype = 'mean'
  if(type == "sample"){v1 <- sample(m+1)}else{v <- (m+1):1}
  a = floor((n-30)/60)
  v = sample(1:a, m, replace = FALSE) - runif(m)/2
  cps = sort(round(v*60, 0))
  ngroup = diff(c(0,cps,n))
  x=unlist(lapply(ngroup, function(x) rnorm(x,0,1)))/4+rep(v1,ngroup)
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
  t <- system.time(f(x, beta, changetype='meanvar'))[[1]]
  return(t)
}
