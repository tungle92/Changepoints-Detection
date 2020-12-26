
# Time complexity simulation for change in mean:
mean.simu <- function(n, type = "sample", func = "OP", beta = 1, changetype = 'mean')
{
  m = sample(n/10)
  if(type == "sample"){v <- sample(m)}else{v <- m:1}
  w = sample(m)
  x=rep(v,w*n/sum(w))+runif(length(rep(v,w*n/sum(w))))
  f=get(func)
  t <- system.time(f(x, beta, changetype='mean'))[[1]]
  return(t)
}
mean.simu(100, func = 'OP', beta =1)


# Time complexity simulation for change in both mean and variance:
var.simu <- function(n, type = "sample", func = "OP", beta = 1, changetype = 'mean')
{
  m = sample(n/10)
  if(type == "sample"){v <- sample(m)}else{v <- m:1}
  w = sample(m)
  x=rep(v,w*n/sum(w))+runif(length(rep(v,w*n/sum(w))))
  f=get(func)
  t <- system.time(f(x, beta, changetype='mean'))[[1]]
  return(t)
}
var.simu(100, func = 'OP', beta =1)