hall <- function(x, order = 3){
  n=length(x)
  d1 = c(0.5^0.5,-0.5^0.5)
  d2 = c(0.8090, -0.5,-0.3090) 
  d3 = c(0.1942, 0.2809, 0.3832, -0.8582)
  d4 = c(0.2708, -0.0142, 0.6909, -0.4858, -0.4617)
  d5 = c(0.9064, -0.2600, -0.2167, -0.1774, -0.1420, -0.1103)
  d = get(paste("d",order,sep=""))
  s=0
  for (k in 1:(n-order)){
    s = s + sum((d*x[k:(k+order)]))^2
  }
  return(sqrt(s/(n-order)))
}

Q<-function(x, cps, changetype){
  idu = c(length(x), cps)
  idl = c(cps+1, 1)
  Q=0
  for (i in 1:(length(cps)+1)){
    Q=Q+cost_rcpp(x[idu[i]:idl[i]], changetype)
  }
  return(Q)
}

CROPS <- function(x, beta, changetype){
  n=length(beta)-1
  stat=rep(1,n)
  stat=c(stat, 0)
  cps=rep(0, n+1)
  while (sum(stat)>0){
    n=length(beta)-1
    for (i in n:1){
      if (stat[i]==1){
        beta1 = beta[i]
        beta2 = beta[i+1]
        cp1 = PELT_rcpp(x, beta1, changetype)
        cp2 = PELT_rcpp(x, beta2, changetype)
        cps[i] = n1 = length(cp1$cps)
        cps[i+1] = n2 = length(cp2$cps)
        if (n1 > (n2+1)){
          beta_int = (cp1$Q-cp2$Q-n1*beta1+n2*beta2)/(n2-n1)
          cp_int = PELT_rcpp(x, beta_int, changetype)$cps
          n_int = length(cp_int)
          if ((beta_int == beta2)|(beta_int == beta1)){
            stat[i] = 0
          }
          else{
            beta = c(beta[1:i], beta_int, beta[(i+1):length(beta)])
            cps = c(cps[1:i], n_int, cps[(i+1):length(cps)])
            if (n_int!=n2){
              stat = c(stat[1:i], 1, stat[(i+1):length(stat)])
            }
            else {stat = c(stat[1:i], 0, stat[(i+1):length(stat)])}
          }
        }
        else {stat[i]=0}
      }
    }
  }
  return(list(beta=beta,cps=cps))
}