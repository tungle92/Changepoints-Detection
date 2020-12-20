
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