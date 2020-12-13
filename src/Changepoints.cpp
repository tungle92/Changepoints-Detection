#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object

#include<vector> //to use std::vector<double>

// [[Rcpp::export]]
double cost_rcpp(NumericVector x){
  double m = mean(x);
  float c = 0;
  for(int i=0; i<x.size(); ++i){
    c += pow(x[i]-m,2);
  }
  return c;
}

IntegerVector optimal_partitioning_rcpp(NumericVector x, double beta){
  int n = x.size();
  IntegerVector cp (n);
  NumericVector Fcost (n+1, -beta);
  for (int i = 1; i <= n; i++){
    NumericVector Fcompare (i);
    double min = std::numeric_limits<double>::infinity();
    for (int j = 0; j < i; j++){
      Fcompare[j] = Fcost[j] + cost_rcpp(x[Range(j, i-1)]) + beta;
      if (Fcompare[j] <= min) {
        min = Fcompare[j];
        cp[i-1] = j ;
      }
      Fcost[i] = min;
    }
  }
  IntegerVector cps;
  while (cp[n-1]>0){
    cps.push_back(cp[n-1]);
    n = cp[n-1];
  }
  return(cps);
}
  


