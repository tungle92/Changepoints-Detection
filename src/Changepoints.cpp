#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object

#include<vector> //to use std::vector<double>

// [[Rcpp::export]]
double cost_rcpp(NumericVector x){
  double sum = 0;
  for(int i=0; i<x.length(); ++i){
    sum += x[i];
  }
  return sum;
}
