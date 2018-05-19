#include <Rcpp.h>
#include <cmath>        // std::abs

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector knn_dist(NumericVector y, int k){
  int n = y.size();
  
  NumericVector x = clone(y);

  for(int i = 0; i < n; ++i){
    x[i] = std::abs(y[i] - y[k]);
  }

  std::sort(x.begin(), x.end());	// sort x in ascending order
  
  return x;
}
