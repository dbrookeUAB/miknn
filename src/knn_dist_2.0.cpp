#include <Rcpp.h>
#include <cmath>        // std::abs

using namespace Rcpp;

// [[Rcpp::export]]
double knn_dist_2(NumericVector y, int knn){
  int n = y.size();
  
  NumericVector x = clone(y);

  for(int i = 0; i < n; ++i){
    x[i] = std::abs(y[i] - y[knn]);
  }

  std::sort(x.begin(), x.end());	// sort x in ascending order
  
  return x[knn];
}
