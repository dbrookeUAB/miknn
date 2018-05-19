#include <Rcpp.h>
#include <cmath>        // std::abs

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector neighbors(NumericVector y, NumericVector x){
  
  int n = y.size();
  
  
  NumericVector z = clone(y);
  
  for(int j = 0; j < n; ++j){
    int total = 0;
    double low = y[j]-x[j];
    double high = y[j]+x[j];
    
    for(int i = 0; i < n; ++i){
      if( y[i] <= high && y[i] >=low ) {
        total += 1;
        } 
      }
    z[j] = total -1;
    }
  return z;
  
  
}
