#include <Rcpp.h>
#include <cmath>        // std::abs

using namespace Rcpp;

// [[Rcpp::export(.knn_dist)]]
double knn_dist(NumericVector y, int knn, int position){
  int n = y.size();
  position = position -1;
  
  NumericVector x(n);
  
  for(int i = 0; i < n; ++i){
    x[i] = std::abs(y[i] - y[position]);
  }
  
  std::sort(x.begin(), x.end());	// sort x in ascending order
  
  return x[knn];
}

// [[Rcpp::export(.kDistC)]]
double kDistC(NumericVector y, int knn, int position){
  int n = y.size();
  // window size
  int winStart = 0;                     
  if (position > knn)
  {
    winStart = position - knn;
  }
  
  int winEnd = position + knn;
  if (winEnd > n)
  {
    winEnd = n;
  }
  
  NumericVector x(winEnd-winStart+1);     // holds distance calculation
  for(int i = 0; i < winEnd-winStart+1; ++i){
    x[i] = std::abs(y[i+winStart] - y[position]);
  }
  
  std::sort(x.begin(), x.end());	// sort x in ascending order
  
  return x[knn];
}


// [[Rcpp::export(.kVector)]]
NumericVector kVector(NumericVector w, int knn){
  int n = w.size();
  NumericVector z(n);
  
  for (int i = 0; i < n; i++){
    z[i] = kDistC(w, knn, i);
  }
  return(z);
}


// [[Rcpp::export(.neighbors)]]
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

