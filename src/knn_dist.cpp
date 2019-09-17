#include <Rcpp.h>
#include <cmath>        // std::abs

using namespace Rcpp;


// [[Rcpp::export(.kDistC)]]
double kDistC(NumericVector y, int knn, int position){
  int n = y.size();                 //  size of the vector
  
  //----- window start---------------------------------------------------//
  int winStart = 0;                       
  if (position > knn)  
  {
    winStart = position - knn;    
  }
  //----- window end---------------------------------------------------//
  int winEnd = position + knn;
  if (winEnd > n)
  {
    winEnd = n;   // fixes the window as it approaches the right
  }
  
  //- x holds distance calculation ------------------------------------//
  std::vector<double> x(winEnd-winStart+1);   // creates a vector the size of the window   
  // for loop calculates lengths from a fixed position
  for(int i = 0; i < winEnd-winStart+1; ++i){ 
    x[i] = std::fabs(y[i+winStart] - y[position]);
  }
  std::sort(x.begin(), x.end());	// sort x in ascending order
  return x[knn]; // returns the distance of kth nearest neighbor
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
    int i = 1;
    while(y[j-i] >=low && y[j+i]<=high && j-i>=0 && j+i <n) {
      ++i;
      total = total + 2;
    }
    int down = i;
    while(y[j-down] >=low && j-down>=0) {
      ++total;
      ++down;
    }
    int up = i;
    while(y[j+up] <=high && j+up<n) {
      ++total;
      ++up;
    }
    z[j] = total;
  }
  return z;
}

