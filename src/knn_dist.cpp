#include <Rcpp.h>
#include <cmath>        // std::abs
#include <algorithm>    // std::execution
#ifdef _OPENMP
  #include <omp.h>
#endif

using namespace Rcpp;
// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export(.kVector)]]
std::vector<double> kVector(std::vector<double> w, int knn, int nthreads=1){
  int n = w.size();
  std::vector<double> z(n);
  
#pragma omp parallel for num_threads(nthreads) 
  for (int i = 0; i < n; i++){
    //----- window start---------------------------------------------------//
    int winStart = 0;                       
    if (i > knn)  
    {
      winStart = i - knn;    
    }
    //----- window end---------------------------------------------------//
    int winEnd = i + knn;
    if (winEnd > w.size())
    {
      winEnd = w.size();   // fixes the window as it approaches the right
    }
    
    //- x holds distance calculation ------------------------------------//
    std::vector<double> x(winEnd-winStart+1);   // creates a vector the size of the window   
    
    // for loop calculates lengths from a fixed position
    for(int j = 0; j < winEnd-winStart+1; ++j){ 
      x[j] = std::fabs(w[j+winStart] - w[i]);
    }
    std::sort(x.begin(), x.end());	// sort x in ascending order
    z[i] = x[knn]; // returns the distance of kth nearest neighbor
  }
  
  return(z);
}


// [[Rcpp::export(kVector)]]
std::vector<double> kVector2(std::vector<double> w, int knn, int nthreads=1){
  int n = w.size();
  std::vector<double> z(n);
  
#pragma omp parallel for num_threads(nthreads) 
  for (int i = 0; i < n; i++){
    //----- window start---------------------------------------------------//
    int winStart = 0;                       
    if (i > knn)  
    {
      winStart = i - knn;    
    }
    //----- window end---------------------------------------------------//
    int winEnd = i + knn;
    if (winEnd > w.size())
    {
      winEnd = w.size();   // fixes the window as it approaches the right
    }
    
    //- x holds distance calculation ------------------------------------//
    std::vector<double> x(winEnd-winStart+1);   // creates a vector the size of the window   
    
    // for loop calculates lengths from a fixed position
    for(int j = 0; j < winEnd-winStart+1; ++j){ 
      x[j] = std::fabs(w[j+winStart] - w[i]);
    }
    std::sort(x.begin(), x.end());	// sort x in ascending order
    z[i] = x[knn]; // returns the distance of kth nearest neighbor
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

