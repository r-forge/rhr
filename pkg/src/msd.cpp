#include <Rcpp.h>
#include <cmath>

RcppExport SEXP msdcpp(SEXP xx, SEXP yy, SEXP mmx, SEXP mmy) {
BEGIN_RCPP
  Rcpp::NumericVector x(xx);
  Rcpp::NumericVector y(yy);
  Rcpp::NumericVector mx(mmx);
  Rcpp::NumericVector my(mmy);

  int n = x.size();
  double sumx = 0;
  double sumy = 0;
  double tmp_ri = 0;  // What is tmp_ri?
  int i;  // counting variable used within the loop

  for (i = 0; i < n; i++) {
    double tx = x[i] - mx[0];
    double ty = y[i] - my[0];
    tmp_ri += pow(tx, 2) + pow(ty, 2);
  }
  
  return Rcpp::NumericVector::create(1/(double)(n-1) * tmp_ri);
END_RCPP
}
