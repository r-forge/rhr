#include <Rcpp.h>
#include <cmath>

RcppExport SEXP randomWalkcpp(SEXP ssx, SEXP ssy, SEXP ssinA, SEXP ccosA, SEXP dd, SEXP rrx, SEXP rry) {
BEGIN_RCPP
  Rcpp::NumericVector sx(ssx);
  Rcpp::NumericVector sy(ssy);
  Rcpp::NumericVector sinA(ssinA);
  Rcpp::NumericVector cosA(ccosA);
  Rcpp::NumericVector d(dd);
  Rcpp::NumericVector rx(rrx);
  Rcpp::NumericVector ry(rry);

  int n = d.size();
  int i;  // counting variable used within the loop

  rx[0] = sx[0];
  ry[0] = sy[0];

  for (i = 1; i < n; i++) {

    rx[i] = rx[i-1] + cosA[i-1] * d[i];
    ry[i] = ry[i-1] + sinA[i-1] * d[i];
   
  }
  
  return Rcpp::List::create(Rcpp::Named("rx") = rx,
		     Rcpp::Named("ry") = ry);
END_RCPP
}
