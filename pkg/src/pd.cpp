#include <Rcpp.h>
#include <cmath>

RcppExport SEXP pdcpp(SEXP pprob) {
BEGIN_RCPP
  Rcpp::NumericVector prob(pprob);

  int n = prob.size();
  Rcpp::NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    int sum = 0;
    for (int j = 0; j < n; ++j) {
      if ( prob[j] >= prob[i]) {
        sum += 1;
      }
    }
    out[i] = (double)sum/n;
  }
  return out;
END_RCPP
}
