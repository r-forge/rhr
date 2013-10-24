#include <Rcpp.h>
#include <cmath>

// Binned LSCV for kernel bandwidth estimation
RcppExport SEXP binnedCV(SEXP xxs, SEXP yys, SEXP ffreq, SEXP hhs) {
  BEGIN_RCPP

    // Input
    Rcpp::NumericVector xs(xxs);
    Rcpp::NumericVector ys(yys);
    Rcpp::NumericVector freq(ffreq);
    Rcpp::NumericVector hs(hhs);

  int n1 = ys.size();
  int n2 = (n1 * (n1 - 1)) / 2 + n1;
  int at = 0;

  int nh = hs.size();

  Rcpp::NumericVector outh(nh);
  Rcpp::NumericVector dists(n2);
  Rcpp::NumericVector fdists(n2);

  // total number of points
  int n3 = 0;
  for (int i = 0; i < n1; ++i) {
    n3 += freq[i];
  }
		
  // Calculate distances and weights, only once
  for(int i = 0; i < n1; ++i) {
    for (int j = 0; j < n1; ++j) {
      if (i <= j) {
	dists[at] = sqrt(pow(xs[i] - xs[j], 2.0) + pow(ys[i] - ys[j], 2.0));

	if (i != j) {
	  fdists[at] = (2 * freq[i] * freq[j]);
	} else {
	  fdists[at] = freq[i] * freq[j];
	}
	at++;
      }
    }
  }
	

  // Cycle over all h's
  for (int i = 0; i < nh; ++i) {

    double out = 0.0;
    double h = hs[i];

    for(int j = 0; j < n2; ++j) {
      double d = dists[j];
      double ft = exp(-pow(d, 2.0) / (4.0 * pow(h, 2.0)));
      double st = 4.0 * exp(-pow(d, 2.0)/(2.0 * pow(h, 2.0)));
      out += ((ft - st) * fdists[j]);
    	
    }

    double ft = 1.0 / (3.14159265359 * pow(h, 2.0) * n3);
    double st = 1.0 / (3.14159265359 * 4.0 * pow(h, 2.0) * pow(n3, 2.0));
    outh[i] = ft + st * out;
  }
  return (outh);
END_RCPP
}
